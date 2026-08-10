#include "cimport.h"
#include "common.h"
#include "fmt/base.h"
#include "fmt/format.h"
#include "llvmcomp.h"
#include "types.h"
#include "value.h"
#include <cctype>
#include <charconv>
#include <filesystem>
#include <optional>
#include <queue>
#include <simdjson.h>
#include <string_view>
#include <unordered_map>
#include <vector>

using namespace simdjson;
namespace fs = std::filesystem;

static TypeCache cTypes = {
  {"uint8_t",     Pool()._u8   },
  {"uint16_t",    Pool()._u16  },
  {"uint32_t",    Pool()._u32  },
  {"uint64_t",    Pool()._u64  },
  {"int8_t",      Pool()._s8   },
  {"int16_t",     Pool()._s16  },
  {"int32_t",     Pool()._s32  },
  {"int64_t",     Pool()._s64  },
  {"__uint64_t",  Pool()._u64  },
  {"__uint128_t", Pool()._u128 },
  {"int",         Pool()._s32  },
  {"char",        Pool()._u8   },
  {"size_t",      Pool()._usize},
  {"void",        Pool()._void },
  {"intptr_t",    Pool()._usize},
  {"uintptr_t",   Pool()._usize},
  {"bool",        Pool()._bool },
  {"char",        Pool()._u8   },
  {"float",       Pool()._f32  },
  {"double",      Pool()._f64  },
  // TODO: vector types
  {"__m128",      Pool()._void },
};

TypeIndex parseType(
  std::string_view qualType,
  std::queue<std::string>& globals
) {
  Logger log(LogLevel::CImport);
  // TODO(mut)
  if (qualType.starts_with("const ")) {
    qualType = qualType.substr(6);
  }

  if (cTypes.contains(qualType)) {
    return cTypes[qualType];
  }

  for (string_view recordPrefix : {"struct ", "union ", "enum "}) {
    if (qualType.starts_with(recordPrefix)) {
      auto unqualified = qualType.substr(recordPrefix.size());
      if (cTypes.contains(unqualified)) {
        return cTypes[unqualified];
      }
    }
  }

  qualType = StringPool::inst().copy(qualType);

  // Tokenize base type
  u32 endIndex = 0;
  while (endIndex < qualType.size()) {
    auto c = qualType[endIndex];
    if (!(isalnum(c) || c == '_')) {
      break;
    }
    endIndex++;
  }
  auto baseTypeString = qualType.substr(0, endIndex);
  if (!cTypes.contains(baseTypeString)) {
    crash("Undefined C type: {} in type {}", baseTypeString, qualType);
  }
  log("base type: {}", baseTypeString);
  TypeIndex type = cTypes[baseTypeString];

  auto modifiers = qualType.substr(endIndex);
  while (true) {
    if (modifiers.starts_with(" *")) {
      modifiers = modifiers.substr(2);
      type = Pool().pointerTo(type);
    } else if (modifiers.starts_with('[')) {
      modifiers = modifiers.substr(1);
      auto closingBracketIndex = modifiers.find(']');
      auto lengthString = modifiers.substr(0, closingBracketIndex);
      u32 arrayLength;
      std::from_chars(
        lengthString.data(),
        lengthString.data() + lengthString.size(),
        arrayLength
      );

      type = Pool().sizedArrayOf(type, arrayLength);
      modifiers = modifiers.substr(closingBracketIndex + 1);
    } else if (modifiers.starts_with(' ')) {
      modifiers = modifiers.substr(1);
    } else {
      break;
    }
  }

  if (modifiers.starts_with("(*)")) {
    modifiers = modifiers.substr(3);
  }

  // TODO: does this work for nested function pointers?
  if (modifiers.starts_with('(')) {
    if (modifiers == "(void)") {
      std::vector<TypeIndex> emptyTuple;
      auto [_, paramTypes] = Pool().tupleOf(std::move(emptyTuple));
      type = Pool().addFunction(
        FunctionType{.parameters = paramTypes, .returnType = type}
      );
    } else {
      modifiers = modifiers.substr(1);
      std::vector<TypeIndex> paramTypes;
      while (true) {
        auto commaIndex = modifiers.find(", ");
        if (commaIndex != std::string::npos) {
          auto subType = modifiers.substr(0, commaIndex);
          paramTypes.push_back(parseType(subType, globals));
          modifiers = modifiers.substr(commaIndex + 2);
          continue;
        }

        auto parenIndex = modifiers.find(")");
        if (parenIndex == std::string::npos) {
          crash("Malformed C type '{}'", modifiers);
        }

        auto paramType = modifiers.substr(0, parenIndex);
        paramTypes.push_back(parseType(paramType, globals));
        break;
      }

      auto [_, paramTuple] = Pool().tupleOf(paramTypes);
      type = Pool().addFunction(
        FunctionType{.parameters = paramTuple, .returnType = type}
      );
    }
  }

  cTypes[qualType] = type;
  return type;
}

u32 longestPrefixEndingIn(std::span<std::string_view> strings, char lastChar) {
  u32 prevLength = 0;
  while (true) {
    auto start = prevLength + 1;
    auto i = start;
    for (; i < strings[0].length(); i++) {
      if (strings[0][i] == lastChar) break;
    }

    if (i == prevLength + 1) break;

    for (auto string : strings) {
      if (i >= string.length()) return prevLength;
      if (
        string.substr(start, i - prevLength) !=
        strings[0].substr(start, i - prevLength)
      )
        return prevLength;
    }
    prevLength = i;
  }

  return prevLength;
}

TypeIndex parseRecord(
  ondemand::value& node,
  Identifier cName,
  Identifier unprefixedName,
  std::queue<std::string>& globals,
  const TypeEmitter& emitType
) {
  Logger log(LogLevel::CImport);
  std::string_view tagUsed;
  node["tagUsed"].get(tagUsed);
  TypeIndex resultTypeIndex;
  if (tagUsed == "struct") {
    auto [typeIndex, structIndex] = Pool().makeStruct(
      unprefixedName,
      cName.empty() ? RegisterName(Environment::structIndex())
                    : RegisterName(cName)
    );

    ondemand::array structFields;
    if (node["inner"].get_array().get(structFields)) {
      crash("Error parsing fields for C struct '{}'", unprefixedName);
    }

    OptionalType anonType = TypeIndex::null();
    u32 anonymousFieldIndex = 0;
    for (auto structField : structFields) {
      std::string_view fieldKind;
      structField["kind"].get(fieldKind);
      if (fieldKind == "RecordDecl") {
        auto anonymousName = copyStr(
          "{}.anon.{}",
          cName.empty() ? string_view("c") : cName,
          anonymousFieldIndex
        );
        anonType = parseRecord(
          structField.value(),
          anonymousName,
          "",
          globals,
          emitType
        );
      } else if (fieldKind == "FieldDecl") {
        std::string_view fieldName;
        bool hasName = structField["name"].get(fieldName) == SUCCESS;

        TypeIndex fieldType;
        if (anonType) {
          fieldType = anonType;
          anonType = TypeIndex::null();
        } else {
          std::string_view fieldTypeName;
          structField["type"]["qualType"].get(fieldTypeName);
          fieldType = parseType(fieldTypeName, globals);
        }

        if (!hasName) {
          fieldName = copyStr(
            "{}{}",
            TypePool::anonymousFieldPrefix,
            anonymousFieldIndex++
          );
        } else {
          fieldName = StringPool::inst().copy(fieldName);
        }
        Pool().getStruct(structIndex).defineField(fieldName, fieldType);
      } else {
        log("Skipping inner node for struct of kind {}", fieldKind);
      }
    }

    log("Struct fields for {}", cName);
    for (auto [fieldName, type] : Pool().getStruct(structIndex).fields) {
      log("Fieldname: {}", fieldName);
      log("{}: {}", fieldName, TypeName(type));
    }

    if (emitType) {
      emitType(typeIndex);
    } else {
      Pool().defineLLVMStruct(structIndex, globals);
    }
    resultTypeIndex = typeIndex;
  } else if (tagUsed == "union") {
    std::vector<TypeIndex> anonymousVariants;
    std::vector<pair<TypeIndex, Identifier>> namedVariants;
    ondemand::array variants;
    if (node["inner"].get_array().get(variants)) {
      crash("Error parsing variants for C union '{}'", cName);
    }

    OptionalType anonType = TypeIndex::null();
    for (ondemand::value variant : variants) {
      std::string_view variantKind;
      variant["kind"].get(variantKind);
      if (variantKind == "RecordDecl") {
        auto anonymousName = copyStr(
          "{}.anon.{}",
          cName.empty() ? "c" : cName,
          anonymousVariants.size()
        );
        anonType = parseRecord(variant, anonymousName, "", globals, emitType);
      } else if (variantKind != "FieldDecl") {
        log("Skipping inner node for union {} of kind {}", cName, variantKind);
      } else {
        log("Defining union variant kind: {}", variantKind);
        std::string_view variantName;
        std::string_view fieldTypeName;
        TypeIndex variantType;
        variant["type"]["qualType"].get(fieldTypeName);
        if (
          fieldTypeName.starts_with("union ") ||
          fieldTypeName.starts_with("struct ")
        ) {
          if (anonType) {
            variantType = anonType;
            anonType = TypeIndex::null();
          } else {
            variantType = parseType(fieldTypeName, globals);
          }
        } else {
          variantType = parseType(fieldTypeName, globals);
        }

        if (variant["name"].get(variantName) == SUCCESS) {
          variantName = StringPool::inst().copy(variantName);
          namedVariants.push_back({variantType, variantName});
        } else {
          anonymousVariants.push_back(variantType);
        }
      }
    }

    auto typeIndex = Pool().addType(
      Union{
        .namedVariants = namedVariants,
        .anonymousVariants = anonymousVariants
      }
    );
    resultTypeIndex = typeIndex;
  } else {
    log("Unknown tag '{}' for C RecordDecl '{}'; skipping", tagUsed, cName);
    TODO("Error for parsing C record: invalid tag");
  }

  if (!cName.empty()) cTypes[cName] = resultTypeIndex;

  return resultTypeIndex;
}

Environment* cBindings(
  fs::path& cFile,
  string_view prefix,
  std::queue<std::string>& globals,
  TypeCache& definedTypes,
  TypeEmitter emitType,
  std::function<void(std::string_view)> emitStaticInline
) {
  auto fileName = cFile.string();
  static std::unordered_map<fs::path, Environment> importedFiles;

  for (auto [typeName, type] : definedTypes) {
    cTypes[typeName] = type;
  }

  if (importedFiles.contains(cFile)) {
    return &importedFiles[cFile];
  }
  Logger log(LogLevel::CImport);
  log("Importing {}", cFile.string());

  Environment& environment = importedFiles[cFile];

  // TODO: handle crash
  auto astDumpFile = ".blub/ast.json";
  auto command = fmt::format(
    "clang -Xclang -ast-dump=json -Xclang -ast-dump-filter={} {} '{}' > '{}'",
    prefix,
    fmt::join(CompilerContext::inst().c.clangArgs, " "),
    cFile.string(),
    astDumpFile
  );

  log("Dumping AST with command:\n{}", command);

  if (auto rc = system(command.c_str())) {
    crash("Error dumping AST for bindings for C file {}", cFile.string());
  }
  ondemand::parser parser;
  padded_string json;
  if (auto error = padded_string::load(astDumpFile).get(json)) {
    crash(
      "Unable to read C AST dump for '{}': {}",
      cFile.string(),
      error_message(error)
    );
  }
  ondemand::document_stream ast;
  if (auto error = parser.iterate_many(json, json.size()).get(ast)) {
    crash(
      "Unable to parse filtered C AST dump for '{}': {}",
      cFile.string(),
      error_message(error)
    );
  }

  // -ast-dump-filter emits a whitespace-separated sequence of declaration
  // objects, rather than one TranslationUnitDecl with an `inner` array.
  for (auto document : ast) {
    if (auto error = document.error()) {
      crash(
        "Unable to read filtered C AST declaration for '{}': {}",
        cFile.string(),
        error_message(error)
      );
    }
    ondemand::value node;
    if (auto error = document.get_value().get(node)) {
      crash(
        "Unable to parse filtered C AST declaration for '{}': {}",
        cFile.string(),
        error_message(error)
      );
    }
    string_view valueName, kind;
    if (node["kind"].get(kind) != SUCCESS || node["name"].get(valueName))
      continue;
    if (!valueName.starts_with(prefix)) {
      log(
        "skipping value '{}' due to not starting with prefix '{}'",
        valueName,
        prefix
      );
      continue;
    } else {
      log("Defining C value '{}'", valueName);
    }
    valueName = StringPool::inst().copy(valueName);
    string_view unprefixedValueName = valueName.substr(prefix.size());

    Reference blubInterface;
    if (environment.find(unprefixedValueName)) {
      log("Skipping duplicate C binding '{}'", unprefixedValueName);
      continue;
    }

    if (cTypes.contains(valueName)) {
      blubInterface.value = cTypes[valueName];
    } else if (kind == "EnumDecl") {
      u32 currentValue = 0;
      log(
        "Making enum '{}' with raw value '{}'",
        unprefixedValueName,
        TypeName(Pool()._s32)
      );
      auto [typeIndex, enumIndex] = Pool().addEnum(
        Pool()._s32,
        StringPool::inst().copy(unprefixedValueName)
      );
      std::vector<std::string_view> enumVals;
      if (auto inner = node["inner"]; inner.error() == SUCCESS) {
        for (auto element : inner.get_array()) {
          std::string_view valueName;
          element["name"].get(valueName);
          if (valueName[0] == '_') continue;
          enumVals.push_back(valueName);
        }
      }
      u32 prefixLength = longestPrefixEndingIn(enumVals, '_');
      log(
        "Enum prefix: {}\n{}",
        enumVals[0].substr(0, prefixLength),
        fmt::join(enumVals, "\n")
      );

      if (auto inner = node["inner"]; inner.error() == SUCCESS) {
        for (auto element : inner.get_array()) {
          std::string_view valueName;
          element["name"].get(valueName);
          if (valueName[0] == '_') {
            currentValue++;
            continue;
          }
          valueName = valueName.substr(1 + prefixLength);
          valueName = StringPool::inst().copy(valueName);
          if (element["inner"].has_value()) {
            ondemand::array array;
            bool error = element["inner"].get_array().get(array);
            if (error) {
              TODO("Error when reading enum-provided value");
            } else {
              std::string_view numberValue;
              array.at(0)["value"].get(numberValue);
              std::from_chars(
                numberValue.data(),
                numberValue.data() + numberValue.size(),
                currentValue
              );
            }
          }
          if (!Pool().getEnum(enumIndex).define(valueName, currentValue)) {
            auto definition = Pool().getEnum(enumIndex);
            fmt::println(
              std::cerr,
              "Duplicate enum value '{}' for enum '{}'",
              valueName,
              TypeName(typeIndex)
            );
            for (auto [name, _] : definition.values) {
              log("Variant: {}", name);
            }
            crash(
              "Duplicate enum value '{}' for enum '{}'",
              valueName,
              TypeName(typeIndex)
            );
          }

          currentValue++;
        }
      } else {
        crash("Empty enum");
      }

      cTypes[valueName] = typeIndex;

      blubInterface.value = typeIndex;
      globals.push(
        Environment::dumpEntryNames(Pool().getEnum(enumIndex)).str()
      );
    } else if (kind == "FunctionDecl") {
      log("Making function: {}", unprefixedValueName);
      bool isInline = false;
      std::string_view storageClass;
      node["inline"].get(isInline);
      node["storageClass"].get(storageClass);
      if (isInline && storageClass == "static" && emitStaticInline) {
        emitStaticInline(valueName);
      }
      std::string_view qualType;
      bool error = node["type"]["qualType"].get(qualType);
      if (error) {
        crash("Unable to get qualified type for C function '{}'", valueName);
      }

      // TODO: factor out to Types module?
      auto declareName = StringPool::inst().copy(valueName);
      TypeIndex type = parseType(qualType, globals);
      if (!Pool().functionType(type).has_value()) {
        crash(
          "Unable to get function type for C type '{}'\nBlub name: '{}'",
          qualType,
          TypeName(type)
        );
      }
      auto functionType = Pool().functionType(type).value();
      log(
        "Generating llvm declaration for C function: '{}': {}",
        unprefixedValueName,
        qualType
      );
      log("Internal name: {}", valueName);
      functionType.forwardDeclare(declareName, globals, "");
      blubInterface.value = Function(functionType, declareName);
    } else if (kind == "RecordDecl") {
      blubInterface.value =
        parseRecord(node, valueName, unprefixedValueName, globals, emitType);
    } else {
      log(
        "Skipping clang ast node of kind '{}'; name: '{}'",
        kind,
        unprefixedValueName
      );
      continue;
    }

    if (!environment.define(unprefixedValueName, blubInterface, {})) {
      // Clang commonly emits a RecordDecl followed by a same-named typedef.
      // Both map to the same unprefixed Blub binding, so retain the first one.
      log("Skipping duplicate C binding '{}'", unprefixedValueName);
    } else {
      log("Defining C type '{}'", unprefixedValueName);
    }
  }

  if (environment.defs.size() == 0) {
    crash("No bindings generated for imported C file {}", fileName);
  }
  return &environment;
}

TEST_CASE("cimport handles unions and anonymous promoted fields") {
  fs::path fixture =
    fs::path(BLUB_SOURCE_DIR) / "tests/cimport_union_fixture.h";
  std::queue<std::string> globals;
  TypeCache definedTypes;

  auto* env = cBindings(fixture, "ci_", globals, definedTypes);
  REQUIRE(env != nullptr);

  auto topUnionRef = env->find("top_union");
  REQUIRE(topUnionRef != nullptr);
  auto topUnionType = topUnionRef->unboxType();
  REQUIRE(topUnionType);
  CHECK(Pool().unbox<Union>(topUnionType) != nullptr);
  CHECK(Pool().getFieldPath(topUnionType, "as_int"));

  auto anonUnionRef = env->find("with_anon_union");
  REQUIRE(anonUnionRef != nullptr);
  auto anonUnionType = anonUnionRef->unboxType();
  REQUIRE(anonUnionType);
  auto promotedUnionField = Pool().getFieldPath(anonUnionType, "int_value");
  auto promotedLeaf = Pool().getFieldPath(anonUnionType, "leaf");
  CHECK(promotedUnionField);
  CHECK(promotedLeaf);
  CHECK_EQ(promotedUnionField.type, Pool()._s32);
  CHECK_EQ(promotedUnionField.segments.size(), 2);
  CHECK(
    Pool().unbox<Union>(promotedUnionField.segments.front().fieldType) !=
    nullptr
  );

  auto anonStructRef = env->find("with_anon_struct");
  REQUIRE(anonStructRef != nullptr);
  auto anonStructType = anonStructRef->unboxType();
  REQUIRE(anonStructType);
  auto promotedX = Pool().getFieldPath(anonStructType, "x");
  CHECK(promotedX);
  CHECK_EQ(promotedX.type, Pool()._s32);
  CHECK_EQ(promotedX.segments.size(), 2);

  auto nestedRef = env->find("with_nested_both");
  REQUIRE(nestedRef != nullptr);
  auto nestedType = nestedRef->unboxType();
  REQUIRE(nestedType);
  auto promotedLeft = Pool().getFieldPath(nestedType, "left");
  auto promotedPair = Pool().getFieldPath(nestedType, "pair");
  CHECK(promotedLeft);
  CHECK(promotedPair);
  CHECK_EQ(promotedLeft.type, Pool()._s32);
  CHECK_EQ(promotedLeft.segments.size(), 3);
}
