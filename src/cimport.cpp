#include "cimport.h"
#include "common.h"
#include "fmt/base.h"
#include "fmt/format.h"
#include "types.h"
#include "value.h"
#include <cctype>
#include <charconv>
#include <cstdlib>
#include <filesystem>
#include <optional>
#include <queue>
#include <simdjson.h>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
#include <vector>

using namespace simdjson;
namespace fs = std::filesystem;

TypeIndex parseType(std::string_view qualType, TypeCache& cTypes, std::queue<std::string>& globals) {
  Logger log(LogLevel::CImport);
  // TODO(mut)
  if (qualType.starts_with("const ")) {
    qualType = qualType.substr(6);
  }

  if (cTypes.contains(qualType)) {
    return cTypes[qualType];
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
    throw std::invalid_argument(fmt::format("Undefined C type: {} in type {}", baseTypeString, qualType));
  }
  log("base type: {}", baseTypeString);
  TypeIndex type = cTypes[baseTypeString];

  auto modifiers = qualType.substr(endIndex);
  while (true) {
    if (modifiers.starts_with(" *")) {
      modifiers = modifiers.substr(2);
      type = Types::Pool().pointerTo(type);
    } else if (modifiers.starts_with('[')) {
      modifiers = modifiers.substr(1);
      auto closingBracketIndex = modifiers.find(']');
      auto lengthString = modifiers.substr(0, closingBracketIndex);
      u32 arrayLength;
      std::from_chars(lengthString.data(), lengthString.data() + lengthString.size(), arrayLength);

      type = Types::Pool().sizedArrayOf(type, arrayLength);
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
      auto [_, paramTypes] = Types::Pool().tupleOf(std::move(emptyTuple));
      type = Types::Pool().addFunction(Types::FunctionType{.parameters = paramTypes, .returnType = type});
    } else {
      modifiers = modifiers.substr(1);
      std::vector<TypeIndex> paramTypes;
      while (true) {
        auto commaIndex = modifiers.find(", ");
        if (commaIndex != std::string::npos) {
          auto subType = modifiers.substr(0, commaIndex);
          paramTypes.push_back(parseType(subType, cTypes, globals));
          modifiers = modifiers.substr(commaIndex + 2);
          continue;
        }

        auto parenIndex = modifiers.find(")");
        if (parenIndex == std::string::npos) {
          throw std::invalid_argument(fmt::format("Malformed C type '{}'", modifiers));
        }

        auto paramType = modifiers.substr(0, parenIndex);
        paramTypes.push_back(parseType(paramType, cTypes, globals));
        break;
      }

      auto [_, paramTuple] = Types::Pool().tupleOf(paramTypes);
      type = Types::Pool().addFunction(Types::FunctionType{.parameters = paramTuple, .returnType = type});
    }
  }

  cTypes[qualType] = type;
  return type;
}

u32 longestPrefixEndingIn(std::span<std::string_view> strings, char lastChar) {
  u32 prevLength = 0;
  u32 currentLength = 0;
  while (true) {
    auto start = prevLength + 1;
    auto i = start;
    for (; i < strings[0].length(); i++) {
      if (strings[0][i] == lastChar) break;
    }

    if (i == prevLength + 1) break;

    for (auto string : strings) {
      if (i >= string.length()) return prevLength;
      if (string.substr(start, i - prevLength) != strings[0].substr(start, i - prevLength)) return prevLength;
    }
    prevLength = i;
  }

  return prevLength;
}

TypeIndex parseRecord(ondemand::value& node, Identifier cName, Identifier unprefixedName, TypeCache& cTypes, std::queue<std::string>& globals) {
  Logger log(LogLevel::CImport);
  std::string_view tagUsed;
  node["tagUsed"].get(tagUsed);
  TypeIndex resultTypeIndex;
  if (tagUsed == "struct") {
    static u32 anonIndex;
    auto [typeIndex, structIndex] =
      Types::Pool().makeStruct(std::string(unprefixedName), cName.empty() ? fmt::format("%.cstruct.{}", anonIndex++) : fmt::format("%.cstruct.{}", cName));

    ondemand::array structFields;
    if (node["inner"].get_array().get(structFields)) {
      throw std::invalid_argument(fmt::format("Error parsing fields for C struct '{}'", unprefixedName));
    }

    Types::OptionalType anonType;
    for (auto structField : structFields) {
      std::string_view fieldKind;
      structField["kind"].get(fieldKind);
      if (fieldKind == "FieldDecl") {
        std::string_view fieldName;
        structField["name"].get(fieldName);
        fieldName = StringPool::inst().copy(fieldName);

        TypeIndex fieldType;
        if (anonType) {
          fieldType = anonType.value();
        } else {
          std::string_view fieldTypeName;
          structField["type"]["qualType"].get(fieldTypeName);
          fieldType = parseType(fieldTypeName, cTypes, globals);
        }
        Types::Pool().getStruct(structIndex).defineField(fieldName, fieldType);
      } else if (fieldKind == "RecordDecl") {
        anonType = parseRecord(structField.value(), "", "", cTypes, globals);
      } else {
        log("Skipping inner node for struct of kind {}", fieldKind);
      }
    }

    Types::Pool().setStructSizing(structIndex);
    Types::Pool().defineLLVMStruct(structIndex, globals);
    resultTypeIndex = typeIndex;
  } else if (tagUsed == "union") {
    std::vector<TypeIndex> anonymousVariants;
    std::vector<pair<TypeIndex, Identifier>> namedVariants;
    ondemand::array variants;
    if (node["inner"].get_array().get(variants)) {
      throw std::invalid_argument(fmt::format("Error parsing variants for C union '{}'", cName));
    }

    Types::OptionalType anonType;
    for (ondemand::value variant : variants) {
      std::string_view variantKind;
      variant["kind"].get(variantKind);
      if (variantKind == "RecordDecl") {
        anonType = parseRecord(variant, "", "", cTypes, globals);
      } else if (variantKind != "FieldDecl") {
        log("Skipping inner node for union {} of kind {}", cName, variantKind);
      } else {
        log("Defining union variant kind: {}", variantKind);
        std::string_view variantName;
        std::string_view fieldTypeName;
        TypeIndex variantType;
        variant["type"]["qualType"].get(fieldTypeName);
        if (fieldTypeName.starts_with("union ") || fieldTypeName.starts_with("struct ")) {
          if (anonType) {
            variantType = anonType.value();
            anonType = std::nullopt;
          } else {
            throw std::invalid_argument("Unknown type for union variant");
          }
        } else {
          variantType = parseType(fieldTypeName, cTypes, globals);
        }

        if (variant["name"].get(variantName)) {
          variantName = StringPool::inst().copy(variantName);
          namedVariants.push_back({variantType, variantName});
        } else {
          anonymousVariants.push_back(variantType);
        }
      }
    }

    auto typeIndex = Types::Pool().addType(Types::Union{.namedVariants = namedVariants, .anonymousVariants = anonymousVariants});
    resultTypeIndex = typeIndex;
  } else {
    log("Unknown tag '{}' for C RecordDecl '{}'; skipping", tagUsed, cName);
    TODO("Error for parsing C record: invalid tag");
  }

  if (!cName.empty()) cTypes[cName] = resultTypeIndex;

  return resultTypeIndex;
}

Environment* cBindings(fs::path cFile, std::string prefix, std::queue<std::string>& globals, TypeCache& definedTypes) {
  auto fileName = cFile.string();
  static std::unordered_map<fs::path, Environment> importedFiles;
  static TypeCache cTypes = {
    {"uint8_t",     Types::Pool()._u8   },
    {"uint16_t",    Types::Pool()._u16  },
    {"uint32_t",    Types::Pool()._u32  },
    {"uint64_t",    Types::Pool()._u64  },
    {"int8_t",      Types::Pool()._s8   },
    {"int16_t",     Types::Pool()._s16  },
    {"int32_t",     Types::Pool()._s32  },
    {"int64_t",     Types::Pool()._s64  },
    {"__uint64_t",  Types::Pool()._u64  },
    {"__uint128_t", Types::Pool()._u128 },
    {"int",         Types::Pool()._s32  },
    {"char",        Types::Pool()._u8   },
    {"size_t",      Types::Pool()._usize},
    {"void",        Types::Pool()._void },
    {"intptr_t",    Types::Pool()._usize},
    {"uintptr_t",   Types::Pool()._usize},
    {"bool",        Types::Pool()._bool },
    {"char",        Types::Pool()._u8   },
    {"float",       Types::Pool()._f32  },
    {"double",      Types::Pool()._f64  },
    // TODO: vector types
    {"__m128",      Types::Pool()._void },
  };

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
  auto astDumpFile = "ast.json";
  auto command = fmt::format("clang -Xclang -ast-dump=json {} > {}", cFile.string(), astDumpFile);
  system(command.c_str());
  ondemand::parser parser;
  auto json = padded_string::load(astDumpFile);
  ondemand::document ast = parser.iterate(json);

  for (auto node : ast["inner"]) {
    std::string_view valueName, kind;
    if (node["kind"].get(kind) != SUCCESS || node["name"].get(valueName)) continue;
    if (!valueName.starts_with(prefix)) continue;
    valueName = StringPool::inst().copy(valueName);
    std::string_view unprefixedValueName = valueName.substr(prefix.size());

    Reference blubInterface;
    if (cTypes.contains(valueName)) {
      blubInterface.value = cTypes[valueName];
    } else if (kind == "EnumDecl") {
      u32 currentValue = 0;
      log("Making enum '{}' with raw value '{}'", unprefixedValueName, TypeName(Types::Pool()._s32));
      auto [typeIndex, enumIndex] = Types::Pool().addEnum(Types::Pool()._s32, std::string(unprefixedValueName));
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
      log("Enum prefix: {}\n{}", enumVals[0].substr(0, prefixLength), fmt::join(enumVals, "\n"));

      if (auto inner = node["inner"]; inner.error() == SUCCESS) {
        for (auto element : inner.get_array()) {
          std::string_view valueName;
          element["name"].get(valueName);
          if (valueName[0] != '_') {
            valueName = valueName.substr(1 + prefixLength);
          }
          valueName = StringPool::inst().copy(valueName);
          if (element["inner"].has_value()) {
            ondemand::array array;
            bool error = element["inner"].get_array().get(array);
            if (error) {
              TODO("Error when reading enum-provided value");
            } else {
              std::string_view numberValue;
              array.at(0)["value"].get(numberValue);
              std::from_chars(numberValue.data(), numberValue.data() + numberValue.size(), currentValue);
            }
          }
          if (!Types::Pool().getEnum(enumIndex).define(valueName, currentValue)) {
            auto definition = Types::Pool().getEnum(enumIndex);
            fmt::println(std::cerr, "Duplicate enum value '{}' for enum '{}'", valueName, TypeName(typeIndex));
            for (auto [name, _] : definition.values) {
              log("Variant: {}", name);
            }
            throw std::invalid_argument(fmt::format("Duplicate enum value '{}' for enum '{}'", valueName, TypeName(typeIndex)));
          }

          currentValue++;
        }
      } else {
        throw std::invalid_argument("Empty enum");
      }

      cTypes[valueName] = typeIndex;

      blubInterface.value = typeIndex;
    } else if (kind == "FunctionDecl") {
      log("Making function: {}", unprefixedValueName);
      std::string_view qualType;
      bool error = node["type"]["qualType"].get(qualType);
      if (error) {
        throw std::invalid_argument(fmt::format("Unable to get qualified type for C function '{}'", valueName));
      }

      // TODO: factor out to Types module?
      auto declareName = StringPool::inst().copy(fmt::format("@{}", valueName));
      TypeIndex type = parseType(qualType, cTypes, globals);
      if (!Types::Pool().functionType(type).has_value()) {
        fmt::println(std::cerr, "Unable to get function type for C type '{}'", qualType);
        fmt::println(std::cerr, "Blub name: '{}'", TypeName(type));
        abort();
      }
      auto functionType = Types::Pool().functionType(type).value();
      log("Generating llvm declaration for C function: '{}': {}", unprefixedValueName, qualType);
      log("Internal name: {}", valueName);
      functionType.forwardDeclare(declareName, globals);
      blubInterface.value = Function(functionType, declareName);
    } else if (kind == "RecordDecl") {
      blubInterface.value = parseRecord(node.value(), valueName, unprefixedValueName, cTypes, globals);
    } else {
      log("Skipping clang ast node of kind '{}'; name: '{}'", kind, unprefixedValueName);
    }

    environment.define(unprefixedValueName, blubInterface);
  }

  // if (Logger::globalLevels & LogLevel::CImport) {
  //   fmt::println("C Types");
  //   for (auto [cName, blubName] : cTypes) {
  //     fmt::println("{}: {}", cName, TypeName(blubName));
  //   }
  // }
  importedFiles[cFile] = environment;
  return &environment;
}
