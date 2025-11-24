#include "fmt/base.h"
#include "fmt/format.h"
#include "types.h"
#include "value.h"
#include <cctype>
#include <charconv>
#include <cstdlib>
#include <filesystem>
#include <simdjson.h>
#include <stdexcept>
#include <unordered_map>
#include <vector>

using namespace simdjson;
namespace fs = std::filesystem;
using TypeCache = std::unordered_map<std::string_view, TypeIndex>;

struct StringViewPool {
  char* bytes;
  i32 offset;
  i32 max;

  std::string_view copy(std::string_view view) {
    i32 newOffset = offset + view.length();
    if (newOffset < offset || offset >= max) {
      throw std::invalid_argument("OOM in string view pool");
    }
    std::memcpy(bytes + offset, view.data(), view.length());

    return {bytes + offset, view.length()};
  }
};

TypeIndex parseType(std::string_view qualType, TypeCache& cTypes, std::queue<std::string>& globals) {
  // fmt::println("Parsing C type '{}'", qualType);
  // TODO(mut)
  if (qualType.starts_with("const ")) {
    qualType = qualType.substr(6);
  }

  if (cTypes.contains(qualType)) {
    return cTypes[qualType];
  }

  // Tokenize base type
  i32 endIndex = 0;
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
  fmt::println("base type: {}", baseTypeString);
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
      i32 arrayLength;
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
    // fmt::println("Making function with return type: {}", TypeName(type));
    if (modifiers == "(void)") {
      fmt::println("Empty params {}", modifiers);
      std::vector<TypeIndex> emptyTuple;
      auto [_, paramTypes] = Types::Pool().tupleOf(std::move(emptyTuple), globals);
      type = Types::Pool().addFunction(Types::FunctionType{.parameters = paramTypes, .returnType = type});
    } else {
      fmt::println("Not empty params {}", modifiers);
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

      auto [_, paramTuple] = Types::Pool().tupleOf(paramTypes, globals);
      type = Types::Pool().addFunction(Types::FunctionType{.parameters = paramTuple, .returnType = type});
    }
  }

  cTypes[qualType] = type;
  return type;
}

Environment* cBindings(fs::path cFile, std::string prefix, std::queue<std::string>& globals) {
  static std::unordered_map<fs::path, Environment> importedFiles;
  static std::unordered_map<std::string_view, TypeIndex> cTypes = {
    {"uint8_t",    Types::Pool().u8   },
    {"uint16_t",   Types::Pool().u16  },
    {"uint32_t",   Types::Pool().u32  },
    {"uint64_t",   Types::Pool().u64  },
    {"int8_t",     Types::Pool().s8   },
    {"int16_t",    Types::Pool().s16  },
    {"int32_t",    Types::Pool().s32  },
    {"int64_t",    Types::Pool().s64  },
    {"__uint64_t", Types::Pool().s64  },
    {"int",        Types::Pool().s32  },
    {"char",       Types::Pool().u8   },
    {"size_t",     Types::Pool().usize},
    {"void",       Types::Pool()._void},
    {"intptr_t",   Types::Pool().usize},
    {"uintptr_t",  Types::Pool().usize},
    {"bool",       Types::Pool()._bool},
    {"char",       Types::Pool().u8   },
    {"float",      Types::Pool().f32  },
    {"double",     Types::Pool().f64  },
  };
  fmt::println("Importing {}", cFile.string());
  if (importedFiles.contains(cFile)) {
    return &importedFiles[cFile];
  }

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
    std::string_view unprefixedValueName = valueName.substr(prefix.size());
    std::string stableValueName(unprefixedValueName);
    unprefixedValueName = stableValueName;

    Reference blubInterface;
    if (kind == "EnumDecl") {
      i32 currentValue = 0;
      fmt::println("Making enum '{}' with raw value '{}'", unprefixedValueName, TypeName(Types::Pool().s32));
      auto [typeIndex, enumIndex] = Types::Pool().addEnum(Types::Pool().s32, std::move(stableValueName));
      if (auto inner = node["inner"]; inner.error() == SUCCESS) {
        for (auto element : inner.get_array()) {
          std::string_view valueName;
          element["name"].get(valueName);
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
      std::string_view qualType;
      bool error = node["type"]["qualType"].get(qualType);
      if (error) {
        throw std::invalid_argument(fmt::format("Unable to get qualified type for C function '{}'", valueName));
      }

      // TODO: factor out to Types module?
      auto declareName = fmt::format("@{}", valueName);
      TypeIndex type = parseType(qualType, cTypes, globals);
      if (!Types::Pool().functionType(type).has_value()) {
        fmt::println("Unable to get function type for C type '{}'", qualType);
        fmt::println("Blub name: '{}'", TypeName(type));
        abort();
      }
      auto functionType = Types::Pool().functionType(type).value();
      fmt::println("Generating llvm declaration for C function: '{}': {}", unprefixedValueName, qualType);
      fmt::println("Internal name: {}", valueName);
      bool returnsStruct = Types::Pool().storageType(functionType.returnType) == Types::LLVMStorage::VARIABLE;
      bool returnsVoid = Types::Pool().isVoid(functionType.returnType);
      std::stringstream instruction;
      fmt::print(instruction, "declare ");
      auto returnType = functionType.returnType;
      if (returnsStruct | returnsVoid) {
        fmt::print(instruction, "void ");
      } else {
        fmt::print(instruction, "{} ", LlvmName(returnType));
      }
      fmt::print(instruction, "{} ", declareName);
      bool hasMultiple = false;
      if (returnsStruct) {
        hasMultiple = true;
        fmt::print(instruction, "ptr sret({}) align {}", LlvmName(returnType), Types::Pool().getSizing(returnType).alignment.byteAlignment());
      }
      for (auto paramType : Types::Pool().tupleElements(functionType.parameters)) {
        if (hasMultiple) instruction << ", ";
        hasMultiple = true;
        auto typeName = paramType;
        if (Types::Pool().isLlvmLiteralType(paramType)) {
          fmt::print(instruction, "{}", LlvmName(typeName));
        } else {
          fmt::print(instruction, "ptr byval({})", LlvmName(typeName));
        }
      }
      instruction << ")\n";
      globals.push(instruction.str());
      blubInterface.value = Function(functionType, declareName);
    } else if (kind == "RecordDecl") {
      std::string_view tag;
      node["tagUsed"].get(tag);
      if (tag != "struct") {
        fmt::println("Unknown tag '{}' for C RecordDecl '{}'; skipping", tag, valueName);
        continue;
      }
      auto llvmName = fmt::format("%.cstruct.{}", valueName);
      auto [typeIndex, structIndex] = Types::Pool().makeStruct(std::string(unprefixedValueName), llvmName);

      Reference* value;
      ondemand::array structFields;
      if (node["inner"].get_array().get(structFields)) {
        throw std::invalid_argument(fmt::format("Error parsing fields for C struct '{}'", valueName));
      }

      for (auto structField : structFields) {
        std::string_view fieldName;
        std::string_view fieldType;
        structField["name"].get(fieldName);
        structField["type"]["qualType"].get(fieldType);

        Types::Pool().getStruct(structIndex).defineField(fieldName, parseType(fieldType, cTypes, globals));
      }

      Types::Pool().setStructSizing(structIndex);
      Types::Pool().defineLLVMStruct(structIndex, globals);
      cTypes[valueName] = typeIndex;
    } else {
      fmt::println("Skipping clang ast node of kind '{}'; name: '{}'", kind, unprefixedValueName);
      // TODO(fmt::format("Unknown clang ast node kind: '{}'", kind));
    }

    environment.define(unprefixedValueName, blubInterface);
  }
  importedFiles[cFile] = environment;
  return &environment;
}

// template <typename... Args> [[noreturn]] void crash(fs::path& path, fmt::format_string<Args...> fmt, Args&&... args) const {
//   auto& out = std::cerr;
//   auto location = locationOf(lexeme());
//   fmt::println(out, "C import error in file {} at line {}:{}", path.string(), location.line, location.column);
//   location.underline(out);
//   fmt::println(out, fmt, std::forward<Args>(args)...);
//   abort();
// }
