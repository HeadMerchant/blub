#include "fmt/base.h"
#include "fmt/format.h"
#include "interpreter/value.h"
#include "types.h"
#include <charconv>
#include <filesystem>
#include <simdjson.h>
#include <stdexcept>
#include <unordered_map>
#include <vector>

using namespace simdjson;
namespace fs = std::filesystem;
using TypeCache = std::unordered_map<std::string_view, TypeIndex>;
TypeIndex parseType(std::string_view qualType, TypeCache& cTypes, std::ostream& o) {
  auto start = 0;
  if (qualType.starts_with("const ")) {
    qualType = qualType.substr(6);
    start = 6;
  }

  if (cTypes.contains(qualType)) {
    return cTypes[qualType];
  }

  TypeIndex type;
  auto found = qualType.find(' ');
  if (found == std::string::npos) {
    if (cTypes.contains(qualType)) {
      return cTypes[qualType];
    }

    throw std::invalid_argument(fmt::format("Undefined C type: {}", qualType));
  }

  auto baseTypeString = qualType.substr(0, found);
  if (!cTypes.contains(baseTypeString)) {
    throw std::invalid_argument(fmt::format("Undefined C type: {} in type {}", baseTypeString, qualType));
  }
  type = cTypes[baseTypeString];

  auto typeString = qualType.substr(found + 1);
  // Pointer
  if (typeString[0] == '*') {
    type = Types::Pool().pointerTo(type);
    typeString = typeString.substr(1);
  }

  // Sized arrays
  while (typeString[0] == '[') {
    typeString = typeString.substr(1);
    auto closingBracketIndex = typeString.find(']');
    auto lengthString = typeString.substr(0, closingBracketIndex);
    i32 arrayLength;
    std::from_chars(lengthString.data(), lengthString.data() + lengthString.size(), arrayLength);

    type = Types::Pool().sizedArrayOf(type, arrayLength);
    typeString = typeString.substr(closingBracketIndex + 1);
  }

  if (typeString.starts_with("(*)")) {
    typeString = typeString.substr(3);
  }

  // TODO: does this work for nested function pointers?
  if (typeString[0] == '(') {
    if (typeString == "(void)") {
      type = Types::Pool().addFunction(Types::FunctionType{.parameters = {0}, .returnType = type});
    } else {
      typeString = typeString.substr(1);
      std::vector<TypeIndex> paramTypes;
      while (typeString[0] != ')') {
        auto commaIndex = typeString.find(", ");
        if (commaIndex != std::string::npos) {
          auto subType = typeString.substr(0, commaIndex);
          paramTypes.push_back(parseType(subType, cTypes, o));
          typeString = typeString.substr(commaIndex + 1);
        } else {
          throw std::invalid_argument(fmt::format("Malformed C type '{}'", typeString));
        }
      }

      type = Types::Pool().addFunction(Types::FunctionType{.parameters = Types::Pool().tupleOf(paramTypes, o).second, .returnType = type});
    }
  }

  cTypes[qualType] = type;
  return type;
}

Environment* cBindings(fs::path cFile, std::string prefix, std::ostream& outputFile) {
  static std::unordered_map<fs::path, Environment*> importedFiles;
  static std::unordered_map<std::string_view, TypeIndex> cTypes = {
    {"uint8_t", Types::Pool().u8},
    {"uint16_t", Types::Pool().u16},
    {"uint32_t", Types::Pool().u32},
    {"__uint64_t", Types::Pool().s64},
    {"int", Types::Pool().s32},
    {"char", Types::Pool().u8},
    {"size_t", Types::Pool().usize},
    {"void", Types::Pool()._void}};
  if (importedFiles.contains(cFile)) {
    return importedFiles[cFile];
  }

  Environment* environment = new Environment();

  // TODO: handle crash
  auto astDumpFile = "ast.json";
  execl("clang", "-Xclang", "-ast-dump=json", cFile.c_str(), ">", astDumpFile);
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
      auto [typeIndex, enumIndex] = Types::Pool().addEnum(Types::Pool().s32, stableValueName);
      auto enumDefinition = Types::Pool().getEnum(enumIndex);
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
          if (!enumDefinition.define(valueName, currentValue)) {
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
      TypeIndex type = parseType(qualType, cTypes, outputFile);
      auto functionType = Types::Pool().functionType(type).value();
      bool returnsStruct = Types::Pool().storageType(functionType.returnType) == Types::LLVMStorage::VARIABLE;
      bool returnsVoid = Types::Pool().isVoid(functionType.returnType);
      fmt::print(outputFile, "declare ");
      auto returnType = functionType.returnType;
      if (returnsStruct | returnsVoid) {
        fmt::print(outputFile, "void ");
      } else {
        fmt::print(outputFile, "{} ", LlvmName(returnType));
      }
      fmt::print(outputFile, "{} ", declareName);
      bool hasMultiple = false;
      if (returnsStruct) {
        hasMultiple = true;
        fmt::print(outputFile, "ptr sret({}) align {}", LlvmName(returnType), Types::Pool().getSizing(returnType).alignment.byteAlignment());
      }
      for (auto paramType : Types::Pool().tupleElements(functionType.parameters)) {
        if (hasMultiple) outputFile << ", ";
        hasMultiple = true;
        auto typeName = paramType;
        if (Types::Pool().isLlvmLiteralType(paramType)) {
          fmt::print(outputFile, "{}", LlvmName(typeName));
        } else {
          fmt::print(outputFile, "ptr byval({})", LlvmName(typeName));
        }
      }
      outputFile << ")\n";
      blubInterface.value = Function(functionType, declareName);
    } else if (kind == "RecordDecl") {
      std::string_view tag;
      node["tag"].get(tag);
      if (tag != "struct") {
        throw std::invalid_argument(fmt::format("Unknown tag '{}' for C record '{}'", tag, valueName));
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

        Types::Pool().getStruct(structIndex).defineField(fieldName, parseType(fieldType, cTypes, outputFile));
      }

      Types::Pool().setStructSizing(structIndex);
      Types::Pool().defineLLVMStruct(structIndex, outputFile);
    } else {
      TODO(fmt::format("Unknown clang ast node kind: '{}'", kind));
    }

    environment->define(unprefixedValueName, blubInterface);
  }
  return environment;
}
