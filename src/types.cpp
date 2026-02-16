#include "common.h"
#include "fmt/format.h"
#include "types.h"
#include <ranges>

Logger logger(LogLevel::Compile);

Types::TypePool& Types::Pool() {
  static Types::TypePool pool = Types::TypePool();
  return pool;
}

Types::OptionalType Types::TypePool::dereference(TypeIndex type) {
  auto typeDefinition = underlyingTypes[type.value];
  if (auto multiPtr = std::get_if<MultiPointer>(&typeDefinition)) {
    logger("dereferencing mutlipointer type {}", TypeName(type));
    return multiPtr->dereferencedType;
  } else if (auto ptr = std::get_if<Pointer>(&typeDefinition)) {
    logger("dereferencing pointer type {}", TypeName(type));
    return ptr->dereferencedType;
  }

  return std::nullopt;
}

template <> struct fmt::formatter<TypeIndex> : ostream_formatter {};

void Types::TypePool::defineLLVMStruct(Types::StructIndex structIndex, std::queue<std::string>& globals) {
  Types::Struct& structDefinition = getStruct(structIndex);
  auto typeNames = structDefinition.fieldTypes | std::views::transform([this](const auto x) { return LlvmName(x); });
  globals.push(
    fmt::format(
      "{} = type {{{}}}", structDefinition.llvmName, fmt::join(structDefinition.fieldTypes | std::views::transform([this](const auto x) { return LlvmName(x); }), ", ")));
}

void Types::TypePool::debugTypes() {
  fmt::println("Pool contains these types:");
  for (u32 j = 0; j < underlyingTypes.size(); j++) {
    fmt::println("{}: {}", j, TypeName(TypeIndex{j}));
  }
}

void Types::FunctionType::forwardDeclare(std::string_view name, std::queue<std::string>& globals) {
  auto returnType = this->returnType;
  Types::LLVMStorage returnStorage = Types::Pool().storageType(returnType);
  auto parameterTypes = Types::Pool().tupleElements(this->parameters);

  std::stringstream instruction;
  fmt::print(instruction, "declare ");
  if (Types::Pool().isLiteralReturn(returnType)) {
    fmt::print(instruction, "{} ", LlvmName(returnType));
  } else {
    fmt::print(instruction, "void ");
  }
  fmt::print(instruction, "{}(", name);

  bool hasParameters = false;
  if (returnStorage == Types::LLVMStorage::VARIABLE) {
    // TODO: factor out %return register
    fmt::print(instruction, "ptr noalias sret({}) align {} %return", LlvmName(returnType), Types::Pool().getSizing(returnType).alignment.byteAlignment());
    hasParameters = true;
  }
  for (auto paramType : parameterTypes) {
    if (hasParameters) {
      instruction << ", ";
    }
    hasParameters = true;

    bool isLiteralParameter = Types::Pool().isLlvmLiteralType(paramType);
    if (isLiteralParameter) {
      fmt::print(instruction, "{}", LlvmName(paramType));
    } else {
      // TODO: type alignment; for now align to s64
      fmt::print(instruction, "ptr byval({})", LlvmName(paramType));
    }
  }

  instruction << ")";

  globals.push(instruction.str());
}

bool TypeIndex::isInfer() {
  return *this == Types::Pool().infer;
}
