#include "common.h"
#include "fmt/format.h"
#include "types.h"
#include <ranges>

Logger logger(LogLevel::Compile);

TypePool& Pool() {
  static TypePool pool = TypePool();
  return pool;
}

OptionalType TypePool::dereference(TypeIndex type) {
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

void TypePool::defineLLVMStruct(StructIndex structIndex, std::queue<std::string>& globals) {
  Struct& structDefinition = getStruct(structIndex);
  globals.push(
    fmt::format("{} = type {{{}}}", structDefinition.llvmName, fmt::join(structDefinition.fieldTypes | transform([this](const auto x) { return LlvmName(x); }), ", "))
  );
}

void TypePool::debugTypes() {
  fmt::println("Pool contains these types:");
  for (u32 j = 0; j < underlyingTypes.size(); j++) {
    fmt::println("{}: {}", j, TypeName(TypeIndex{j}));
  }
}

void FunctionType::forwardDeclare(std::string_view name, std::queue<std::string>& globals) {
  auto returnType = this->returnType;
  LLVMStorage returnStorage = Pool().storageType(returnType);
  auto parameterTypes = Pool().tupleElements(this->parameters);

  std::stringstream instruction;
  fmt::print(instruction, "declare ");
  if (Pool().isLiteralReturn(returnType)) {
    fmt::print(instruction, "{} ", LlvmName(returnType));
  } else {
    fmt::print(instruction, "void ");
  }
  fmt::print(instruction, "{}(", name);

  bool hasParameters = false;
  if (returnStorage == LLVMStorage::VARIABLE) {
    // TODO: factor out %return register
    fmt::print(instruction, "ptr noalias sret({}) align {} %return", LlvmName(returnType), Pool().getSizing(returnType).alignment.byteAlignment());
    hasParameters = true;
  }
  for (auto paramType : parameterTypes) {
    if (hasParameters) {
      instruction << ", ";
    }
    hasParameters = true;

    bool isLiteralParameter = Pool().isLlvmLiteralType(paramType);
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
  return *this == Pool().infer;
}
