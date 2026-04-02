#include "types.h"
#include "abi.h"
#include "common.h"
#include "fmt/format.h"
#include <ranges>

Logger logger(LogLevel::Compile);

TypePool& Pool() {
  static TypePool pool = TypePool();
  return pool;
}

TypeSpan StructIndex::fields() {
  return Pool().getStruct(*this).fieldTypes;
}

TypeSpan TupleIndex::fields() {
  return Pool().tupleElements(*this);
}

TypeIndex EnumIndex::rawType() {
  return Pool().getEnum(*this).rawType;
}

TypeIndex AlignedType::rawType() {
  return this->baseType;
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

void TypePool::defineLLVMStruct(
  StructIndex structIndex,
  std::queue<std::string>& globals
) {
  Struct& structDefinition = getStruct(structIndex);
  globals.push(
    fmt::format(
      "{} = type {{{}}}",
      structDefinition.llvmName,
      fmt::join(
        structDefinition.fieldTypes |
          transform([this](const auto x) { return LlvmName(x); }),
        ", "
      )
    )
  );
}

void TypePool::debugTypes() {
  fmt::println("Pool contains these types:");
  for (u32 j = 0; j < underlyingTypes.size(); j++) {
    fmt::println("{}: {}", j, TypeName(TypeIndex{j}));
  }
}

void FunctionType::forwardDeclare(
  std::string_view name,
  std::queue<std::string>& globals
) {
  std::stringstream instruction;
  fmt::print(instruction, "declare ");
  Function function{.type = *this, .globalName = name};
  declareParamRegisters(instruction, function);
  globals.push(instruction.str());
}

bool TypeIndex::isInfer() {
  return *this == Pool().infer;
}

void TypePool::registerStorage(
  TypeIndex typeIndex,
  RegisterAssignment& assignment
) {
  auto sizing = getSizing(typeIndex);
  if (sizing.byteSize == 0) return;
  if (sizing.byteSize > 16) {
    assignment.push(RegisterType::Memory);
    return;
  }

  auto type = getType(typeIndex);
  auto startLength = assignment.length;
  std::visit(
    overloaded{
      [&]<IntRegister T>(T) {
        assignment.push(RegisterType::Int, sizing.byteSize);
      },
      [&](Float x) { assignment.push(RegisterType::Float, x.byteSize()); },
      [&]<AggregateType T>(T x) {
        for (auto element : x.fields()) {
          registerStorage(element, assignment);
        }
      },
      [&](EnumIndex x) { registerStorage(getEnum(x).rawType, assignment); },
      [&](auto x) {
        fmt::println(
          "Error for trying to get storage type for type that can't be passed: "
          "'{}'",
          TypeName(typeIndex)
        );
        TODO(
          "Error for trying to get storage type for type that can't be passed: "
          "'{}'"
        );
      },
    },
    type
  );
  if (!assignment.isMemory()) {
    assert(assignment.length - startLength == sizing.byteSize);
  }
}

void TypeIndex::debug() {
  fmt::println("{}: {}", TypeName(*this), LlvmName(*this));
}
