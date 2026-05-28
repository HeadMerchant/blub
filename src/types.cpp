#include "abi.h"
#include "common.h"
#include "fmt/format.h"
#include "types.h"
#include <ranges>

Logger TypePool::logger(LogLevel::TypeCheck);

TypePool& Pool() {
  static TypePool pool = TypePool();
  return pool;
}

StructFieldType StructIndex::fields() {
  return Pool().getStruct(*this).fieldTypes();
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

  return TypeIndex::null();
}

template <> struct fmt::formatter<TypeIndex> : ostream_formatter {};

void TypePool::defineLLVMStruct(
  StructIndex structIndex,
  std::queue<std::string>& globals
) {
  Struct& structDefinition = getStruct(structIndex);
  globals.push(
    fmt::format(
      "%.struct.{} = type {{{}}}",
      structDefinition.llvmName,
      fmt::join(
        structDefinition.fieldTypes() |
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
  RegisterName name,
  std::queue<std::string>& globals,
  string_view extraDeclarationInfo
) {
  std::stringstream instruction;
  fmt::print(instruction, "declare ");
  instruction << extraDeclarationInfo;
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
  std::visit(
    overloaded{
      [&]<IntRegister T>(T) {
        assignment.push(RegisterType::Int, sizing.byteSize);
      },
      [&](Float x) { assignment.push(RegisterType::Float, sizing.byteSize); },
      [&]<AggregateType T>(T x) {
        for (auto element : x.fields()) {
          registerStorage(element, assignment);
        }
      },
      [&]<RecursiveType T>(T x) { registerStorage(x.rawType(), assignment); },
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
  if (auto structIndex = std::get_if<StructIndex>(&type)) {
    auto structDef = getStruct(*structIndex);
    if (structDef.name == "quat" || structDef.name == "vec4") {
      fmt::println("Using {} registers: {:#b}; All int: {}", structDef.name, assignment.types, assignment.allInt());
    }
  }
}

void TypeIndex::debug() {
  fmt::println("{}: {}", TypeName(*this), LlvmName(*this));
}

span<TypeIndex> TypePool::coerceableTypes(TypeIndex type) {
  static vector<TypeIndex> floatTypes = {Pool()._f32, Pool()._f16, Pool()._f64};
  // TODO: add ints
  static vector<TypeIndex> intTypes = {Pool().floatLiteral};
  auto underlying = getType(type);
  if (isAny<FloatLiteralType>(underlying)) return floatTypes;
  if (isAny<IntLiteralType>(underlying)) return intTypes;
  return {};
}
