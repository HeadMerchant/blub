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
  return std::visit(
    overloaded{
      [&]<RecursiveType T>(T x) { return dereference(x.rawType()); },
      [](MultiPointer x) { return x.dereferencedType; },
      [](Pointer x) { return x.dereferencedType; },
      [](auto) { return TypeIndex::null(); },
    },
    typeDefinition
  );
  return TypeIndex::null();
}

template <> struct fmt::formatter<TypeIndex> : ostream_formatter {};

// TODO: remove
void TypePool::defineLLVMStruct(
  StructIndex structIndex,
  std::queue<std::string>& globals
) {
  Struct& structDefinition = getStruct(structIndex);
  globals.push(
    fmt::format(
      "%{} = type {{{}}}",
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
