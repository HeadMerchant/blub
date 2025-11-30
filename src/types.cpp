#include "fmt/format.h"
#include "types.h"
#include <ranges>

Types::TypePool& Types::Pool() {
  static Types::TypePool pool = Types::TypePool();
  return pool;
}

Types::OptionalType Types::TypePool::dereference(TypeIndex type) {
  auto typeDefinition = underlyingTypes[type.value];
  if (auto multiPtr = std::get_if<MultiPointer>(&typeDefinition)) {
    fmt::println("dereferencing mutlipointer type {}", TypeName(type));
    return multiPtr->dereferencedType;
  } else if (auto ptr = std::get_if<Pointer>(&typeDefinition)) {
    fmt::println("dereferencing pointer type {}", TypeName(type));
    return ptr->dereferencedType;
  }

  return std::nullopt;
}

template <> struct fmt::formatter<TypeIndex> : ostream_formatter {};
std::pair<TypeIndex, Types::TupleIndex> Types::TypePool::tupleOf(std::vector<TypeIndex> types, std::queue<std::string>& globals) {
  if (tuples.contains(types)) {
    return tuples[types];
  }

  auto elementTypes = std::span(types);
  auto llvmNames = types | std::views::transform([](TypeIndex x) { return LlvmName(x); });
  TupleIndex tupleIndex{(i32)tuplePool.size()};

  globals.push(fmt::format("%.tuple.{} = type {{{}}}", tupleIndex.value, fmt::join(llvmNames, ", ")));

  auto typeName = types | std::views::transform([](TypeIndex x) { return TypeName(x); });
  // TODO: sizing
  if (types.size() == 1) {
    if (auto enumIndex = std::get_if<EnumIndex>(&getType(types[0]))) {
      fmt::println("Tuple with enum index: '{}' with raw type '{}'", enumIndex->value, enumPool[enumIndex->value].rawType.value);
    }
  }
  Sizing sizing;
  tuplePool.emplace_back(elementTypes, getSizing(elementTypes), fmt::format("({})", fmt::join(typeName, ", ")));

  auto typeIndex = addType(tupleIndex);

  std::pair<TypeIndex, TupleIndex> cached{typeIndex, tupleIndex};
  tuples[std::move(types)] = cached;
  return cached;
}

void Types::TypePool::defineLLVMStruct(Types::StructIndex structIndex, std::queue<std::string>& globals) {
  Types::Struct& structDefinition = getStruct(structIndex);
  auto typeNames = structDefinition.fieldTypes | std::views::transform([this](const auto x) { return LlvmName(x); });
  globals.push(fmt::format("{} = type {{{}}}", structDefinition.llvmName, fmt::join(structDefinition.fieldTypes | std::views::transform([this](const auto x) {return LlvmName(x);}), ", ")));
}

void Types::TypePool::debugTypes() {
  fmt::println("Pool contains these types:");
  for (i32 j = 0; j < underlyingTypes.size(); j++) {
    fmt::println("{}: {}", j, TypeName(TypeIndex{j}));
  }
}
