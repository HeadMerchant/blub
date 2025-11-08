#include "types.h"
#include <ranges>

Types::TypePool& Types::Pool() {
  static Types::TypePool pool;
  return pool;
}

template <> struct fmt::formatter<TypeIndex> : ostream_formatter {};
std::pair<TypeIndex, Types::TupleIndex> Types::TypePool::tupleOf(std::vector<TypeIndex> types, std::ostream& irFile) {
  if (types.empty()) {
    return {_void, {0}};
  }
  // if (types.size() == 1) {
  //   return {};
  //   TODO("1-tuples should just be the same as their element type");
  // }
  if (tuples.contains(types)) {
    return tuples[types];
  }

  auto elementTypes = std::span(types);
  auto llvmNames = types | std::views::transform([](TypeIndex x) { return LlvmName(x); });
  TupleIndex tupleIndex{(i32)tuplePool.size()};

  fmt::println(irFile, "%.tuple.{} = type \{{}}", tupleIndex.value, fmt::join(llvmNames, ", "));

  auto typeName = types | std::views::transform([](TypeIndex x) { return TypeName(x); });
  tuplePool.emplace_back(elementTypes, getSizing(std::span(types)), fmt::format("({})", tupleIndex.value, fmt::join(typeName, ", ")));

  auto typeIndex = addType(tupleIndex);

  std::pair<TypeIndex, TupleIndex> cached{typeIndex, tupleIndex};
  tuples[types] = cached;
  return cached;
}

void Types::TypePool::defineLLVMStruct(Types::StructIndex structIndex, std::ostream& outputFile) {
  Types::Struct& structDefinition = getStruct(structIndex);
  auto typeNames = structDefinition.fields | std::views::transform([this](const Struct::StructField x) { return LlvmName(x.type); });
  fmt::print(outputFile, "{} = type \{{}}", structDefinition.llvmName, fmt::join(typeNames, ", "));
}
