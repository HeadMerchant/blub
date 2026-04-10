#pragma once
#include "common.h"
#include "fmt/base.h"
#include "registers.h"
#include <algorithm>
#include <array>
#include <concepts>
#include <cstdint>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <fmt/ranges.h>
#include <iostream>
#include <limits>
#include <optional>
#include <queue>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

struct Reference;

struct TypeIndex {
  u32 value;
  bool operator==(const TypeIndex& other) const {
    return value == other.value;
  }
  bool isInfer();

  __attribute__((noinline, used)) void debug();
};

template <> struct std::hash<TypeIndex> {
  std::size_t operator()(const TypeIndex& k) const {
    using std::hash;
    return hash<u32>()(k.value);
  }
};

template <> struct std::hash<vector<TypeIndex>> {
  std::size_t operator()(const vector<TypeIndex>& vec) const {
    std::size_t seed = vec.size();
    for (auto& i : vec) {
      seed ^= i.value + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
  }
};

const u32 NUM_BUILTINS = 14;
const std::string_view SliceName = "%.slice";

enum class LLVMStorage { VOID, LITERAL, VARIABLE };

using OptionalType = std::optional<TypeIndex>;
using TypeSpan = std::span<TypeIndex>;

struct TypeField {
  TypeIndex type;
  u32 index;
};

using FieldMap = std::unordered_map<Identifier, TypeField>;

using std::same_as;
template <typename T>
concept AggregateType = requires(T t) {
  { t.fields() } -> std::ranges::range;
  requires same_as<std::ranges::range_value_t<decltype(t.fields())>, TypeIndex>;
};

struct StructIndex {
  u32 value;
  TypeSpan fields();
};
static_assert(
  AggregateType<StructIndex>,
  "StructIndex must implement method 'fields'"
);

template <typename T>
concept RecursiveType = requires(T t) {
  { t.rawType() } -> same_as<TypeIndex>;
};
struct EnumIndex {
  u32 value;
  TypeIndex rawType();
};
static_assert(
  RecursiveType<EnumIndex>,
  "EnumIndex must implement method 'rawType'"
);

struct TupleIndex {
  u32 value;
  bool operator==(const TupleIndex& other) const {
    return value == other.value;
  }
  TypeSpan fields();
};
static_assert(
  AggregateType<TupleIndex>,
  "TupleIndex must implement method 'fields'"
);
template <> struct std::hash<TupleIndex> {
  std::size_t operator()(const TupleIndex& x) const {
    return std::hash<u32>()(x.value);
  }
};

struct FunctionType {
  TupleIndex parameters;
  TypeIndex returnType;
  bool operator==(const FunctionType& other) const {
    return parameters == other.parameters && returnType == other.returnType;
  }

  void forwardDeclare(std::string_view name, std::queue<std::string>& globals);
};
template <> struct std::hash<FunctionType> {
  std::size_t operator()(const FunctionType& k) const {
    using std::hash;
    return (
      hash<u32>()(k.parameters.value) ^ (hash<u32>()(k.returnType.value) << 1)
    );
  }
};

struct Log2Alignment {
  uint8_t value;
  static Log2Alignment fromByteSize(u32 byteSize) {
    uint8_t log2Alignment =
      std::numeric_limits<u32>::digits - 1 - __builtin_clz(byteSize);
    return {log2Alignment};
  }

  static Log2Alignment fromBitSize(u32 bitSize) {
    auto byteSize = 1 + (bitSize - 1) / 8;
    return fromByteSize(byteSize);
  }

  u32 byteAlignment() const {
    return 1 << value;
  }

  friend std::ostream& operator<<(std::ostream& o, const Log2Alignment& self) {
    o << self.byteAlignment();
    return o;
  }
};

struct Sizing {
  u32 byteSize;
  u32 bitSize;
  Log2Alignment alignment;

  static Sizing fromBitSize(u32 bitSize) {
    if (bitSize == 0) {
      TODO("Zero-sized structs/int sizing");
    }
    u32 byteSize = 1 + (bitSize - 1) / 8;
    auto alignment = Log2Alignment::fromByteSize(byteSize);

    if (bitSize <= 8) {
      assert(byteSize == 1);
      assert(alignment.byteAlignment() == 1);
    } else if (bitSize <= 16) {
      assert(byteSize == 2);
      assert(alignment.byteAlignment() == 2);
    } else if (bitSize <= 32) {
      assert(byteSize == 4);
      assert(alignment.byteAlignment() == 4);
    }
    return Sizing{
      .byteSize = byteSize,
      .bitSize = bitSize,
      .alignment = alignment
    };
  }

  static Sizing alignToPointer(u32 byteSize) {
    return Sizing{
      .byteSize = byteSize,
      .bitSize = byteSize * 8,
      .alignment = Log2Alignment::fromByteSize(8)
    };
  }
};

struct Struct {
  FieldMap fields;
  // SymbolMap statics;
  std::string name;
  std::string llvmName;
  std::vector<TypeIndex> fieldTypes;

  Struct(std::string name, std::string llvmName)
      : name(name), llvmName(llvmName) {}

  std::optional<TypeField> getField(std::string_view fieldName) {
    if (fields.contains(fieldName)) {
      return fields[fieldName];
    }

    return std::nullopt;
  }

  bool defineField(Identifier name, TypeIndex type) {
    auto [_, success] = fields.insert({
      name,
      TypeField{.type = type, .index = (u32)fields.size()}
    });
    fieldTypes.push_back(type);

    return success;
  }
};

struct Enum {
  std::unordered_map<Identifier, uint64_t> values;
  std::string name;
  TypeIndex rawType;
  TypeIndex enumType;

  Enum(std::string name, TypeIndex rawType)
      : values(), name(name), rawType(rawType) {}

  bool define(Identifier valueName, uint64_t value) {
    bool succeeded = values.emplace(valueName, value).second;
    return succeeded;
  }

  std::optional<uint64_t> get(Identifier valueName) {
    if (!values.contains(valueName)) {
      return std::nullopt;
    }
    return values[valueName];
  }
};

struct PointerType {
  TypeIndex pointer;
  TypeIndex slice;
  TypeIndex multiPointer;
};

struct VoidType {};
struct Infer {};
struct SignedInt {
  u32 bitSize;
};

struct UnsignedInt {
  u32 bitSize;
};
struct Float {
  enum class Precision { f16, f32, f64 };
  using enum Precision;
  Precision precision;
  u32 bitSize() {
    return precision == f16 ? 16 : precision == f32 ? 32 : 64;
  }

  u32 byteSize() {
    return bitSize() / 8;
  }
};
struct Pointer {
  TypeIndex dereferencedType;
};
struct MultiPointer {
  TypeIndex dereferencedType;
};
struct Slice {
  TypeIndex dereferencedType;
};
struct FunctionIndex {
  u32 index;
};
struct Opaque {
  std::string name;
  std::string llvmName;
};
struct SizedArray {
  TypeIndex dereferencedType;
  u32 length;
  auto fields() {
    using namespace std::views;
    return iota(0, (int)length) |
           transform([&](int) { return dereferencedType; });
  }
};
static_assert(
  AggregateType<SizedArray>,
  "SizedArray must implement method 'fields'"
);

struct NeverType {};
struct IntLiteralType {};
struct FloatLiteralType {};
struct Type {};
struct EnvironmentType {};
// TODO: implement params and return type
struct TypeOfGeneric {};
struct RangeLiteral {};
struct AlignedType {
  TypeIndex baseType;
  Log2Alignment newAlignment;
  TypeIndex rawType();
};
static_assert(
  RecursiveType<AlignedType>,
  "AlignedType must implement method 'rawType'"
);

// TODO: tagged unions
struct Union {
  std::vector<pair<TypeIndex, Identifier>> namedVariants;
  std::vector<TypeIndex> anonymousVariants;
};

struct VectorType {
  TypeIndex elementType;
  u8 length;
};

using UnderlyingType = std::variant<
  VoidType,
  SignedInt,
  UnsignedInt,
  Float,
  Pointer,
  MultiPointer,
  Slice,
  StructIndex,
  TupleIndex,
  EnumIndex,
  FunctionType,
  Infer,
  Opaque,
  SizedArray,
  NeverType,
  IntLiteralType,
  FloatLiteralType,
  Type,
  EnvironmentType,
  TypeOfGeneric,
  RangeLiteral,
  AlignedType,
  Union,
  VectorType>;
template <typename T>
concept IntRegister =
  same_as<T, SignedInt> || same_as<T, UnsignedInt> || same_as<T, Pointer> ||
  same_as<T, MultiPointer> || same_as<T, FunctionType> || same_as<T, Slice>;

struct Tuple {
  std::span<TypeIndex> types;
  Sizing sizing;
};

enum class LlvmReturn {
  Void,
  Literal,
  Aggregate,
  Memory,
};

class TypePool {
private:
  bool testFlag;

public:
  static TypePool pool;
  std::vector<UnderlyingType> underlyingTypes;

  std::unordered_map<TypeIndex, PointerType> pointersTo;
  std::vector<Struct> structPool;
  std::vector<Enum> enumPool;

  std::vector<Tuple> tuplePool;
  std::vector<TypeIndex> tupleTypeIndices;
  std::unordered_map<std::vector<TypeIndex>, std::pair<TypeIndex, TupleIndex>>
    tuples;
  std::unordered_map<u32, std::unordered_map<TypeIndex, TypeIndex>> sizedArrays;
  // TODO: function
  std::unordered_map<FunctionType, TypeIndex> functionCache;
  std::unordered_map<TypeIndex, std::array<TypeIndex, 9>> alignmentTypes;
  unordered_map<TypeIndex, unordered_map<Identifier, Reference*>>
    associatedValues;

  // TODO: enum

  // LLVM types
  TypeIndex _void;
  TypeIndex _bool;
  TypeIndex _u8;
  TypeIndex _u16;
  TypeIndex _u32;
  TypeIndex _u64;
  TypeIndex _u128;
  TypeIndex _s8;
  TypeIndex _s16;
  TypeIndex _s32;
  TypeIndex _s64;
  TypeIndex _s128;
  TypeIndex _f16;
  TypeIndex _f32;
  TypeIndex _f64;
  TypeIndex _usize;
  TypeIndex _isize;

  // Type system jank
  TypeIndex infer;
  TypeIndex never;
  TypeIndex intLiteral;
  TypeIndex floatLiteral;
  TypeIndex type;
  TypeIndex environment;
  TypeIndex generic;
  TypeIndex rangeLiteral;

  TypeIndex addType(UnderlyingType type) {
    u32 index = underlyingTypes.size();
    underlyingTypes.push_back(type);
    return {index};
  }

  TypePool() {
    // 23 builtins initialized
    underlyingTypes.reserve(256);

    _void = addType(VoidType{});
    _u8 = addType(UnsignedInt(8));
    _u16 = addType(UnsignedInt(16));
    _u32 = addType(UnsignedInt(32));
    _u64 = addType(UnsignedInt(64));
    _u128 = addType(UnsignedInt(128));
    _s8 = addType(SignedInt(8));
    _s16 = addType(SignedInt(16));
    _s32 = addType(SignedInt(32));
    _s64 = addType(SignedInt(64));
    _s128 = addType(SignedInt(128));
    // TODO: change based on target word size
    _usize = _u64;
    _isize = _s64;

    _f16 = addType(Float(Float::f16));
    _f32 = addType(Float(Float::f32));
    _f64 = addType(Float(Float::f64));
    _bool = addType(UnsignedInt(1));

    infer = addType(Infer{});
    never = addType(NeverType{});
    intLiteral = addType(IntLiteralType{});
    floatLiteral = addType(FloatLiteralType{});

    type = addType(Type{});
    environment = addType(EnvironmentType{});
    generic = addType(TypeOfGeneric{});
    rangeLiteral = addType(RangeLiteral{});
  }

  TypeIndex pointerTo(TypeIndex type) {
    return pointerTypesFor(type).pointer;
  }

  TypeIndex sliceOf(TypeIndex type) {
    return pointerTypesFor(type).slice;
  }

  TypeIndex multiPointerTo(TypeIndex type) {
    return pointerTypesFor(type).multiPointer;
  }

  PointerType pointerTypesFor(TypeIndex type) {
    assert(type.value < underlyingTypes.size());

    if (pointersTo.contains(type)) {
      return pointersTo[type];
    }

    PointerType pointers = {
      .pointer = addType(Pointer(type)),
      .slice = addType(Slice(type)),
      .multiPointer = addType(MultiPointer(type)),
    };
    pointersTo[type] = pointers;

    return pointers;
  }

  UnderlyingType& getType(TypeIndex type) {
    return underlyingTypes[type.value];
  }

  OptionalType dereference(TypeIndex type);

  std::pair<TypeIndex, StructIndex> makeStruct(
    std::string name,
    std::string llvmName
  ) {
    StructIndex structIndex{(u32)structPool.size()};
    structPool.emplace_back(name, llvmName);
    return {addType(structIndex), structIndex};
  }

  std::optional<Struct*> getStruct(TypeIndex type) {
    if (auto structIndex = std::get_if<StructIndex>(&getType(type))) {
      return &structPool[structIndex->value];
    }
    return std::nullopt;
  }

  Struct& getStruct(StructIndex index) {
    return structPool[index.value];
  }

  // std::optional<Struct*> getStruct(TypeIndex index) {
  //   Type definition = (*this)[index];
  //   if (definition.type == Intrinsic::STRUCT) {
  //     return &getStruct(StructIndex{definition.definition});
  //   }

  //   return std::nullopt;
  // }

  bool isPointer(TypeIndex index) {
    auto underlying = underlyingTypes[index.value];
    return std::holds_alternative<Pointer>(underlying) ||
           std::holds_alternative<MultiPointer>(underlying);
  }

  std::optional<TypeIndex> unboxReference(TypeIndex index) {
    if (auto pointer = std::get_if<Pointer>(&underlyingTypes[index.value])) {
      return pointer->dereferencedType;
    }
    return std::nullopt;
  }

  std::optional<pair<TypeField, TypeIndex>> getFieldIndex(
    TypeIndex typeIndex,
    std::string_view fieldName
  ) {
    auto structDefinition = getStruct(typeIndex);
    if (structDefinition) {
      if (auto fieldDef = (*structDefinition)->getField(fieldName)) {
        return pair(*fieldDef, typeIndex);
      }
      return std::nullopt;
    }

    if (auto slice = std::get_if<Slice>(&getType(typeIndex))) {
      if (fieldName == "data") {
        return pair(
          TypeField{
            .type = multiPointerTo(slice->dereferencedType),
            .index = 0
          },
          typeIndex
        );
      }
      if (fieldName == "length") {
        return pair(TypeField{.type = _usize, .index = 1}, typeIndex);
      }
    }

    if (auto unionType = std::get_if<Union>(&getType(typeIndex))) {
      for (auto variantType : unionType->anonymousVariants) {
        if (auto fieldResult = getFieldIndex(variantType, fieldName))
          return fieldResult;
      }
    }

    return std::nullopt;
  }

  // TODO: function overloading???
  // TODO: support nested tuples
  OptionalType isAssignable(TypeIndex valueIndex, TypeIndex targetIndex) {
    UnderlyingType valueType = getType(valueIndex);
    UnderlyingType targetType = getType(targetIndex);

    auto type = coerce(valueIndex, targetIndex);
    if (!type.has_value()) return std::nullopt;

    if (
      std::holds_alternative<Infer>(valueType) ||
      std::holds_alternative<VoidType>(valueType)
    ) {
      return std::nullopt;
    }

    if (type == intLiteral) return _s32;
    if (type == floatLiteral) return _f32;

    return type;
  }

  OptionalType isTupleAssignable(TypeIndex value, TypeIndex targetType) {
    if (value == targetType) {
      return value;
    }

    std::span<TypeIndex> valueElements = tupleElements(value);
    std::span<TypeIndex> targetElements = tupleElements(targetType);
    if (valueElements.size() != targetElements.size()) {
      return std::nullopt;
    }

    for (int i = 0; i < valueElements.size(); i++) {
      if (!isAssignable(
             value = valueElements[i],
             targetType = targetElements[i]
          )
             .has_value())
        return std::nullopt;
    }

    return targetType;
  }

  // void printTypes() {
  //   std::cout << "Number of types: " << names.size() << "\n";
  //   for (auto name : names) {
  //       std::cout << name << "\n";
  //   }
  //   std::cout << std::endl;
  // }

  std::pair<TypeIndex, TupleIndex> tupleOf(std::vector<TypeIndex> types) {
    if (tuples.contains(types)) {
      return tuples[types];
    }

    auto elementTypes = std::span(types);
    TupleIndex tupleIndex{(u32)tuplePool.size()};

    tuplePool.emplace_back(elementTypes, getSizing(elementTypes));

    auto typeIndex = addType(tupleIndex);
    tupleTypeIndices.push_back(typeIndex);

    std::pair<TypeIndex, TupleIndex> cached{typeIndex, tupleIndex};
    tuples[std::move(types)] = cached;
    return cached;
  }

  TupleIndex tupleIndex(TypeIndex type) {
    if (auto tuple = std::get_if<TupleIndex>(&underlyingTypes[type.value])) {
      return *tuple;
    } else {
      TODO("Error for trying to get tuple index from type that isn't tuple");
    }
  }

  std::span<TypeIndex> tupleElements(TypeIndex type) {
    TupleIndex index = tupleIndex(type);
    return tupleElements(index);
  }

  std::span<TypeIndex> tupleElements(TupleIndex type) {
    return tuplePool[type.value].types;
  }

  TypeIndex addFunction(FunctionType type) {
    if (functionCache.contains(type)) return functionCache[type];

    // TODO: copy pointer sizes
    // TypeIndex typeIndex = addType(
    //   {.type = Intrinsic::FUNCTION, .llvmName = "ptr", .definition = (i32)
    //   functionTypePool.size()}, fmt::format("({}) -> {}",
    //   typeName(tupleTypeIndices[type.parameters.value]),
    //   typeName(type.returnType))
    // );
    auto typeIndex = addType(type);
    functionCache[type] = typeIndex;

    return typeIndex;
  }

  std::optional<FunctionType> functionType(TypeIndex type) {
    auto underlyingType = getType(type);
    if (auto function = std::get_if<FunctionType>(&underlyingType)) {
      return *function;
    }

    return std::nullopt;
  }

  bool isVoid(TypeIndex type) {
    return std::holds_alternative<VoidType>(getType(type));
  }

  OptionalType sliceElementType(TypeIndex type) {
    if (auto boxed = std::get_if<Slice>(&getType(type))) {
      return boxed->dereferencedType;
    }
    return std::nullopt;
  }

  OptionalType multiPointerElement(TypeIndex type) {
    if (auto boxed = std::get_if<MultiPointer>(&getType(type))) {
      return boxed->dereferencedType;
    }
    return std::nullopt;
  }

  std::optional<SizedArray> sizedArray(TypeIndex type) {
    if (auto boxed = std::get_if<AlignedType>(&getType(type))) {
      return sizedArray(boxed->baseType);
    }
    if (auto boxed = std::get_if<SizedArray>(&getType(type))) {
      return *boxed;
    }
    return std::nullopt;
  }

  TypeIndex sizedArrayOf(TypeIndex elementType, u32 size) {
    auto& sizes = sizedArrays[size];
    if (sizes.contains(elementType)) {
      return sizes[elementType];
    }

    auto index = addType(SizedArray(elementType, size));
    sizes[elementType] = index;
    return index;
  }

  TypeIndex addOpaque(std::string name) {
    return addType(Opaque(name));
  }

  std::pair<TypeIndex, EnumIndex> addEnum(Enum enumDefinition) {
    EnumIndex enumIndex{(u32)enumPool.size()};
    enumPool.push_back(enumDefinition);
    return {addType(enumIndex), enumIndex};
  }

  std::pair<TypeIndex, EnumIndex> addEnum(TypeIndex rawType, std::string name) {
    return addEnum(Enum(name, std::move(rawType)));
  }

  Enum* getEnum(TypeIndex type) {
    if (auto enumIndex = std::get_if<EnumIndex>(&getType(type))) {
      return &getEnum(*enumIndex);
    }
    return nullptr;
  }

  Enum& getEnum(EnumIndex type) {
    return enumPool[type.value];
  }

  bool isFloat(TypeIndex type) {
    return isAny<Float, FloatLiteralType>(getType(type));
  }

  std::optional<Float> getFloat(TypeIndex type) {
    if (auto boxed = std::get_if<Float>(&getType(type))) {
      return *boxed;
    }

    return std::nullopt;
  }

  bool isSignedInt(TypeIndex type) {
    return std::holds_alternative<SignedInt>(getType(type));
  }

  bool isUnsignedInt(TypeIndex type) {
    return std::holds_alternative<UnsignedInt>(getType(type));
  }

  bool isInt(TypeIndex type) {
    return isSignedInt(type) || isUnsignedInt(type) ||
           isAny<IntLiteralType>(getType(type));
  }

  bool isInfer(TypeIndex type) {
    return std::holds_alternative<Infer>(getType(type));
  }

  void defineLLVMStruct(
    StructIndex structDefinition,
    std::queue<std::string>& globals
  );

  Sizing getSizing(TypeIndex type) {
    return std::visit(
      overloaded{
        [](VoidType x) { return Sizing{0, 0}; },
        [](SignedInt x) { return Sizing::fromBitSize(x.bitSize); },
        [](UnsignedInt x) { return Sizing::fromBitSize(x.bitSize); },
        [](Float x) { return Sizing::fromBitSize(x.bitSize()); },
        [](Pointer x) { return Sizing::fromBitSize(64); },
        [](MultiPointer x) { return Sizing::fromBitSize(64); },
        [](Slice x) { return Sizing::alignToPointer(2 * 8); },
        // [this]<AggregateType T>(T x) { return getSizing() },
        [this](StructIndex x) { return getSizing(getStruct(x).fieldTypes); },
        [this](TupleIndex x) { return getSizing(tupleElements(x)); },
        [this](EnumIndex x) { return getSizing(enumPool[x.value].rawType); },
        [](FunctionType x) { return Sizing::fromBitSize(64); },
        [](Infer x) {
          TODO("Error for sizing an inferred type");
          return Sizing{};
        },
        [](Opaque x) {
          TODO("Error for sizing an opaque type");
          return Sizing{};
        },
        [this](SizedArray x) {
          auto sizing = getSizing(x.dereferencedType);
          auto length = x.length;
          return Sizing{
            .byteSize = length * sizing.byteSize,
            .bitSize = length * sizing.bitSize,
            .alignment = sizing.alignment
          };
        },
        [](NeverType) {
          TODO("Error for sizing Never type");
          return Sizing{};
        },
        [](IntLiteralType) {
          TODO("Error for sizing IntLiteral type");
          return Sizing{};
        },
        [](FloatLiteralType) {
          TODO("Error for sizing FloatLiteral type");
          return Sizing{};
        },
        [](Type) {
          TODO("Error for sizing Type type");
          return Sizing{};
        },
        [](EnvironmentType) {
          TODO("Error for sizing Environment type");
          return Sizing{};
        },
        [](TypeOfGeneric) {
          TODO("Error for sizing Generic type");
          return Sizing{};
        },
        [](RangeLiteral) {
          TODO("Error for sizing Range type");
          return Sizing{};
        },
        [this](AlignedType x) {
          Sizing sizing = getSizing(x.baseType);
          sizing.alignment = x.newAlignment;
          sizing.byteSize =
            alignTo(sizing.byteSize, sizing.alignment.byteAlignment());
          return sizing;
        },
        [this](Union& x) {
          Sizing sizing;
          for (auto [type, _] : x.namedVariants) {
            auto variantSizing = getSizing(type);
            sizing.alignment.value =
              std::max(variantSizing.alignment.value, sizing.alignment.value);
            sizing.bitSize = std::max(variantSizing.bitSize, sizing.bitSize);
            sizing.byteSize = std::max(variantSizing.byteSize, sizing.byteSize);
          }

          for (auto type : x.anonymousVariants) {
            auto variantSizing = getSizing(type);
            sizing.alignment.value =
              std::max(variantSizing.alignment.value, sizing.alignment.value);
            sizing.bitSize = std::max(variantSizing.bitSize, sizing.bitSize);
            sizing.byteSize = std::max(variantSizing.byteSize, sizing.byteSize);
          }

          sizing.byteSize =
            alignTo(sizing.byteSize, sizing.alignment.byteAlignment());
          // TODO: How does bit sizing work here?
          return sizing;
        },
        [this](VectorType x) {
          auto baseSizing = getSizing(x.elementType);
          return Sizing{
            .byteSize = baseSizing.byteSize * x.length,
            .bitSize = baseSizing.bitSize * x.length,
            .alignment = Log2Alignment::fromByteSize(32),
          };
        },
      },
      getType(type)
    );
  }

private:
  void registerStorage(TypeIndex typeIndex, RegisterAssignment& assignment);

public:
  RegisterAssignment registerStorage(
    TypeIndex typeIndex,
    CallingConvention cc = CallingConvention::C
  ) {
    RegisterAssignment registers;
    registerStorage(typeIndex, registers);
    return registers;
  }

  u32 alignTo(u32 size, u32 alignment) {
    return (size + alignment - 1) & ~(alignment - 1);
  }

  Sizing getSizing(TypeSpan types) {
    u32 structSize = 0;
    u32 structAlignment = 1;

    for (auto field : types) {
      auto fieldSizing = getSizing(field);

      auto fieldAlignment = fieldSizing.alignment.byteAlignment();
      structSize = alignTo(structSize, fieldAlignment);
      structSize += fieldSizing.byteSize;
      structAlignment = std::max(structAlignment, fieldAlignment);
    }

    structSize = alignTo(structSize, structAlignment);
    return Sizing{
      .byteSize = structSize,
      .bitSize = structSize * 8,
      .alignment = Log2Alignment::fromByteSize(structAlignment)
    };
  }

  OptionalType coerce(TypeIndex a, TypeIndex b) {
    if (a == b) {
      return a;
    }
    // {
    //   auto unboxedA = a;
    //   if (auto aAligned = std::get_if<AlignedType>(&aType)) {
    //     unboxedA = aAligned->baseType;
    //     aType = getType(aAligned->baseType);
    //   }
    //   auto unboxedB = b;
    //   if (auto bAligned = std::get_if<AlignedType>(&bType)) {
    //     unboxedB = bAligned->baseType;
    //     bType = getType(bAligned->baseType);
    //   }
    //   if (unboxedA == unboxedB) {
    //     return a;
    //   }
    // }
    auto aType = getType(a);
    auto bType = getType(b);

    if (subTypes(aType, bType)) return b;
    if (subTypes(bType, aType)) return a;

    return std::nullopt;
  }

  bool subTypes(UnderlyingType& child, UnderlyingType& parent) {
    if (isAny<Infer>(child)) return true;
    if (isAny<NeverType>(child)) return !isAny<Infer>(parent);
    if (isAny<IntLiteralType>(child)) {
      return isAny<SignedInt, UnsignedInt, FloatLiteralType, Float>(parent);
    }
    if (isAny<FloatLiteralType>(child)) {
      return isAny<Float>(parent);
    }
    if (auto parentVal = std::get_if<Pointer>(&parent)) {
      return parentVal->dereferencedType == _void && isAny<Pointer>(child);
    }
    if (auto parentVal = std::get_if<MultiPointer>(&parent)) {
      return parentVal->dereferencedType == _void && isAny<MultiPointer>(child);
    }
    return false;
  }

  TypeIndex alignType(TypeIndex baseType, Log2Alignment alignment) {
    auto underlyingType = getType(baseType);
    if (auto existingAligned = std::get_if<AlignedType>(&underlyingType)) {
      baseType = existingAligned->baseType;
    }
    auto baseAlignment = getSizing(baseType).alignment.value;
    if (baseAlignment == alignment.value) {
      return baseType;
    }

    if (baseAlignment < alignment.value) {
      auto index = alignment.value - 1;
      auto typeIndex =
        addType(AlignedType{.baseType = baseType, .newAlignment = alignment});
      alignmentTypes[baseType][index] = typeIndex;
      return typeIndex;
    } else {
      TODO("Error for shrinking alignment");
    }
  }

  static constexpr u32 maxVectorWidth = 8;
  std::unordered_map<TypeIndex, std::array<TypeIndex, maxVectorWidth>>
    vectorTypes;

  TypeIndex vectorOf(TypeIndex elementType, u8 length) {
    assert(length < maxVectorWidth);
    if (vectorTypes.contains(elementType)) {
      return vectorTypes[elementType][length];
    }

    std::array<TypeIndex, maxVectorWidth>& types = vectorTypes[elementType];
    TypeIndex result;
    for (u8 i = 0; i < maxVectorWidth; i++) {
      auto type =
        addType(VectorType{.elementType = elementType, .length = (u8)(i + 1)});
      types[i] = type;
      if (i == length) result = type;
    }
    return result;
  }

  void debugTypes();

  template <typename T> T* unbox(TypeIndex type) {
    if (auto x = std::get_if<T>(&getType(type))) return x;
    return nullptr;
  }

  bool isAggregate(TypeIndex type) {
    return std::visit(
      overloaded{
        []<AggregateType T>(T x) { return true; },
        [](auto x) { return false; },
      },
      getType(type)
    );
  }
};

TypePool& Pool();

struct TypeName {
  TypeIndex type;

  static void print(std::ostream& o, UnderlyingType& type) {
    std::visit(
      overloaded{
        [&o](VoidType x) { o << "void"; },
        [&o](SignedInt x) { o << "s" << x.bitSize; },
        [&o](UnsignedInt x) { o << "u" << x.bitSize; },
        [&o](Float x) { o << "f" << x.bitSize(); },
        [&o](Pointer x) {
          o << "^";
          print(o, x.dereferencedType);
        },
        [&o](MultiPointer x) {
          o << "[^]";
          print(o, x.dereferencedType);
        },
        [&o](Slice x) {
          o << "[]";
          print(o, x.dereferencedType);
        },
        [&o](StructIndex x) { o << Pool().structPool[x.value].name; },
        [&o](TupleIndex x) {
          bool hasMultiple = false;
          o << "(";
          for (auto type : Pool().tupleElements(x)) {
            if (hasMultiple) {
              o << ", ";
            }

            hasMultiple = true;
            print(o, type);
          }
          o << ")";
        },
        [&o](EnumIndex x) { o << Pool().enumPool[x.value].name; },
        [&o](FunctionType x) {
          print(o, Pool().tupleTypeIndices[x.parameters.value]);
          o << " -> ";
          print(o, x.returnType);
        },
        [&o](Infer x) { o << "infer"; },
        [&o](Opaque x) { o << x.name; },
        [&o](SizedArray x) {
          o << "[" << x.length << "]";
          print(o, x.dereferencedType);
        },
        [&o](NeverType x) { o << "never"; },
        [&o](IntLiteralType) { o << "int literal"; },
        [&o](FloatLiteralType) { o << "float literal"; },
        [&o](TypeOfGeneric) { o << "generic"; },
        [&o](EnvironmentType) { o << "environment"; },
        [&o](Type) { o << "type"; },
        [&o](RangeLiteral) { o << "range"; },
        [&o](AlignedType x) {
          fmt::print(o, "@align({}) ", x.newAlignment.byteAlignment());
          print(o, x.baseType);
        },
        [&o](Union x) {
          o << "(";
          auto hasMultiple = false;
          for (auto [type, fieldName] : x.namedVariants) {
            if (hasMultiple) o << " | ";
            hasMultiple = true;
            fmt::print(o, "{}: ", fieldName);
            print(o, type);
          }
          for (auto type : x.anonymousVariants) {
            if (hasMultiple) o << " | ";
            hasMultiple = true;
            print(o, type);
          }
          o << ")";
        },
        [&o](VectorType x) {
          fmt::print(o, "<{}>", x.length);
          print(o, x.elementType);
        },
      },
      type
    );
  }

  static void print(std::ostream& o, TypeIndex type) {
    print(o, Pool().getType(type));
  }

  friend std::ostream& operator<<(std::ostream& o, const TypeName& type) {
    print(o, type.type);
    return o;
  }
};

struct LlvmName {
  TypeIndex type;
  static void format(std::ostream& o, TypeIndex type) {
    auto underlyingType = Pool().getType(type);
    std::visit(
      overloaded{
        [&o](VoidType x) { o << "void"; },
        [&o](SignedInt x) { o << "i" << x.bitSize; },
        [&o](UnsignedInt x) { o << "i" << x.bitSize; },
        [&o](Float x) {
          o
            << (x.precision == Float::f16
                  ? "half"
                  : (x.precision == Float::f32 ? "float" : "double"));
        },
        [&o](Pointer x) { o << "ptr"; },
        [&o](MultiPointer x) { o << "ptr"; },
        [&o](Slice x) { o << "%.slice"; },
        [&o](StructIndex x) { o << Pool().structPool[x.value].llvmName; },
        [&o](TupleIndex x) {
          bool hasMultiple = false;
          o << "{";
          for (auto type : Pool().tupleElements(x)) {
            if (hasMultiple) {
              o << ", ";
            }
            hasMultiple = true;
            format(o, type);
          }
          o << "}";
        },
        [&o](EnumIndex x) { format(o, Pool().enumPool[x.value].rawType); },
        [&o](FunctionType x) { o << "ptr"; },
        [&o](Opaque x) { o << x.llvmName; },
        [&o](Infer x) { TODO("Error for llvm name of an inferred type"); },
        [&o](SizedArray x) {
          o << "[" << x.length << " x ";
          format(o, x.dereferencedType);
          o << "]";
        },
        [&o](NeverType x) { TODO("Error for llvm name for never type"); },
        [&o](IntLiteralType) { o << "i32"; },
        [&o](FloatLiteralType) { o << "float"; },
        [&o](TypeOfGeneric) {
          TODO("Error for llvm name for float literal type");
        },
        [&o](EnvironmentType) {
          TODO("Error for llvm name for float literal type");
        },
        [&o](Type) { TODO("Error for llvm name for type literal type"); },
        [&o](RangeLiteral) {
          TODO("Error for llvm name for range literal type");
        },
        [&o](AlignedType x) { format(o, x.baseType); },
        [&o, type](Union x) {
          // TODO: move to using largest type?
          // fmt::print(o, "[i8 x {}]", Types::Pool().getSizing(type).byteSize);
          format(o, x.anonymousVariants[0]);
        },
        [&o](VectorType x) {
          fmt::print(o, "<{} x ", x.length);
          format(o, x.elementType);
          o << ">";
        },
      },
      underlyingType
    );
  }

  friend std::ostream& operator<<(std::ostream& o, const LlvmName& type) {
    format(o, type.type);
    return o;
  }
};

template <> struct fmt::formatter<TypeName> : ostream_formatter {};
template <> struct fmt::formatter<LlvmName> : ostream_formatter {};
template <> struct fmt::formatter<Log2Alignment> : ostream_formatter {};

TEST_CASE("Built-in type registers") {
  SUBCASE("Float") {
    auto typeIndex = Pool()._f32;
    RegisterAssignment registers = Pool().registerStorage(typeIndex);
    CHECK_EQ(registers.length, 4);
  }
  SUBCASE("Int registers") {
    std::vector<TypeIndex> types = {
      Pool().pointerTo(Pool()._f32),
      Pool().multiPointerTo(Pool()._f32),
      Pool()._u32,
      Pool()._s16,
      Pool().sliceOf(Pool()._isize),
      Pool().sizedArrayOf(Pool()._bool, 17)
    };

    for (auto type : types) {
      RegisterAssignment registers = Pool().registerStorage(type);
      auto size = Pool().getSizing(type).byteSize;
      if (size > 16) {
        CHECK(registers.isMemory());
        continue;
      }
      // fmt::println("What {} looks like: {:#b}", TypeName(type),
      // registers.types);
      CHECK_EQ(registers.length, size);
      for (auto i = 0; i < size; i++) {
        CHECK_EQ(registers.pop(), RegisterType::Int);
      }
    }
  }
}
