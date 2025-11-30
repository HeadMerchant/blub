#pragma once

#include "common.h"
#include "fmt/base.h"
#include <algorithm>
#include <array>
#include <cstdint>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <fmt/ranges.h>
#include <iostream>
#include <limits>
#include <optional>
#include <queue>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

struct Reference;

struct TypeIndex {
  i32 value;
  bool operator==(const TypeIndex& other) const {
    return value == other.value;
  }

  struct Hash {
    std::size_t operator()(const TypeIndex& k) const {
      using std::hash;
      return hash<i32>()(k.value);
    }

    std::size_t operator()(const std::vector<TypeIndex>& vec) const {
      std::size_t seed = vec.size();
      for (auto& i : vec) {
        seed ^= i.value + 0x9e3779b9 + (seed << 6) + (seed >> 2);
      }
      return seed;
    }
  };

  bool isInfer();
};

namespace Types {
const i32 NUM_BUILTINS = 14;

enum class LLVMStorage { VOID, LITERAL, VARIABLE };

using OptionalType = std::optional<TypeIndex>;
using TypeSpan = std::span<TypeIndex>;

struct TypeField {
  TypeIndex type;
  i32 index;
};

struct DataIndex {
  i32 value;
};
struct StructIndex {
  i32 value;
};
struct EnumIndex {
  i32 value;
};

struct TupleIndex {
  i32 value;
  bool operator==(const TupleIndex& other) const {
    return value == other.value;
  }

  struct Hash {
    std::size_t operator()(const TupleIndex& k) const {
      return std::hash<i32>()(k.value);
    }
  };
};

struct FunctionType {
  TupleIndex parameters;
  TypeIndex returnType;
  bool operator==(const FunctionType& other) const {
    return parameters == other.parameters && returnType == other.returnType;
  }

  struct Hash {
    std::size_t operator()(const Types::FunctionType& k) const {
      using std::hash;
      return (hash<i32>()(k.parameters.value) ^ (hash<i32>()(k.returnType.value) << 1));
    }
  };
};

struct Log2Alignment {
  uint8_t value;
  static Log2Alignment fromByteSize(i32 byteSize) {
    uint8_t log2Alignment = std::numeric_limits<i32>::digits - 1 - __builtin_clz(byteSize);
    return {log2Alignment};
  }

  static Log2Alignment fromBitSize(i32 bitSize) {
    auto byteSize = 1 + (bitSize - 1) / 8;
    return fromByteSize(byteSize);
  }

  i32 byteAlignment() const {
    return 1 << value;
  }

  friend std::ostream& operator<<(std::ostream& o, const Log2Alignment& self) {
    o << self.byteAlignment();
    return o;
  }
};

struct Sizing {
  i32 byteSize;
  i32 bitSize;
  Log2Alignment alignment;

  static Sizing fromBitSize(i32 bitSize) {
    if (bitSize == 0) {
      TODO("Zero-sized structs/int sizing");
    }
    i32 byteSize = 1 + (bitSize - 1) / 8;
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
    return Sizing{.byteSize = byteSize, .bitSize = bitSize, .alignment = alignment};
  }

  static Sizing alignToPointer(i32 byteSize) {
    return Sizing{.byteSize = byteSize, .bitSize = byteSize * 8, .alignment = Log2Alignment::fromByteSize(8)};
  }
};

struct Struct {
  std::unordered_map<Identifier, TypeField> fields;
  // SymbolMap statics;
  std::string name;
  std::string llvmName;
  Sizing sizing;
  std::vector<TypeIndex> fieldTypes;

  Struct(std::string name, std::string llvmName) : name(name), llvmName(llvmName) {}

  std::optional<TypeField> getField(std::string_view fieldName) {
    if (fields.contains(fieldName)) {
      return fields[fieldName];
    }

    return std::nullopt;
  }

  bool defineField(Identifier name, TypeIndex type) {
    auto [_, success] = fields.insert({
      name, TypeField{.type = type, .index = (i32)fields.size()}
    });
    fieldTypes.push_back(type);
    if (success) {
      fmt::println("Defining field: {}", name);
    }

    return success;
  }
};

struct Enum {
  std::unordered_map<Identifier, uint64_t> values;
  std::string name;
  TypeIndex rawType;
  TypeIndex enumType;

  Enum(std::string name, TypeIndex rawType) : name(name), rawType(rawType), values() {}

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

struct Void {};
struct Infer {};
struct SignedInt {
  i32 bitSize;
};

struct UnsignedInt {
  i32 bitSize;
};
struct Float {
  enum { f16, f32, f64 } floatName;
  i32 bitSize() {
    return floatName == f16 ? 16 : floatName == f32 ? 32 : 64;
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
  i32 index;
};
struct Opaque {
  std::string name;
  std::string llvmName;
};
struct SizedArray {
  TypeIndex dereferencedType;
  i32 length;
};
struct Never {};
struct IntLiteral {};
struct FloatLiteral {};
struct Type {};
struct Environment {};
// TODO: implement params and return type
struct Generic {};
struct RangeLiteral {};
using UnderlyingType = std::variant<
  Void,
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
  Never,
  IntLiteral,
  FloatLiteral,
  Type,
  Environment,
  Generic,
  RangeLiteral>;
template <class... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};

struct Tuple {
  std::span<TypeIndex> types;
  Sizing sizing;
  std::string name;
};

class TypePool {
private:
  bool testFlag;

public:
  static TypePool pool;
  std::vector<UnderlyingType> underlyingTypes;

  std::unordered_map<TypeIndex, PointerType, TypeIndex::Hash> pointersTo;
  std::vector<Struct> structPool;
  std::vector<Enum> enumPool;

  std::vector<Tuple> tuplePool;
  std::vector<TypeIndex> tupleTypeIndices;
  std::unordered_map<std::vector<TypeIndex>, std::pair<TypeIndex, TupleIndex>, TypeIndex::Hash> tuples;
  std::unordered_map<i32, std::unordered_map<TypeIndex, TypeIndex, TypeIndex::Hash>> sizedArrays;
  // TODO: function
  std::unordered_map<FunctionType, TypeIndex, FunctionType::Hash> functionCache;

  // TODO: enum

  // LLVM types
  TypeIndex _void;
  TypeIndex _bool;
  TypeIndex u8;
  TypeIndex u16;
  TypeIndex u32;
  TypeIndex u64;
  TypeIndex s8;
  TypeIndex s16;
  TypeIndex s32;
  TypeIndex s64;
  TypeIndex f16;
  TypeIndex f32;
  TypeIndex f64;
  TypeIndex usize;
  TypeIndex isize;

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
    i32 index = underlyingTypes.size();
    underlyingTypes.push_back(type);
    return {index};
  }

  TypePool() {
    // 23 builtins initialized
    underlyingTypes.reserve(256);

    _void = addType(Void{});
    u8 = addType(UnsignedInt(8));
    u16 = addType(UnsignedInt(16));
    u32 = addType(UnsignedInt(32));
    u64 = addType(UnsignedInt(64));
    s8 = addType(SignedInt(8));
    s16 = addType(SignedInt(16));
    s32 = addType(SignedInt(32));
    s64 = addType(SignedInt(64));
    // TODO: change based on target word size
    usize = u64;
    isize = s64;

    f16 = addType(Float(Float::f16));
    f32 = addType(Float(Float::f32));
    f64 = addType(Float(Float::f64));
    _bool = addType(UnsignedInt(1));

    infer = addType(Infer{});
    never = addType(Never{});
    intLiteral = addType(IntLiteral{});
    floatLiteral = addType(FloatLiteral{});

    type = addType(Type{});
    environment = addType(Environment{});
    generic = addType(Generic{});
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
    // fmt::println("Getting type index {} of {}", type.value, underlyingTypes.size());
    return underlyingTypes[type.value];
  }

  OptionalType dereference(TypeIndex type);

  std::pair<TypeIndex, StructIndex> makeStruct(std::string name, std::string llvmName) {
    StructIndex structIndex{(i32)structPool.size()};
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
    return std::holds_alternative<Pointer>(underlying) || std::holds_alternative<MultiPointer>(underlying);
  }

  std::optional<TypeField> getFieldIndex(TypeIndex typeIndex, std::string_view fieldName) {
    auto structDefinition = getStruct(typeIndex);
    if (structDefinition) {
      return (*structDefinition)->getField(fieldName);
    }

    if (auto slice = std::get_if<Slice>(&getType(typeIndex))) {
      if (fieldName == "data") {
        return TypeField{.type = multiPointerTo(slice->dereferencedType), .index = 0};
      }
      if (fieldName == "length") {
        return TypeField{.type = usize, .index = 1};
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

    if (std::holds_alternative<Infer>(valueType) || std::holds_alternative<Void>(valueType)) {
      return std::nullopt;
    }

    if (type == intLiteral) return s32;
    if (type == floatLiteral) return f32;

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
      if (!isAssignable(value = valueElements[i], targetType = targetElements[i]).has_value()) return std::nullopt;
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

  std::pair<TypeIndex, TupleIndex> tupleOf(std::vector<TypeIndex> types, std::queue<std::string>& globals);

  TupleIndex tupleIndex(TypeIndex type) {
    if (auto tuple = std::get_if<TupleIndex>(&underlyingTypes[type.value])) {
      return *tuple;
    } else {
      TODO("Error for trying to get tuple index from type that isn't tuple");
    }
  }

  std::span<TypeIndex> tupleElements(TypeIndex type) {
    TupleIndex index = tupleIndex(type);
    return tupleElements(type);
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

  // pointers, ints, etc
  // TODO: use a more specific condition
  bool isLlvmLiteralType(TypeIndex type) {
    auto underlyingType = getType(type);
    return !(std::holds_alternative<StructIndex>(underlyingType) || std::holds_alternative<Slice>(underlyingType));
  }

  bool isVoid(TypeIndex type) {
    return std::holds_alternative<Void>(getType(type));
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
    if (auto boxed = std::get_if<SizedArray>(&getType(type))) {
      return *boxed;
    }
    return std::nullopt;
  }

  TypeIndex sizedArrayOf(TypeIndex elementType, i32 size) {
    auto sizes = sizedArrays[size];
    if (sizes.contains(elementType)) {
      return sizes[elementType];
    }

    auto index = addType(SizedArray(elementType, size));
    sizes[elementType] = index;
    return index;
  }

  // OptionalType multiPointerElementType(TypeIndex type) {
  //   Type definition = types[type.value];
  //   if (definition.type != Intrinsic::MULTI_POINTER) {
  //     return std::nullopt;
  //   }
  //   return TypeIndex{definition.definition};
  // }

  // OptionalType sizedArrayElementType(TypeIndex type) {
  //   Type definition = types[type.value];
  //   if (definition.type != Intrinsic::SIZED_ARRAY) {
  //     return std::nullopt;
  //   }
  //   return TypeIndex{definition.definition};
  // }

  LLVMStorage storageType(TypeIndex type) {
    if (isVoid(type)) return LLVMStorage::VOID;
    else if (isLlvmLiteralType(type)) return LLVMStorage::LITERAL;
    else return LLVMStorage::VARIABLE;
  }

  TypeIndex addOpaque(std::string name) {
    return addType(Opaque(name));
  }

  std::pair<TypeIndex, EnumIndex> addEnum(Enum enumDefinition) {
    EnumIndex enumIndex{(i32)enumPool.size()};
    enumPool.push_back(enumDefinition);
    return {addType(enumIndex), enumIndex};
  }

  std::pair<TypeIndex, EnumIndex> addEnum(TypeIndex rawType, std::string name) {
    return addEnum(Enum(name, std::move(rawType)));
  }

  std::optional<Enum*> getEnum(TypeIndex type) {
    if (auto enumIndex = std::get_if<EnumIndex>(&getType(type))) {
      return &getEnum(*enumIndex);
    }
    return std::nullopt;
  }

  Enum& getEnum(EnumIndex type) {
    return enumPool[type.value];
  }

  bool isFloat(TypeIndex type) {
    return std::holds_alternative<Float>(getType(type));
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
    return isSignedInt(type) || isUnsignedInt(type) || isAny<IntLiteral>(getType(type));
  }

  bool isInfer(TypeIndex type) {
    return std::holds_alternative<Infer>(getType(type));
  }

  void defineLLVMStruct(StructIndex structDefinition, std::queue<std::string>& globals);

  Sizing getSizing(TypeIndex type) {
    return std::visit(
      overloaded{
        [](Void x) { return Sizing{0, 0}; },
        [](SignedInt x) { return Sizing::fromBitSize(x.bitSize); },
        [](UnsignedInt x) { return Sizing::fromBitSize(x.bitSize); },
        [](Float x) { return Sizing::fromBitSize(x.bitSize()); },
        [](Pointer x) { return Sizing::fromBitSize(64); },
        [](MultiPointer x) { return Sizing::fromBitSize(64); },
        [](Slice x) { return Sizing::alignToPointer(2 * 8); },
        [this](StructIndex x) { return structPool[x.value].sizing; },
        [this](TupleIndex x) { return tuplePool[x.value].sizing; },
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
          return Sizing{.byteSize = length * sizing.byteSize, .bitSize = length * sizing.bitSize, .alignment = sizing.alignment};
        },
        [](Never) {
          TODO("Error for sizing Never type");
          return Sizing{};
        },
        [](IntLiteral) {
          TODO("Error for sizing IntLiteral type");
          return Sizing{};
        },
        [](FloatLiteral) {
          TODO("Error for sizing FloatLiteral type");
          return Sizing{};
        },
        [](Type) {
          TODO("Error for sizing Type type");
          return Sizing{};
        },
        [](Environment) {
          TODO("Error for sizing Environment type");
          return Sizing{};
        },
        [](Generic) {
          TODO("Error for sizing Generic type");
          return Sizing{};
        },
        [](RangeLiteral) {
          TODO("Error for sizing Range type");
          return Sizing{};
        }},
      getType(type));
  }

  Sizing getSizing(TypeSpan types) {
    i32 structSize = 0;
    i32 structAlignment = 1;

    auto alignTo = [](i32 size, i32 alignment) { return (size + alignment - 1) & ~(alignment - 1); };

    for (auto field : types) {
      auto fieldSizing = getSizing(field);

      auto fieldAlignment = fieldSizing.alignment.byteAlignment();
      structSize = alignTo(structSize, fieldAlignment);
      structSize += fieldSizing.byteSize;
      structAlignment = std::max(structAlignment, fieldAlignment);
    }

    structSize = alignTo(structSize, structAlignment);
    return Sizing{.byteSize = structSize, .bitSize = structSize * 8, .alignment = Log2Alignment::fromByteSize(structAlignment)};
  }

  OptionalType coerce(TypeIndex a, TypeIndex b) {
    if (a == b) {
      return a;
    }

    auto aType = getType(a);
    auto bType = getType(b);
    if (subTypes(aType, bType)) return b;
    if (subTypes(bType, aType)) return a;

    return std::nullopt;
  }

  bool subTypes(UnderlyingType& child, UnderlyingType& parent) {
    if (isAny<Infer>(child)) return true;
    if (isAny<Never>(child)) return !isAny<Infer>(parent);
    if (isAny<IntLiteral>(child)) {
      return isAny<SignedInt, UnsignedInt, FloatLiteral, Float>(parent);
    }
    if (isAny<FloatLiteral>(child)) {
      return isAny<Float>(parent);
    }
    return false;
  }

  void setStructSizing(StructIndex type) {
    Struct& structDefinition = getStruct(type);
    std::vector<TypeIndex> fieldTypes;
    for (auto [_, field] : structDefinition.fields) {
      fieldTypes.push_back(field.type);
    }
    structDefinition.sizing = getSizing(TypeSpan(fieldTypes));
  }

  void debugTypes();
};

TypePool& Pool();

} // namespace Types

struct TypeName {
  TypeIndex type;

  static void print(std::ostream& o, TypeIndex type) {
    std::visit(
      Types::overloaded{
        [&o](Types::Void x) { o << "void"; },
        [&o](Types::SignedInt x) { o << "s" << x.bitSize; },
        [&o](Types::UnsignedInt x) { o << "u" << x.bitSize; },
        [&o](Types::Float x) { o << "f" << x.bitSize(); },
        [&o](Types::Pointer x) {
          o << "^";
          print(o, x.dereferencedType);
        },
        [&o](Types::MultiPointer x) {
          o << "[^]";
          print(o, x.dereferencedType);
        },
        [&o](Types::Slice x) {
          o << "[]";
          print(o, x.dereferencedType);
        },
        [&o](Types::StructIndex x) { o << Types::Pool().structPool[x.value].name; },
        [&o](Types::TupleIndex x) { o << Types::Pool().tuplePool[x.value].name; },
        [&o](Types::EnumIndex x) { print(o, Types::Pool().enumPool[x.value].rawType); },
        [&o](Types::FunctionType x) {
          o << Types::Pool().tuplePool[x.parameters.value].name << " -> ";
          print(o, x.returnType);
        },
        [&o](Types::Infer x) { o << "infer"; },
        [&o](Types::Opaque x) { o << x.name; },
        [&o](Types::SizedArray x) {
          o << "[" << x.length << "]";
          print(o, x.dereferencedType);
        },
        [&o](Types::Never x) { o << "never"; },
        [&o](Types::IntLiteral) { o << "int literal"; },
        [&o](Types::FloatLiteral) { o << "float literal"; },
        [&o](Types::Generic) { o << "generic"; },
        [&o](Types::Environment) { o << "environment"; },
        [&o](Types::Type) { o << "type"; },
        [&o](Types::RangeLiteral) { o << "range"; }},
      Types::Pool().getType(type));
  }

  friend std::ostream& operator<<(std::ostream& o, const TypeName& type) {
    print(o, type.type);
    return o;
  }
};

struct LlvmName {
  TypeIndex type;
  static void format(std::ostream& o, TypeIndex type) {
    std::visit(
      Types::overloaded{
        [&o](Types::Void x) { o << "void"; },
        [&o](Types::SignedInt x) { o << "i" << x.bitSize; },
        [&o](Types::UnsignedInt x) { o << "i" << x.bitSize; },
        [&o](Types::Float x) { o << (x.floatName == Types::Float::f16 ? "half" : (x.floatName == Types::Float::f32 ? "float" : "double")); },
        [&o](Types::Pointer x) { o << "ptr"; },
        [&o](Types::MultiPointer x) { o << "ptr"; },
        [&o](Types::Slice x) { o << "%.slice"; },
        [&o](Types::StructIndex x) { o << Types::Pool().structPool[x.value].llvmName; },
        [&o](Types::TupleIndex x) { o << "%.tuple." << x.value; },
        [&o](Types::EnumIndex x) { format(o, Types::Pool().enumPool[x.value].rawType); },
        [&o](Types::FunctionType x) { o << "ptr"; },
        [&o](Types::Opaque x) { o << x.llvmName; },
        [&o](Types::Infer x) { TODO("Error for llvm name of an inferred type"); },
        [&o](Types::SizedArray x) {
          o << "[" << x.length << " x ";
          format(o, x.dereferencedType);
          o << "]";
        },
        [&o](Types::Never x) { TODO("Error for llvm name for never type"); },
        [&o](Types::IntLiteral) { o << "i32"; },
        [&o](Types::FloatLiteral) { o << "float"; },
        [&o](Types::Generic) { TODO("Error for llvm name for float literal type"); },
        [&o](Types::Environment) { TODO("Error for llvm name for float literal type"); },
        [&o](Types::Type) { TODO("Error for llvm name for type literal type"); },
        [&o](Types::RangeLiteral) { TODO("Error for llvm name for range literal type"); }},
      Types::Pool().getType(type));
  }

  friend std::ostream& operator<<(std::ostream& o, const LlvmName& type) {
    format(o, type.type);
    return o;
  }
};

template <> struct fmt::formatter<TypeName> : ostream_formatter {};
template <> struct fmt::formatter<LlvmName> : ostream_formatter {};
template <> struct fmt::formatter<Types::Log2Alignment> : ostream_formatter {};
