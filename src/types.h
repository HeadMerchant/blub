#pragma once

#include "common.h"
#include <fmt/format.h>
#include <fmt/ranges.h>
#include <iostream>
#include <optional>
#include <ranges>
#include <span>
#include <string_view>
#include <unordered_map>
#include <array>
#include <algorithm>
#include <utility>
#include <vector>
#include <sstream>
#include <string>

struct Reference;
using Identifier = std::string_view;
using Map = std::unordered_map<Identifier, Reference *>;

struct SymbolMap {
  Map symbolValues;
  std::unordered_map<Identifier, i32> symbolIndex;
  std::vector<Identifier> insertionOrder;

  void define(Identifier identifier, Reference* value) {
    if (symbolValues.contains(identifier)) {
      std::stringstream message;
      message << "Attempting to define already-defined symbol: " << identifier;
      throw std::invalid_argument(message.str());
    }
    
    // NEXT
    symbolValues[identifier] = value;
    symbolIndex[identifier] = symbolIndex.size();
    insertionOrder.push_back(identifier);
  }

  bool isDefined(Identifier identifier) {
    return symbolValues.contains(identifier);
  }

  int indexOf(Identifier identifier) {
    if (!isDefined(identifier)) {
      std::stringstream ss;
      ss << "Undefined field name " << identifier;
      throw std::invalid_argument(ss.str());
    }
    return symbolIndex[identifier];
  }
};

namespace Types {
  enum class Intrinsic {
      VOID = 0,
      UNTYPED_INT,
      UNTYPED_FLOAT,
      TUPLE,
      STRING,
      NATIVE_FUNCTION,
      REFERENCE,
      FUNCTION,
      FUNCTION_TABLE,
      LLVM_TYPE,
      LLVM_FUNCTION,

      // Types
      TYPE,

      // Wait until initialization to set type
      INFER,
      OPAQUE,

      LAST = INFER,
      POINTER_TO,
      // TODO: fixed-size array
      SIZED_ARRAY,
      SLICE,
      MULTI_POINTER,
      STRUCT,
      GENERIC
  };

  const i32 NUM_INTRINSICS = static_cast<i32>(Intrinsic::LAST) + 1;
  const i32 NUM_BUILTINS = 14;

  namespace Encodings {
    struct Struct {};
  }

  enum class LLVMStorage {
    VOID,
    LITERAL,
    VARIABLE
  };

  struct TypeIndex {
    i32 value;
    bool operator==(const TypeIndex &other) const {
      return value == other.value;
    }
 
    struct Hash {
      std::size_t operator()(const Types::TypeIndex& k) const {
        using std::hash;
        return hash<i32>()(k.value);
      }

      std::size_t operator()(const std::vector<Types::TypeIndex>& vec) const {
        std::size_t seed = vec.size();
        for(auto& i : vec) {
          seed ^= i.value + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        }
        return seed;
      }
    };
  };

  using OptionalType = std::optional<TypeIndex>;

  constexpr TypeIndex indexOf(Intrinsic type) {
    i32 value = static_cast<i32>(type);
    assert (type <= Intrinsic::LAST);
    return {value};
  }
  
  struct DataIndex {i32 value;};
  struct StructIndex {i32 value;};

  struct TupleIndex {
    i32 value;
    bool operator==(const TupleIndex &other) const {
      return value == other.value;
    }
  };

  struct FunctionType {
    TupleIndex parameters;
    TypeIndex returnType;
    bool operator==(const FunctionType &other) const {
      return parameters == other.parameters && returnType == other.returnType;
    }

    struct Hash {
      std::size_t operator()(const Types::FunctionType& k) const {
        using std::hash;
        return (hash<i32>()(k.parameters.value)
                 ^ (hash<i32>()(k.returnType.value) << 1));
      }
    };
  };

  struct Struct {
    struct StructField {
      Types::TypeIndex type;
      Identifier name;
    };
    
    std::vector<StructField> fields; 
    std::unordered_map<Identifier, i32> fieldNames;
    SymbolMap statics;

    std::optional<std::pair<StructField*, i32>> getField(std::string_view fieldName) {
      if (fieldNames.contains(fieldName)) {
        i32 index = fieldNames[fieldName];
        return std::make_pair(&fields[fieldNames[fieldName]], index);
      }

      return std::nullopt;
    }

    bool defineField(Identifier name, TypeIndex type) {
      if (fieldNames.contains(name)) {
        return false;
      }

      fields.push_back({.type = type, .name = name});
      fieldNames[name] = fieldNames.size();
      return true;
    }
  };

  struct PointerType {
    TypeIndex pointer;
    TypeIndex slice;
    TypeIndex multiPointer;
  };
  
  // TODO: SoA this struct
  struct Type {
    Intrinsic type;
    std::string llvmName;
    i32 definition;
    i32 size;
    // TODO: reduce to 4? values - 1, 2, 4, 8
    i32 alignment;
    i32 length;
  };

  class TypePool {
    public:
    static TypePool pool;
    std::vector<Type> types;
    std::vector<std::string> names = {
      "void",
      "UNTYPED_INT",
      "UNTYPED_FLOAT",
      "TUPLE",
      "ARRAY",
      "STRING",
      "NATIVE_FUNCTION",
      "REFERENCE",
      "FUNCTION",
      "LLVM_TYPE",
      "LLVM_FUNCTION",
      "TYPE",
      "INFER",
    };
    
    std::unordered_map<TypeIndex, PointerType, TypeIndex::Hash> pointersTo;
    std::vector<Struct> structPool;
    std::vector<std::vector<TypeIndex>> tuplePool;
    std::vector<TypeIndex> tupleTypeIndices;
    std::unordered_map<std::vector<TypeIndex>, TypeIndex, TypeIndex::Hash> tuples;
    std::unordered_map<i32, std::unordered_map<TypeIndex, TypeIndex, TypeIndex::Hash>> sizedArrays;
    // TODO: function
    std::unordered_map<FunctionType, TypeIndex, FunctionType::Hash> functionCache;
    std::vector<FunctionType> functionTypePool;

    // TODO: enum

    // LLVM types
    TypeIndex boolean;
    TypeIndex u8;
    TypeIndex u16;
    TypeIndex u32;
    TypeIndex u64;
    // TODO: distinct types for signed vs signed
    TypeIndex s8;
    TypeIndex s16;
    TypeIndex s32;
    TypeIndex s64;
    TypeIndex f16;
    TypeIndex f32;
    TypeIndex f64;
    TypeIndex usize;
    TypeIndex isize;

    TypePool(): types() {
      types.reserve(NUM_INTRINSICS + NUM_BUILTINS);
      for (i32 i = 0; i < NUM_INTRINSICS; i++) {
        types.push_back({.type = static_cast<Intrinsic>(i), .llvmName = "void"});
      }

      u8 = addLLVMType("i8", "u8", 1, 1);
      u16 = addLLVMType("i16", "u16", 2, 2);
      u32 = addLLVMType("i32", "u32", 4, 4);
      u64 = addLLVMType("i64", "u64", 8, 8);
      s8 = addLLVMType("i8", "s8", 1, 1);
      s16 = addLLVMType("i16", "s16", 2, 2);
      s32 = addLLVMType("i32", "s32", 4, 4);
      s64 = addLLVMType("i64", "s64", 8, 8);
      // TODO: change based on target word size
      usize = addLLVMType("i64", "usize", 8, 8);
      isize = addLLVMType("i64", "isize", 8, 8);

      f16 = addLLVMType("half", "f16", 2, 2);
      f32 = addLLVMType("float", "f32", 4, 4);
      f64 = addLLVMType("double", "f64", 8, 8);
      boolean = addLLVMType("i1", "boolean", 1, 1);
      fmt::println("Initializing s32: {}", s32.value);
    }

    TypeIndex addType(Type type, std::string name) {
      TypeIndex index = {(i32) types.size()};
      types.push_back(std::move(type));
      names.push_back(std::move(name));
      return index;
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

    TypeIndex sizedArrayOf(TypeIndex type, i32 length) {
      // TODO: consider hash function for TypeIndex
      if(sizedArrays[length].contains(type)) return sizedArrays[length][type];
      

      TypeIndex arrayType = addType(Type {
          .type = Intrinsic::SIZED_ARRAY,
          .llvmName = fmt::format("[{} x {}]", length, getLLVMType(type)),
          .definition = type.value,
          .size = types[type.value].size*length,
          .alignment = types[type.value].alignment,
        },
        fmt::format("[{}]{}", length, typeName(type))
      );
      sizedArrays[length][type] = arrayType;
      return arrayType;
    }

    PointerType pointerTypesFor(TypeIndex type) {
      assert(type.value < types.size());

      if (pointersTo.contains(type)) {
        return pointersTo[type];
      }

      PointerType pointers = {
        .pointer = addType(Type {.type = Intrinsic::POINTER_TO, .llvmName = "ptr", .definition = type.value, .size = 8, .alignment = 8}, "^"+names[type.value]),
        // TODO: 32-bit mode
        .slice = addType(Type {.type = Intrinsic::SLICE, .llvmName = "{ptr, i64}", .definition = type.value, .size = 16, .alignment = 8}, "[]"+names[type.value]),
        .multiPointer = addType(Type {.type = Intrinsic::MULTI_POINTER, .llvmName = "ptr", .definition = type.value, .size = 8, .alignment = 8}, "[^]"+names[type.value]),
      };
      pointersTo[type] = pointers;

      return pointers;
    }
    
    OptionalType dereference(TypeIndex type) {
      if (!isPointer(type)) {
        return std::nullopt;
      }
      Type rawType = (*this)[type];

      return TypeIndex {rawType.definition};
    }
    
    TypeIndex addStruct(Struct structDefinition, std::string name, std::string llvmName) {
      i32 structIndex = structPool.size();
      structPool.push_back(structDefinition);
      Type type = {.type = Intrinsic::STRUCT, .llvmName = llvmName, .definition = structIndex};
      return addType(type, name);
    }

    bool setTypeName(TypeIndex type, std::string name) {
      if (names[type.value] != "") return false;
      names[type.value] = name;
      return true;
    }
    
    Type& operator[](TypeIndex index) {
      return types[index.value];
    }

    Struct& getStruct(StructIndex index) {
      return structPool[index.value];
    }

    std::optional<Struct*> getStruct(TypeIndex index) {
      Type definition = (*this)[index];
      if (definition.type == Intrinsic::STRUCT) {
        return &getStruct(StructIndex{definition.definition});
      }

      return std::nullopt;
    }

    bool isPointer(TypeIndex index) {
      return types[index.value].type == Intrinsic::POINTER_TO;
    }

    bool isStruct(TypeIndex index) {
      return types[index.value].type == Intrinsic::STRUCT;
    }
    
    Type getDefinition(TypeIndex index) {
      return types[index.value];
    }

    std::string& typeName(TypeIndex index) {
      return names[index.value];
    }

    // TODO: intrinsic llvm types
    TypeIndex addLLVMType(std::string llvmName, std::string name, i32 size, i32 alignment) {
      Type type = {.type = Intrinsic::LLVM_TYPE, .llvmName = std::move(llvmName), .size = size, .alignment = alignment};
      return addType(std::move(type), std::move(name));
    }

    std::string_view getLLVMType(TypeIndex index) {
      return std::string_view(types[index.value].llvmName);
    }

    std::optional<std::pair<Struct::StructField*, i32>> getFieldIndex(TypeIndex typeIndex, std::string_view fieldName) {
      auto structDefinition = getStruct(typeIndex);
      if (structDefinition) {
        return (*structDefinition)->getField(fieldName);
      }

      return std::nullopt;
    }

    // TODO: function overloading???
    // TODO: support nested tuples
    OptionalType isAssignable(TypeIndex value, TypeIndex targetType) {
      bool inferredAssignment = targetType == indexOf(Intrinsic::INFER);

      if (value == indexOf(Intrinsic::INFER) || value == indexOf(Intrinsic::VOID)) {
        return std::nullopt;
      }

      if (value == targetType) {
        return value;
      }

      if (inferredAssignment) {
        // Convert untyped to typed
        if (value == indexOf(Intrinsic::UNTYPED_INT)) {
          return s32;
        }
        if (value == indexOf(Intrinsic::UNTYPED_FLOAT)) {
          return f32;
        }
        
        return value;
      }

      bool targetIsNumeric = isNumeric(targetType) && !isUntypedNumeric(targetType);
      bool valueIsUntypedNumeric = isUntypedNumeric(value);
      
      if (targetIsNumeric && valueIsUntypedNumeric) {
        return targetType;
      }
      
      return std::nullopt;
    }

    OptionalType coerce(TypeIndex a, TypeIndex b) {
      assert(a != indexOf(Intrinsic::INFER) && b != indexOf(Intrinsic::INFER));
      if (a == b) {
        return a;
      }

      bool aUntyped = isUntypedNumeric(a);
      bool bUntyped = isUntypedNumeric(b);
      if (!(aUntyped || bUntyped)) {
        return std::nullopt;
      }

      if (aUntyped && bUntyped) {
        if (a == indexOf(Intrinsic::UNTYPED_FLOAT) || b == indexOf(Intrinsic::UNTYPED_FLOAT)) return indexOf(Intrinsic::UNTYPED_FLOAT);
        return indexOf(Intrinsic::UNTYPED_INT);
      }

      if (aUntyped) {
        if (isTypedFloat(b) || a == indexOf(Intrinsic::UNTYPED_INT)) return b;
        // Can't coerce float to int
        return std::nullopt;
      }
      if (isTypedFloat(a) || b == indexOf(Intrinsic::UNTYPED_INT)) return a;
      return std::nullopt;
    }

    bool isNumeric(TypeIndex type) {
      static std::array<TypeIndex, 11> numericTypes = {
        u8, u16, u32, u64,
        s8, s16, s32, s64,
        f16, f32, f64,
      };
      return std::find(numericTypes.begin(), numericTypes.end(), type) != numericTypes.end();
    }

    bool isFloat(TypeIndex type) {
      return type == f16 || type == f32 || type == f64 || type == indexOf(Intrinsic::UNTYPED_FLOAT);
    }

    bool isInt(TypeIndex type) {
      static std::array<TypeIndex, 9> numericTypes = {
        u8, u16, u32, u64,
        s8, s16, s32, s64,
        indexOf(Intrinsic::UNTYPED_INT)
      };
      return std::find(numericTypes.begin(), numericTypes.end(), type) != numericTypes.end();
    }

    bool isSignedInt(TypeIndex type) {
      static std::array<TypeIndex, 5> numericTypes = {
        s8, s16, s32, s64,
        indexOf(Intrinsic::UNTYPED_INT)
      };
      return std::find(numericTypes.begin(), numericTypes.end(), type) != numericTypes.end();
    }
    
    bool isTypedFloat(TypeIndex type) {
      static std::array<TypeIndex, 11> numericTypes = {
        f16, f32, f64,
      };
      return std::find(numericTypes.begin(), numericTypes.end(), type) != numericTypes.end();
    }

    bool isUntypedNumeric(TypeIndex type) {
      return (type == indexOf(Intrinsic::UNTYPED_INT)) || (type == indexOf(Intrinsic::UNTYPED_FLOAT));
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
            ).has_value()) return std::nullopt;
      }

      return targetType;
    }
    
    void printTypes() {
      std::cout << "Number of types: " << names.size() << "\n";
      for (auto name : names) {
          std::cout << name << "\n";
      }
      std::cout << std::endl;
    }

    TypeIndex tupleOf(std::vector<TypeIndex> types) {
      if (tuples.contains(types)) {
        return tuples[types];
      }

      i32 tupleIndex = (i32) tuplePool.size();
      tuplePool.push_back(types);
      // TODO: llvm name
      auto llvmNames = types | std::views::transform([this](const TypeIndex x) {return getLLVMType(x);});
      Type typeDefinition = {
        .type = Intrinsic::TUPLE,
        .llvmName = fmt::format("\{{}}", fmt::join(llvmNames, ", ")),
        .definition = tupleIndex
      };

      auto typeNames = types | std::views::transform([this](const TypeIndex x) {return typeName(x);});
      
      TypeIndex type = addType(
        typeDefinition,
        fmt::format("{}", fmt::join(typeNames, ", "))
      );
      tuples[std::move(types)] = type;
      tupleTypeIndices.push_back(type);

      return type;
    }

    TupleIndex tupleIndex(TypeIndex type) {
      Type typeDefinition = getDefinition(type);
      assert(typeDefinition.type == Intrinsic::TUPLE);
      i32 tupleIndex = typeDefinition.definition;
      return TupleIndex{tupleIndex};
    }

    std::span<TypeIndex> tupleElements(TypeIndex type) {
      TupleIndex index = tupleIndex(type);
      return tupleElements(type);
    }

    std::span<TypeIndex> tupleElements(TupleIndex type) {
      return std::span(tuplePool[type.value]);
    }

    TypeIndex addFunction(FunctionType type) {
      if (functionCache.contains(type)) return functionCache[type];

      // TODO: copy pointer sizes
      TypeIndex typeIndex = addType(
        {.type = Intrinsic::FUNCTION, .llvmName = "ptr", .definition = (i32) functionTypePool.size()},
        fmt::format("({}) -> {}", typeName(tupleTypeIndices[type.parameters.value]), typeName(type.returnType))
      );
      functionTypePool.push_back(type);
      functionCache[type] = typeIndex;

      return typeIndex;
    }

    std::optional<FunctionType> functionType(TypeIndex type) {
      Type typeDefinition = getDefinition(type);
      if (typeDefinition.type == Intrinsic::FUNCTION) {
        return functionTypePool[typeDefinition.definition];
      }

      return std::nullopt;
    }

    // pointers, ints, etc
    // TODO: use a more specific condition
    bool isLlvmLiteralType(TypeIndex type) {
      return types[type.value].type != Intrinsic::VOID && getLLVMType(type)[0] != '%';
    }

    bool isVoid(TypeIndex type) {
      return types[type.value].type == Intrinsic::VOID;
    }

    OptionalType sliceElementType(TypeIndex type) {
      Type definition = types[type.value];
      if (definition.type != Intrinsic::SLICE) {
        return std::nullopt;
      }
      return Types::TypeIndex{definition.definition};
    }

    OptionalType multiPointerElementType(TypeIndex type) {
      Type definition = types[type.value];
      if (definition.type != Intrinsic::MULTI_POINTER) {
        return std::nullopt;
      }
      return Types::TypeIndex{definition.definition};
    }

    OptionalType sizedArrayElementType(TypeIndex type) {
      Type definition = types[type.value];
      if (definition.type != Intrinsic::SIZED_ARRAY) {
        return std::nullopt;
      }
      return Types::TypeIndex{definition.definition};
    }

    LLVMStorage storageType(TypeIndex type) {
      if (isVoid(type)) return LLVMStorage::VOID;
      else if (isLlvmLiteralType(type)) return LLVMStorage::LITERAL;
      else return LLVMStorage::VARIABLE;
    }

    TypeIndex addOpaque(std::string name, std::string llvmName) {
      return addType({.type = Intrinsic::OPAQUE, .llvmName = llvmName}, name);
    }
  };

  TypePool& Pool();
}
