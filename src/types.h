#pragma once

#include "common.h"
#include "interpreter/value.h"
#include <iostream>
#include <map>
#include <optional>
#include <span>
#include <string_view>
#include <unordered_map>
#include <array>
#include <algorithm>
#include <vector>
#include <sstream>
#include <string>

struct Reference;
using Identifier = std::string_view;
using Map = std::unordered_map<Identifier, Reference *>;

struct SymbolMap {
  Map symbolValues;
  std::unordered_map<Identifier, i32> symbolIndex;

  void define(Identifier identifier, Reference* value) {
    if (symbolValues.contains(identifier)) {
      std::stringstream message;
      message << "Attempting to define already-defined symbol: " << identifier;
      throw std::invalid_argument(message.str());
    }
    
    // NEXT
    symbolValues[identifier] = value;
    symbolIndex[identifier] = symbolIndex.size();
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
      ARRAY,
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

      LAST = INFER,
      POINTER_TO,
      STRUCT,
      GENERIC
  };

  const i32 NUM_INTRINSICS = static_cast<i32>(Intrinsic::LAST) + 1;
  const i32 NUM_BUILTINS = 14;

  namespace Encodings {
    struct Struct {};
  }

  struct TypeIndex {
    i32 value;
    bool operator==(const TypeIndex &other) const {
      return value == other.value;
    }

    bool operator<(const TypeIndex &other) const {
      return value < other.value;
    }
  };

  using OptionalType = std::optional<TypeIndex>;

  constexpr TypeIndex indexOf(Intrinsic type) {
    i32 value = static_cast<i32>(type);
    assert (type <= Intrinsic::LAST);
    return {value};
  }
  
  struct DataIndex {i32 value;};
  struct StructIndex {i32 value;};
  struct GenericIndex {i32 value;};
  struct TupleIndex {i32 value;};

  struct Struct {
    SymbolMap fields;
    SymbolMap statics;

    Reference* getField(std::string_view field) {
      if (statics.isDefined(field)) {
        return statics.symbolValues[field];
      }

      return nullptr;
    }

    i32 numFields() const {
      return fields.symbolValues.size();
    }
  };

  // TODO: SoA this struct
  struct Type {
    Intrinsic type;
    std::string llvmName;
    i32 definition;
  };

  struct Generic {
    TypeIndex type;
    const TupleIndex parameters;

    bool operator==(const Generic &other) const {
      return (type == other.type) && (parameters.value == other.parameters.value);
    }

    bool operator<(const Generic &other) const {
      return type < other.type || (type == other.type && parameters.value < other.parameters.value);
    }
  };

  class TypePool {
    public:
    std::vector<Type> types;
    std::vector<std::string> names = {
      "VOID",
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
    std::map<TypeIndex, TypeIndex> pointersTo;
    std::vector<Struct> structPool;
    std::vector<std::vector<TypeIndex>> tuplePool;
    std::map<std::vector<TypeIndex>, TypeIndex> tuples;
    std::map<Generic, TypeIndex> generics;
    std::vector<Generic> genericPool;

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
        types.emplace_back(static_cast<Intrinsic>(i));
      }

      u8 = addLLVMType("i8", "u8");
      u16 = addLLVMType("i16", "u16");
      u32 = addLLVMType("i32", "u32");
      u64 = addLLVMType("i64", "u64");
      s8 = addLLVMType("i8", "s8");
      s16 = addLLVMType("i16", "s16");
      s32 = addLLVMType("i32", "s32");
      s64 = addLLVMType("i64", "s64");
      // TODO: change based on target word size
      usize = addLLVMType("i64", "usize");
      isize = addLLVMType("i64", "isize");

      f16 = addLLVMType("f16", "f16");
      f32 = addLLVMType("f32", "f32");
      f64 = addLLVMType("f64", "f64");
      boolean = addLLVMType("i1", "boolean");
    }

    TypeIndex addType(Type type, std::string name) {
      TypeIndex index = {(i32) types.size()};
      types.push_back(std::move(type));
      names.push_back(std::move(name));
      return index;
    }

    TypeIndex pointerTo(TypeIndex type) {
      assert(type.value <= types.size());

      if (pointersTo.contains(type)) {
        return pointersTo[type];
      }

      TypeIndex pointerIndex = addType(Type {.type = Intrinsic::POINTER_TO, .llvmName = "ptr", .definition = type.value}, "^"+names[type.value]);
      pointersTo[type] = pointerIndex;
      
      return pointerIndex;
    }
    
    OptionalType dereference(TypeIndex type) {
      if (!isPointer(type)) {
        return std::nullopt;
      }
      Type rawType = (*this)[type];

      return TypeIndex {rawType.definition};
    }
    
    TypeIndex addStruct(Struct structDefinition, std::string name) {
      i32 structIndex = structPool.size();
      structPool.push_back(structDefinition);
      Type type = {.type = Intrinsic::STRUCT, .definition = structIndex};
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

    Struct& getStruct(TypeIndex index) {
      Type definition = (*this)[index];
      assert(definition.type == Intrinsic::STRUCT);
      return getStruct(StructIndex{definition.definition});
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
    TypeIndex addLLVMType(std::string llvmName, std::string name) {
      Type type = {.type = Intrinsic::LLVM_TYPE, .llvmName = std::move(llvmName)};
      return addType(std::move(type), std::move(name));
    }

    std::string_view getLLVMType(TypeIndex index) {
      return std::string_view(types[index.value].llvmName);
    }

    std::optional<Reference*> getFieldIndex(TypeIndex typeIndex, std::string_view fieldName) {
      Types::Struct& structDefinition = getStruct(typeIndex);
      return structDefinition.fields.symbolValues[fieldName];
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

    TypeIndex addGeneric(Generic generic) {
      if (generics.contains(generic)) {
        return generics.at(generic);
      }

      i32 genericIndex = genericPool.size();
      genericPool.push_back(std::move(generic));
      // TODO: llvm type name
      Type typeDefinition = {.type = Intrinsic::GENERIC, .llvmName = "TODO:GENERIC", .definition = genericIndex};
      std::stringstream prettyName;
      
      prettyName << typeName(generic.type) << "<";
      bool hasMultiple = false;
      Type tupleDefinition = types[generic.parameters.value];
      assert(tupleDefinition.type == Intrinsic::TUPLE);
      for (auto typeIndex : tuplePool[tupleDefinition.definition]) {
        if (hasMultiple) {
          prettyName << ", ";
        }
        prettyName << typeName(typeIndex);
      }
      prettyName << ">";
      TypeIndex type = addType(typeDefinition, std::string(prettyName.str()));
      
      return type;
    }

    TypeIndex addGeneric(TypeIndex baseType, std::vector<TypeIndex> parameters) {
      TypeIndex tupleType = tupleOf(std::move(parameters));
      Type tupleDefinition = (*this)[tupleType];
      Generic generic = {.type = baseType, .parameters = {tupleDefinition.definition}};
      return addGeneric(std::move(generic));
    }
    
    TypeIndex tupleOf(std::vector<TypeIndex> types) {
      if (tuples.contains(types)) {
        return tuples[types];
      }

      i32 tupleIndex = (i32) tuplePool.size();
      tuplePool.push_back(types);
      // TODO: llvm name
      Type typeDefinition = {.type = Intrinsic::TUPLE, .llvmName = "", .definition = tupleIndex};

      std::stringstream prettyName;
      prettyName << "(";
      bool hasMultiple = false;
      for (auto typeIndex : types) {
        if (hasMultiple) {
          prettyName << ", ";
        }
        prettyName << typeName(typeIndex);
      }
      prettyName << ")";

      TypeIndex type = addType(typeDefinition, std::move(prettyName.str()));
      tuples[std::move(types)] = type;

      return type;
    }

    std::span<TypeIndex> tupleElements(TypeIndex type) {
      Type typeDefinition = getDefinition(type);
      assert(typeDefinition.type == Intrinsic::TUPLE);
      i32 tupleIndex = typeDefinition.definition;
      return std::span(tuplePool[tupleIndex]);
    }

    TypeIndex addFunction(std::vector<TypeIndex> parameters, TypeIndex returnType) {
      TypeIndex paramType = tupleOf(std::move(parameters));
      std::vector<TypeIndex> genericParameters = {paramType, returnType};
      return addGeneric(indexOf(Intrinsic::FUNCTION), std::move(genericParameters));
    }

    Generic getGeneric(TypeIndex genericType) {
      Type typeDefinition = types[genericType.value];
      assert(typeDefinition.type == Intrinsic::GENERIC);
      return genericPool[typeDefinition.definition];
    }

    std::pair<TypeIndex, TypeIndex> functionParamsAndReturnType(TypeIndex functionType) {
      Generic generic = getGeneric(functionType);
      assert(generic.type == indexOf(Intrinsic::FUNCTION));
      TupleIndex signature = generic.parameters;
      std::vector<TypeIndex>& signatureTypes =  tuplePool[signature.value];
      assert(signatureTypes.size() == 2);
      return {signatureTypes[0], signatureTypes[1]};
    }

    // pointers, ints, etc
    // TODO: use a more specific condition
    bool isLlvmLiteralType(TypeIndex type) {
      return getLLVMType(type)[0] != '%';
    }
  };

  TypePool& Pool();
}
