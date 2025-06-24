#pragma once

#include "common.h"
#include <iostream>
#include <map>
#include <optional>
#include <string_view>
#include <unordered_map>
#include <array>
#include <algorithm>
#include <variant>
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
      ARRAY,
      STRING,
      NATIVE_FUNCTION,
      REFERENCE,
      FUNCTION,
      LLVM_TYPE,
      LLVM_FUNCTION,

      // Types
      TYPE,

      // Wait until initialization to set type
      INFER,

      LAST = INFER,
      POINTER_TO,
      TUPLE,
      STRUCT
  };

  const i32 NUM_INTRINSICS = static_cast<i32>(Intrinsic::LAST) + 1;

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

  constexpr TypeIndex indexOf(Intrinsic type) {
    i32 value = static_cast<i32>(type);
    assert (type <= Intrinsic::LAST);
    return {value};
  }
  
  struct DataIndex {i32 value;};
  struct StructIndex {i32 value;};

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
    const std::vector<TypeIndex> parameters;

    bool operator==(const Generic &other) const {
      return (type == other.type) && (parameters == other.parameters);
    }

    bool operator<(const Generic &other) const {
      return type < other.type || (type == other.type && parameters < other.parameters);
    }
  };

  class TypePool {
    public:
    std::map<Generic, TypeIndex> generics;
    std::vector<Type> types;
    std::vector<std::string> names = {
      "VOID",
      "UNTYPED_INT",
      "UNTYPED_FLOAT",
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

    TypePool(): types(NUM_INTRINSICS) {
      for (i32 i = 0; i < NUM_INTRINSICS; i++) {
        types[i] = {.type = static_cast<Intrinsic>(i)};
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

      if (pointersTo.count(type) > 0) {
        std::cout << "Pointer already exists" << std::endl;
        return pointersTo[type];
      }

      TypeIndex pointerIndex = addType(Type {.type = Intrinsic::POINTER_TO, .llvmName = "ptr", .definition = type.value}, "^"+names[type.value]);
      pointersTo[type] = pointerIndex;
      
      return pointerIndex;
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

    using FieldType = std::variant<i32, Reference*>;
    FieldType getFieldIndex(TypeIndex typeIndex, std::string_view fieldName) {
      Types::Struct& structDefinition = getStruct(typeIndex);

      Reference* fieldValue = structDefinition.getField(fieldName);
      if (fieldValue != nullptr) {
          return fieldValue;
      }

      i32 fieldIndex = structDefinition.fields.indexOf(fieldName);
      return fieldIndex;
    }

    // TODO: function overloading???
    std::optional<TypeIndex> isAssignable(TypeIndex value, TypeIndex targetType) {
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

      bool targetIsNumeric;
      {
        static std::array<TypeIndex, 11> numericTypes = {
          u8, u16, u32, u64,
          s8, s16, s32, s64,
          f16, f32, f64,
        };
        targetIsNumeric = std::find(numericTypes.begin(), numericTypes.end(), targetType) != numericTypes.end();
      }

      bool valueIsUntypedNumeric; 
      {
        static std::array<TypeIndex, 2> numericUntyped = {
          indexOf(Intrinsic::UNTYPED_INT),
          indexOf(Intrinsic::UNTYPED_FLOAT)
        };
        valueIsUntypedNumeric = std::find(numericUntyped.begin(), numericUntyped.end(), value) != numericUntyped.end();
      }
      
      if (targetIsNumeric && valueIsUntypedNumeric) {
        return targetType;
      }
      
      return std::nullopt;
    }

    void printTypes() {
      std::cout << "Number of types: " << names.size() << "\n";
      for (auto name : names) {
          std::cout << name << "\n";
      }
      std::cout << std::endl;
    }
  };

  TypePool& Pool();
}
