#pragma once

#include "common.h"
#include <map>
#include <string_view>
#include <unordered_map>
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
      BOOL,
      INT,
      FLOAT,
      ARRAY,
      STRING,
      NATIVE_FUNCTION,
      REFERENCE,
      FUNCTION,

      // Types
      TYPE,

      // Wait until initialization to set type
      INFER,

      LAST = INFER,
      POINTER_TO,
      TUPLE,
      STRUCT,
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
    std::map<Generic, TypeIndex> generics;
    std::vector<Type> types;
    std::vector<std::string> names = {
      "void",
      "bool",
      "int",
      "float",
      "array",
      "string",
      "native_function",
      "reference",
      "function",
      "type",
      "infer",
    };
    std::map<TypeIndex, TypeIndex> pointersTo;
    std::vector<Struct> structPool;
    // TODO: enum

    public:
    TypePool(): types(NUM_INTRINSICS) {
      for (i32 i = 0; i < NUM_INTRINSICS; i++) {
        types[i] = {.type = static_cast<Intrinsic>(i)};
      }
    }

    TypeIndex addType(Type type, std::string name) {
      TypeIndex index = {(i32) types.size()};
      types.push_back(type);
      names.push_back(name);
      return index;
    }

    TypeIndex pointerTo(TypeIndex type) {
      assert(type.value <= types.size());

      if (pointersTo.count(type) > 0) {
        return pointersTo[type];
      }

      TypeIndex pointerIndex = addType(Type {.type = Intrinsic::POINTER_TO, .definition = type.value}, "^"+names[type.value]);
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
  };

  extern TypePool Pool;
}
