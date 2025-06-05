#pragma once

#include "parser/parser.h"
#include <map>

namespace Types {
  enum class Intrinsic {
      BOOL = 0,
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
      POINTER_TO
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

  // TODO: SoA this struct
  struct Type {
    Intrinsic type;
    TypeIndex definition;
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
    std::map<TypeIndex, TypeIndex> pointersTo;

    public:
    TypePool(): types(NUM_INTRINSICS) {
      for (i32 i = 0; i < NUM_INTRINSICS; i++) {
        types[i] = {.type = static_cast<Intrinsic>(i)};
      }
    }

    TypeIndex addType(Type type) {
      TypeIndex index = {(i32) types.size()};
      types.push_back(type);
      return index;
    }

    TypeIndex pointerTo(TypeIndex type) {
      assert(type.value <= types.size());

      if (pointersTo.count(type) > 0) {
        return pointersTo[type];
      }

      TypeIndex pointerIndex = addType(Type {.type = Intrinsic::POINTER_TO, .definition = type});
      pointersTo[type] = pointerIndex;
      
      return pointerIndex;
    }
    
    Type& operator[](TypeIndex index) {
      return types[index.value];
    }
  };

  extern TypePool Pool;
}
