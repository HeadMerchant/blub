#include "common.h"

enum class RegisterType : u8 { NONE, Memory, Int, Float };
enum class CallingConvention { C };

struct RegisterAssignment {
  using Raw = u32;
  static constexpr Raw bitWidth = 2;
  static constexpr Raw mask = (1 << bitWidth) - 1;

  // Maximum size of 16 bytes before type needs to be passed in memory
  static constexpr Raw maxLength = 16;
  Raw types;

  Raw length() const {
    u32 i = 0;
    Raw bitVec = types;
    for (; i < maxLength; i++) {
      if (!(bitVec & mask)) break;
      bitVec >>= bitWidth;
    }

    if (bitVec) return maxLength;

    return i;
  }

  void push(RegisterType type) {
    // Using NONE as a terminator
    assert(type != RegisterType::NONE);
    invariants();
    auto prev = types;

    types = types << bitWidth | (u32)type;

    assert(get(0) == type);
    assert(types >> bitWidth == prev);
    invariants();
  }

  RegisterType pop() {
    invariants();
    auto type = get(0);
    types >>= 2;
    invariants();
    return type;
  }

  RegisterType get(Raw i) const {
    assert(i < maxLength);
    auto bitVal = mask & (types >> (bitWidth * i));
    assert(bitVal <= mask);
    return (RegisterType)bitVal;
  }

  void set(Raw i, RegisterType type) {
    invariants();
    assert(type != RegisterType::NONE);
    assert(i < maxLength);
    auto shiftAmount = bitWidth * i;
    auto elementMask = mask << shiftAmount;
    auto bitVal = (Raw)type << shiftAmount;

    RegisterAssignment oldTypes(types);
    types = (types & ~elementMask) | bitVal;
    for (Raw j = 0; j < maxLength; j++) {
      if (j == i) continue;
      assert(oldTypes.get(j) == get(j));
    }
    invariants();
  }

  bool allInt() const {
    if (!types) return false;
    for (auto i = 0; i < maxLength; i++) {
      auto type = get(i);
      if (type == RegisterType::NONE) {
        break;
      }
      if (type != RegisterType::Int) {
        return false;
      }
    }
    return true;
  }

  RegisterAssignment dominateSSE(CallingConvention cc) const {
    if (types == (Raw)RegisterType::Memory || types == (Raw)RegisterType::NONE) return *this;
    // TODO: non-system v
    constexpr u32 abiByteStride = 8;
    RegisterAssignment result;
    for (u32 i = 0; i < maxLength; i += abiByteStride) {
      Raw maxValue = 0;
      Raw maxJ = 0;
      for (u32 j = 0; j < abiByteStride; j++) {
        auto masked = (Raw)this->get(i * abiByteStride + j);
        if (!masked) break;
        maxValue = std::max(maxValue, masked);
      }

      assert(maxJ <= mask);
      RegisterType type = (RegisterType)maxJ;
      for (u32 j = 0; j <= maxJ; j++) {
        result.set(i * abiByteStride + j, type);
      }
    }

    result.invariants();

    return result;
  }

  void invariants() const {
#ifdef ASSERT_INVARIANTS
    invariant_bipartiteNone();
    invariant_onlyFirstElementIsMemory();
#endif
  }

  void invariant_bipartiteNone() const {
    // Can partition into contiguous NONE and non-NONE
    for (u32 i = 0; i < maxLength; i++) {
      if (get(i) == RegisterType::NONE) {
        assert(!(types >> (i * bitWidth)));
        break;
      }
    }
  }

  void invariant_onlyFirstElementIsMemory() const {
    if (get(0) == RegisterType::Memory) {
      assert(!(types >> bitWidth));
    }

    for (u32 i = 1; i < maxLength; i++) {
      assert(get(i) != RegisterType::Memory);
    }
  }

  bool isMemory() const {
    return get(0) == RegisterType::Memory;
  }

  bool isVoid() const {
    return types == 0;
  }
};

TEST_CASE("hola im a test") {
  CHECK(true);
}
