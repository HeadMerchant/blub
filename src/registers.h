#pragma once
#include "common.h"

enum class RegisterType : u8 {
  NONE,
  Float,
  Int,
  Memory,
};
enum class CallingConvention { C };

struct RegisterAssignment {
  using Raw = u32;
  static constexpr Raw bitWidth = 2;
  static constexpr Raw mask = (1 << bitWidth) - 1;

  // Maximum size of 16 bytes before type needs to be passed in memory
  static constexpr Raw maxLength = 16;
  Raw types = 0;
  u8 length = 0;
  u8 readIndex = 0;

#ifdef ASSERT_INVARIANTS
  Raw calcLength() const {
    Raw bitVec = types;
    for (u32 i = 0; i < maxLength; i++) {
      if (!(bitVec & mask)) return i;
      bitVec >>= bitWidth;
    }

    return maxLength;
  }
#endif

  void push(RegisterType type, u8 repeat = 1) {
    // Using NONE as a terminator
    assert(type != RegisterType::NONE);
    assert(repeat > 0);
#ifdef ASSERT_INVARIANTS
    invariants();
    auto prev = types;
#endif
    Raw bits = (Raw)type;
    for (u32 i = bitWidth; i < 32; i *= 2) {
      bits |= bits << i;
      // fmt::println("{:#x}", bits);
    }
    Raw bitMask = bitWidth * (Raw)repeat;
    bitMask = bitMask >= 32 ? 0 : (1 << bitMask);
    bitMask -= 1;
    bits = bits & bitMask;

    types = types | (bits << (bitWidth * length));
    length += repeat;

#ifdef ASSERT_INVARIANTS
    for (auto i = 1; i <= repeat; i++) {
      assert(get(length - i) == type);
    }
    Raw testMask = 1 << (bitWidth * (length - repeat));
    testMask -= 1;
    assert((types & testMask) == prev);
    invariants();
#endif
  }

private:
  RegisterType get(Raw i) const {
    auto bitVal = mask & (types >> (bitWidth * i));
    assert(bitVal <= mask);
    return (RegisterType)bitVal;
  }

public:
  RegisterType typeAt(char i = 0) const {
    assert(readIndex + i >= 0);
    if (i < length) {
      return get(readIndex + i);
    }
    return RegisterType::NONE;
  }

  RegisterType pop(Raw count = 1) {
    assert(count <= length - readIndex);
    assert(count > 0);
    auto readFrom = types >> (bitWidth * readIndex);
    auto result = (RegisterType)(readFrom & mask);
#ifdef ASSERT_INVARIANTS
    for (u32 i = 1; i < count; i++) {
      readFrom >>= bitWidth;
      assert(result == (RegisterType)(readFrom & mask));
    }
#endif
    readIndex += count;
    return result;
  }

private:
  void set(Raw i, RegisterType type) {
    assert(i < maxLength);
    auto shiftAmount = bitWidth * i;
    auto elementMask = mask << shiftAmount;
    auto bitVal = (Raw)type << shiftAmount;

    RegisterAssignment oldTypes(types);
    types = (types & ~elementMask) | bitVal;
#ifdef ASSERT_INVARIANTS
    for (Raw j = 0; j < maxLength; j++) {
      if (j == i) continue;
      assert(oldTypes.get(j) == get(j));
    }
#endif
  }

public:
  bool allInt(u8 endIndex = maxLength) const {
    if (!types) return false;
    for (auto i = readIndex; i < endIndex; i++) {
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

  RegisterAssignment dominateSSE(CallingConvention cc = CallingConvention::C) const {
    if (types == (Raw)RegisterType::Memory || types == (Raw)RegisterType::NONE) return *this;
    // TODO: non-system v
    constexpr u32 abiByteStride = 8;
    RegisterAssignment result;

    // i = 0, 8
    for (u32 i = 0; i < maxLength; i += abiByteStride) {
      Raw maxValue = 0;
      for (u32 j = i; j < i + abiByteStride; j++) {
        auto masked = (Raw)get(j);
        if (!masked) break;
        maxValue = std::max(maxValue, masked);
      }

      assert(maxValue <= mask);
      RegisterType type = (RegisterType)maxValue;
      if (type == RegisterType::NONE) continue;
      for (u32 j = i; j < i + abiByteStride; j++) {
        if (get(j) == RegisterType::NONE) break;
        result.set(j, type);
      }
    }
    result.length = length;
    result.invariants();

    return result;
  }

  void invariants() const {
#ifdef ASSERT_INVARIANTS
    invariant_onlyFirstElementIsMemory();
    invariant_lengthPartitionsNone();
#endif
  }

  void invariant_onlyFirstElementIsMemory() const {
    if (get(0) == RegisterType::Memory) {
      assert(!(types >> bitWidth));
    }

    for (u32 i = 1; i < maxLength; i++) {
      assert(get(i) != RegisterType::Memory);
    }
  }

  void invariant_lengthPartitionsNone() const {
    if (length == 16) return;
    assert(!(types >> (length * bitWidth)));
    for (u32 i = 0; i < length; i++) {
      assert(get(i) != RegisterType::NONE);
    }
  }

  bool isMemory() const {
    return get(0) == RegisterType::Memory;
  }

  bool isVoid() const {
    return types == 0;
  }
};

TEST_CASE("Registers start as NONE") {
  RegisterAssignment registers;
  CHECK_EQ(registers.length, 0);
}

TEST_CASE("Length tests") {
  SUBCASE("Positive length") {
    RegisterAssignment registers;
    registers.push(RegisterType::Int, 3);
    registers.push(RegisterType::Float, 1);
    CHECK_EQ(registers.length, 4);
  }

  SUBCASE("Max length") {
    RegisterAssignment registers;
    registers.push(RegisterType::Int, 16);
    CHECK_EQ(registers.length, 16);
  }
}

TEST_CASE("Passing in memory") {
  SUBCASE("At most one 'memory' entry") {
    RegisterAssignment registers;
    registers.push(RegisterType::Memory);
    CHECK_EQ(registers.typeAt(), RegisterType::Memory);
    CHECK_THROWS(registers.push(RegisterType::Memory));
  }
  SUBCASE("Memory not allowed after another type") {
    RegisterAssignment registers;
    registers.push(RegisterType::Float);
    CHECK_EQ(registers.typeAt(), RegisterType::Float);
    CHECK_THROWS(registers.push(RegisterType::Memory));
  }
  SUBCASE("Can't push repeated 'Memory'") {
    RegisterAssignment registers;
    CHECK_THROWS(registers.push(RegisterType::Memory, 3));
  }
}

TEST_CASE("Dominate SSE") {
  SUBCASE("Trivially dominate") {
    RegisterAssignment sse;
    sse.push(RegisterType::Float);
    sse.push(RegisterType::Float);
    CHECK_EQ(sse.typeAt(0), RegisterType::Float);
    CHECK_EQ(sse.typeAt(1), RegisterType::Float);
    auto dominated = sse.dominateSSE();
    CHECK_EQ(sse.types, dominated.types);
    CHECK_EQ(dominated.pop(1), RegisterType::Float);
    CHECK_EQ(dominated.pop(1), RegisterType::Float);
  }
  SUBCASE("Int dominates float registers") {
    RegisterAssignment registers;
    registers.push(RegisterType::Int);
    registers.push(RegisterType::Float);
    CHECK_NE(registers.typeAt(0), registers.typeAt(1));
    auto dominated = registers.dominateSSE();
    CHECK_EQ(dominated.typeAt(0), RegisterType::Int);
    CHECK_EQ(dominated.typeAt(1), RegisterType::Int);

    CHECK_EQ(registers.pop(), RegisterType::Int);
    CHECK_EQ(registers.pop(), RegisterType::Float);
    CHECK_EQ(dominated.pop(), RegisterType::Int);
    CHECK_EQ(dominated.pop(), RegisterType::Int);
  }
}
