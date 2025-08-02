#pragma once
#include <cstdint>
#include <cassert>
#include <string>
#include <tuple>

using i32 = uint32_t;
using i8 = uint8_t;
[[ noreturn ]] void TODO(std::string message);

i32 packInt(i8 a, i8 b, i8 c, i8 d) {
  return (a << 24) | (b << 16) | (c << 8) | (d);
}

std::tuple<i8, i8, i8, i8> unpackInt(i32 value) {
  return {(value >> 24) & 255, (value >> 16) & 255, (value >> 8) & 255, value & 255};
}
