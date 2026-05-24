#include "common.h"
#include <stdexcept>

[[noreturn]] void TODO(std::string message) {
  throw std::runtime_error(message);
}

u32 packInt(u8 a, u8 b, u8 c, u8 d) {
  return (a << 24) | (b << 16) | (c << 8) | (d);
}

std::tuple<u8, u8, u8, u8> unpackInt(u32 value) {
  return {
    (value >> 24) & 255,
    (value >> 16) & 255,
    (value >> 8) & 255,
    value & 255
  };
}

// static char* StringPool::bytes = malloc(64 * 4096);

StringPool& StringPool::inst() {
  static StringPool pool(64 * 4096);
  return pool;
}
