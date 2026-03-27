#pragma once
#include "../deps/doctest.h"
#include "fmt/base.h"
#include <bit>
#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
// using s8 = int8_t;
// using s16 = int16_t;
// using s32 = int32_t;
// using s64 = int64_t;

#ifndef ASSERT_INVARIANTS
#ifndef DOCTEST_CONFIG_INVARIANTS
#define ASSERT_INVARIANTS
#endif
#endif

[[noreturn]] void TODO(std::string message);

u32 packInt(u8 a, u8 b, u8 c, u8 d);

std::tuple<u8, u8, u8, u8> unpackInt(u32 value);

template <typename... Ts, typename Variant> bool isAny(const Variant& v) {
  return ((std::holds_alternative<Ts>(v)) || ...);
}

template <typename... Ts> struct fmt::formatter<std::variant<Ts...>> {
  template <typename FormatParseContext> constexpr auto parse(FormatParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext> auto format(const std::variant<Ts...>& value, FormatContext& ctx) const {
    return std::visit([&ctx](const auto& v) { return fmt::format_to(ctx.out(), "{}", v); }, value);
  }
};

using std::optional;
using std::pair;
using std::stringstream;
using std::tuple;
using std::vector;
using Identifier = std::string_view;
using std::bit_cast;
using std::span;
using std::string_view;
using std::unordered_map;
using std::unordered_set;
using std::views::transform;
namespace fs = std::filesystem;

struct StringPool {
  char* bytes;
  u32 offset;
  u32 capacity;

  std::string_view copy(std::string_view view) {
    u32 newOffset = offset + view.length();
    if (newOffset < offset || offset >= capacity) {
      debug();
      throw std::invalid_argument("OOM in string view pool");
    }
    memcpy(bytes + offset, view.data(), view.length());
    std::string_view newView{bytes + offset, view.length()};

    offset = newOffset;

    return newView;
  }

  StringPool(u32 capacity) {
    bytes = (char*)malloc(capacity);
    this->capacity = capacity;
  }

  void debug() {
    std::string_view view(bytes, offset);
    std::cout << view << std::endl;
  }

  static StringPool& inst();
};

enum class LogLevel {
  Parsing = 1,
  Tokenize = 2,
  CImport = 4,
  Compile = 8,
};

inline LogLevel operator|(LogLevel a, LogLevel b) {
  return static_cast<LogLevel>(static_cast<int>(a) | static_cast<int>(b));
}

inline int operator&(LogLevel a, LogLevel b) {
  return static_cast<int>(a) & static_cast<int>(b);
}

struct Logger {
  // static inline struct {
  //   unsigned int parsing : 1;
  //   unsigned int tokenize : 1;
  //   unsigned int cImport : 1;
  //   unsigned int compile : 1;
  // } setLevels;
  static inline LogLevel globalLevels = static_cast<LogLevel>(0);
  LogLevel logLevel;

  template <typename... Args> void operator()(fmt::format_string<Args...> fmt, Args&&... args) const {
    if (globalLevels & logLevel) {
      fmt::println(fmt, std::forward<Args>(args)...);
    }
  }
};

template <class... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};
