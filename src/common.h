#pragma once
#include "fmt/base.h"
#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <optional>
#include <string>
#include <tuple>
#include <variant>

using i32 = uint32_t;
using i8 = uint8_t;
[[noreturn]] void TODO(std::string message);

i32 packInt(i8 a, i8 b, i8 c, i8 d);

std::tuple<i8, i8, i8, i8> unpackInt(i32 value);

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
using Identifier = std::string_view;
namespace fs = std::filesystem;

struct StringPool {
  char* bytes;
  i32 offset;
  i32 capacity;

  std::string_view copy(std::string_view view) {
    i32 newOffset = offset + view.length();
    if (newOffset < offset || offset >= capacity) {
      debug();
      throw std::invalid_argument("OOM in string view pool");
    }
    memcpy(bytes + offset, view.data(), view.length());
    std::string_view newView{bytes + offset, view.length()};

    offset = newOffset;

    return newView;
  }

  StringPool(i32 capacity) {
    bytes = (char*) malloc(capacity);
    this->capacity = capacity;
  }

  void debug() {
    std::string_view view(bytes, offset);
    std::cout << view << std::endl;
    // fmt::println("Strings: {}", view);
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

