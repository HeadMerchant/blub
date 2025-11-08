#pragma once
#include "fmt/base.h"
#include <cassert>
#include <cstdint>
#include <filesystem>
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
namespace fs = std::filesystem;
