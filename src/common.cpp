#include "common.h"
#include <stdexcept>

[[noreturn]] void TODO(std::string message) {
  throw std::runtime_error(message);
}

i32 packInt(i8 a, i8 b, i8 c, i8 d) {
  return (a << 24) | (b << 16) | (c << 8) | (d);
}

std::tuple<i8, i8, i8, i8> unpackInt(i32 value) {
  return {(value >> 24) & 255, (value >> 16) & 255, (value >> 8) & 255, value & 255};
}

// template <typename... Ts, typename Variant>
// bool isAny(const Variant& v) {
//     return ((std::holds_alternative<Ts>(v)) || ...);
// }

// template<typename... Ts>
// struct fmt::formatter<std::variant<Ts...>> {
//     template<typename FormatParseContext>
//     constexpr auto parse(FormatParseContext& ctx) {
//         return ctx.begin();
//     }

//     template<typename FormatContext>
//     auto format(const std::variant<Ts...>& value, FormatContext& ctx) const {
//         return std::visit(
//             [&ctx](const auto& v) {
//                 return fmt::format_to(ctx.out(), "{}", v);
//             },
//             value
//         );
//     }
// };
