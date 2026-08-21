#pragma once
#include "doctest.h"
#include "fmt/format.h"
#include "tsl/ordered_map.h"
#include <bit>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <iostream>
#include <optional>
#include <ostream>
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
using s8 = int8_t;
using usize = size_t;
using std::bit_cast;
using std::optional;
using std::pair;
using std::span;
using std::string_view;
using std::stringstream;
using std::tuple;
using std::unordered_map;
using std::unordered_set;
using std::vector;
using std::views::transform;
namespace fs = std::filesystem;

// IR is accumulated as spans in a single arena.  This keeps declaration and
// function emission ordered without copying completed text between queues and
// string streams.  The stream adapter lets the existing formatter-based
// instruction emitter write directly to the arena.
class IrCommandBuffer {
public:
  struct TextSpan {
    usize offset;
    usize length;
  };
  struct EmbeddedFile {
    fs::path path;
    bool nullTerminate;
  };
  using Command = std::variant<TextSpan, EmbeddedFile>;

private:
  std::string arena;
  std::vector<Command> commands;

  void append(const char* text, usize length) {
    if (!length) return;
    auto offset = arena.size();
    arena.append(text, length);
    commands.emplace_back(TextSpan{offset, length});
  }

  class StreamBuf : public std::streambuf {
    IrCommandBuffer& buffer;

  public:
    explicit StreamBuf(IrCommandBuffer& buffer) : buffer(buffer) {}

  protected:
    std::streamsize xsputn(const char* text, std::streamsize length) override {
      buffer.append(text, static_cast<usize>(length));
      return length;
    }
    int overflow(int ch) override {
      if (ch != EOF) buffer.append(reinterpret_cast<const char*>(&ch), 1);
      return ch;
    }
  } streamBuf;
  std::ostream stream{&streamBuf};

public:
  IrCommandBuffer() : streamBuf(*this) {}
  IrCommandBuffer(const IrCommandBuffer&) = delete;
  IrCommandBuffer& operator=(const IrCommandBuffer&) = delete;

  std::ostream& output() {
    return stream;
  }
  // Kept as the queue-compatible boundary used by global declarations.  The
  // old queue drain inserted one trailing newline per entry.
  void push(std::string_view text) {
    append(text.data(), text.size());
    append("\n", 1);
  }
  void embed(fs::path path, bool nullTerminate = false) {
    commands.emplace_back(EmbeddedFile{std::move(path), nullTerminate});
  }
  bool empty() const {
    return commands.empty();
  }
  void drain(std::ostream& output);
};

struct Identifier {
  u32 index = 0;

  Identifier() = default;
  Identifier(u32 index) : index(index) {}
  Identifier(string_view value);
  Identifier(const char* value) : Identifier(string_view(value)) {}
  operator string_view() const;
  usize size() const;
  usize length() const {
    return size();
  }
  bool empty() const {
    return size() == 0;
  }
  bool operator==(const Identifier&) const = default;

  friend bool operator==(Identifier left, string_view right) {
    return static_cast<string_view>(left) == right;
  }
  friend bool operator==(string_view left, Identifier right) {
    return left == static_cast<string_view>(right);
  }
};

inline std::ostream& operator<<(std::ostream& output, Identifier identifier) {
  return output << static_cast<string_view>(identifier);
}

namespace std {
template <> struct hash<Identifier> {
  size_t operator()(Identifier identifier) const {
    return identifier.index;
  }
};
} // namespace std

template <> struct fmt::formatter<Identifier> : fmt::formatter<string_view> {
  template <typename FormatContext>
  auto format(Identifier identifier, FormatContext& context) const {
    auto value = static_cast<std::string_view>(identifier);
    return fmt::format_to(context.out(), "{}", value);
  }
};

class Environment;

struct Label {
private:
  u32 index;

  explicit Label(u32 index) : index(index) {}
  friend class Environment;
  friend struct fmt::formatter<Label>;

public:
  static Label null() {
    return Label(0);
  }
};

template <> struct fmt::formatter<Label> : fmt::formatter<u32> {
  template <typename FormatContext>
  auto format(const Label& obj, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "L{}", obj.index);
  }
};

#undef assert
#define assert(cond)                                                          \
  do {                                                                        \
    if (!(cond)) {                                                            \
      throw std::runtime_error(                                               \
        std::string("Assertion failed: ") + #cond + " at " + __FILE__ + ":" + \
        std::to_string(__LINE__)                                              \
      );                                                                      \
    }                                                                         \
  } while (false)

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
  template <typename FormatParseContext>
  constexpr auto parse(FormatParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const std::variant<Ts...>& value, FormatContext& ctx) const {
    return std::visit(
      [&ctx](const auto& v) { return fmt::format_to(ctx.out(), "{}", v); },
      value
    );
  }
};

#include "linkagenames.h"
using RegisterName = std::variant<LinkageName, u32>;

enum class LogLevel {
  Parsing = 1,
  Tokenize = 1,
  TypeCheck = 2,
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
  static inline LogLevel globalLevels = static_cast<LogLevel>(0);
  LogLevel logLevel;

  bool canLog() const {
    return globalLevels & logLevel;
  }

  template <typename... Args>
  void operator()(fmt::format_string<Args...> fmt, Args&&... args) const {
    if (canLog()) {
      fmt::println(fmt, std::forward<Args>(args)...);
    }
  }
};

template <class... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};

template <
  class Key,
  class T,
  class Hash = std::hash<Key>,
  class KeyEqual = std::equal_to<Key>,
  class Allocator = std::allocator<std::pair<Key, T>>,
  class IndexType = std::uint_least32_t>
using ordered_map = tsl::ordered_map<
  Key,
  T,
  Hash,
  KeyEqual,
  Allocator,
  std::vector<std::pair<Key, T>, Allocator>,
  IndexType>;

using std::abort;
#include <algorithm>
#include <map>
#include <span>
#include <vector>

template <typename T> struct VecSpanCompare {
  using is_transparent = void;

  bool operator()(const std::vector<T>& a, const std::vector<T>& b) const {
    return a < b;
  }
  bool operator()(const std::vector<T>& a, std::span<const T> b) const {
    return std::lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }
  bool operator()(std::span<const T> a, const std::vector<T>& b) const {
    return std::lexicographical_compare(a.begin(), a.end(), b.begin(), b.end());
  }
};

// Keeping here for later usage. might not need
template <typename K, typename V>
using VecMap = std::map<std::vector<K>, V, VecSpanCompare<K>>;

#include <ranges>

template <std::ranges::input_range... Rs> auto zip(Rs&&... rs) {
  auto size = std::min({std::ranges::size(rs)...});
  return std::views::iota(0u, size) | std::views::transform([&rs...](auto i) {
           return std::make_tuple(rs[i]...);
         });
}

template <std::ranges::random_access_range R>
auto enumerate(R&& r, std::size_t start = 0) {
  auto size = std::ranges::size(r);
  return std::views::iota(start, start + size) |
         std::views::transform([&r](auto i) {
           return std::make_tuple(i, r[i]);
         });
}

template <typename... Args>
[[noreturn]] void crash(fmt::format_string<Args...> fmt, Args&&... args) {
  fmt::println(fmt, std::forward<Args>(args)...);
  abort();
}
void emitEmbeddedFile(
  std::ostream& outFile,
  const fs::path& filePath,
  bool nullTerminated = false
);
