#pragma once
#include "common.h"
#include "types.h"
#include "value.h"
#include <filesystem>
#include <functional>
#include <unordered_map>

using TypeCache = std::unordered_map<Identifier, TypeIndex>;
using TypeEmitter = std::function<void(TypeIndex)>;

// TODO: proper lexing+parsing: see
// https://github.com/nothings/stb/blob/master/stb_c_lexer.h
TypeIndex parseType(std::string_view qualType, IrCommandBuffer& globals);
Environment* cBindings(
  fs::path& cFile,
  string_view prefix,
  IrCommandBuffer& globals,
  TypeCache& definedTypes,
  TypeEmitter emitType = {},
  std::function<void(std::string_view)> emitStaticInline = {}
);

struct ClangArg {
  struct IncludeFile {
    fs::path path;
    bool operator==(const IncludeFile&) const = default;
  };
  struct IncludeDir {
    fs::path path;
    bool operator==(const IncludeDir&) const = default;
  };
  struct Define {
    Identifier symbol;
    bool operator==(const Define&) const = default;
  };
  struct Undefine {
    Identifier symbol;
    bool operator==(const Undefine&) const = default;
  };
  struct ValueDefine {
    Identifier symbol;
    Identifier value;
    bool operator==(const ValueDefine&) const = default;
  };

  std::variant<IncludeFile, IncludeDir, Define, Undefine, ValueDefine> arg;

  friend std::ostream& operator<<(std::ostream& o, const ClangArg& arg) {
    std::visit(
      overloaded{
        [&o](IncludeFile x) { fmt::print(o, "-include {}", x.path.string()); },
        [&o](IncludeDir x) { fmt::print(o, "-I{}", x.path.string()); },
        [&o](Define x) { fmt::print(o, "-D{}", x.symbol); },
        [&o](Undefine x) { fmt::print(o, "-U{}", x.symbol); },
        [&o](ValueDefine x) { fmt::print(o, "-D{}={}", x.symbol, x.value); }
      },
      arg.arg
    );
    return o;
  }
  bool operator==(const ClangArg&) const = default;
};

namespace std {
template <> struct hash<ClangArg> {
  size_t operator()(const ClangArg& x) const {
    return std::visit(
      overloaded{
        [](const ClangArg::IncludeFile& a) {
          return hash<fs::path>()(a.path) ^ 0x01;
        },
        [](const ClangArg::IncludeDir& a) {
          return hash<fs::path>()(a.path) ^ 0x02;
        },
        [](const ClangArg::Define& a) {
          return hash<Identifier>()(a.symbol) ^ 0x03;
        },
        [](const ClangArg::Undefine& a) {
          return hash<Identifier>()(a.symbol) ^ 0x04;
        },
        [](const ClangArg::ValueDefine& a) {
          auto seed = hash<Identifier>()(a.symbol);
          seed ^= hash<Identifier>()(a.value) ^ 0x06;
          return seed;
        },
      },
      x.arg
    );
  }
};
} // namespace std

template <> struct fmt::formatter<ClangArg> : fmt::ostream_formatter {};
