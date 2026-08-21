#pragma once
#include "common.h"
#include "fmt/base.h"
#include "fmt/ostream.h"
#include "tsl/ordered_set.h"
#include <cctype>
#include <cstring>
#include <sys/mman.h>
#include <unistd.h>

enum class TokenType {
  // Symbols
  LeftParen,
  RightParen,
  LeftCurlyBrace,
  RightCurlyBrace,
  LeftSquareBracket,
  RightSquareBracket,
  Comma,
  Colon,

  // Unary ops
  Pointer,
  MultiPointer,
  Dot,
  Not,

  Assign,
  Subtype,

  // Binops
  BINOP_START,
  COMPARE_START = BINOP_START,
  DoubleEqual = COMPARE_START,
  NotEqual,
  Lt,
  Gt,
  Leq,
  Geq,
  COMPARE_END = Geq,
  ARITH_START,
  Plus = ARITH_START,
  Minus,
  Mult,
  Power,
  Div,
  LeftDiv,
  Remainder,
  ARITH_END = Remainder,
  LOGIC_START,
  LogicAnd = LOGIC_START,
  LogicOr,
  LOGIC_END = LogicOr,
  BITWISE_START,
  BitAnd = BITWISE_START,
  BitOr,
  Xor,
  BITWISE_END = Xor,
  SHIFT_START,
  ShiftLeft = SHIFT_START,
  ShiftRight,
  SHIFT_END = ShiftRight,
  BINOP_END = SHIFT_END,
  BINOP_ASSIGN_START,
  BINOP_ASSIGN_END = BINOP_ASSIGN_START + BINOP_END - BINOP_START,

  ThinArrow,
  FatArrow,
  ExclusiveRange,
  InclusiveRange,

  KEYWORD_START,
  If = KEYWORD_START,
  Else,
  For,
  While,
  Return,
  True,
  False,
  Null,
  Function,
  Kernel,
  Struct,
  Import,
  Union,
  Enum,
  Trait,
  Impl,
  Mut,
  Opaque,
  Generic,
  Self,
  Using,
  When,

  // TODO
  Test,
  Invariant,
  KEYWORD_END = Invariant,
  Wildcard,

  // Literal
  Identifier,
  String,
  NullTerminatedString,
  Decimal,
  Integer,
  HexInt,
  MultiLineString,
  Char,
  // TODO: int types
  UnsignedIntType,
  SignedIntType,
  Undef,

  // Builtins
  BUILTIN_NumCast,
  BUILTIN_BitCast,
  BUILTIN_RegisterType,
  BUILTIN_CInclude,
  BUILTIN_CDefine,
  BUILTIN_CUndef,
  BUILTIN_CIncludeDir,
  BUILTIN_Link,
  BUILTIN_LinkDir,
  BUILTIN_CImport,
  BUILTIN_Type,
  BUILTIN_Align,
  BUILTIN_Local,
  BUILTIN_Shared,
  BUILTIN_Global,
  BUILTIN_Name,
  BUILTIN_CudaPtx,
  BUILTIN_Crash,
  BUILTIN_Sizeof,
  BUILTIN_Alignof,
  BUILTIN_Bitsize,
  BUILTIN_PtrCast,
  BUILTIN_Binclude,
  BUILTIN_Raw,

  // CUDA
  CudaImport,
  CudaBlockIdxX,
  CudaBlockIdxY,
  CudaBlockIdxZ,
  CudaBlockDimX,
  CudaBlockDimY,
  CudaBlockDimZ,
  CudaThreadIdxX,
  CudaThreadIdxY,
  CudaThreadIdxZ,
  CudaGridDimX,
  CudaGridDimY,
  CudaGridDimZ,

  // TODO: figure out meaningful whitespace
  StatementBreak,
  EndOfFile
};

constexpr u32 IdentifierBits = 24;

struct IdentifierInternPool {
  static usize pageSize() {
    static usize size = static_cast<usize>(sysconf(_SC_PAGESIZE));
    return size;
  }
  static usize alignUp(usize size, usize alignment) {
    return (size + alignment - 1) & ~(alignment - 1);
  }

  char* bytes = nullptr;
  usize capacity = alignUp(usize(1) << 34, pageSize());
  usize offset = 0;
  tsl::ordered_set<string_view> pool;

  IdentifierInternPool() {
    void* memory = mmap(
      nullptr,
      capacity,
      PROT_READ | PROT_WRITE,
      MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE,
      -1,
      0
    );
    if (memory == MAP_FAILED) throw std::bad_alloc();
    bytes = static_cast<char*>(memory);
    pool.insert(""); // Reserve index zero as the null identifier.
  }

  Identifier intern(string_view value) {
    auto [iterator, inserted] = pool.insert(value);
    auto index = static_cast<u32>(std::distance(pool.begin(), iterator));
    if (index >= (1u << IdentifierBits)) {
      throw std::invalid_argument("Too many identifiers");
    }
    if (inserted) {
      if (offset + value.size() > capacity) {
        throw std::invalid_argument("OOM in identifier pool");
      }
      char* destination = bytes + offset;
      memcpy(destination, value.data(), value.size());
      offset += value.size();
      auto key = const_cast<string_view*>(&iterator.key());
      *key = string_view(destination, value.size());
    }
    return static_cast<Identifier>(index);
  }

  string_view get(Identifier identifier) const {
    return pool.values_container().at(identifier.index);
  }
};

struct Token {
  u32 location = 0;
  Identifier identifier{};
  TokenType type = TokenType::EndOfFile;

  explicit operator bool() const {
    return type != TokenType::EndOfFile;
  }

  static bool isTokenSubType(TokenType type, TokenType lower, TokenType upper) {
    return type >= lower && type <= upper;
  }
  static bool isArithmeticOperation(TokenType type) {
    return isTokenSubType(type, TokenType::BINOP_START, TokenType::BINOP_END);
  }
  static bool isComparison(TokenType type) {
    return isTokenSubType(
      type,
      TokenType::COMPARE_START,
      TokenType::COMPARE_END
    );
  }
  static bool isLogic(TokenType type) {
    return isTokenSubType(type, TokenType::LOGIC_START, TokenType::LOGIC_END);
  }
  static bool isShift(TokenType type) {
    return isTokenSubType(type, TokenType::SHIFT_START, TokenType::SHIFT_END);
  }
  static bool isOpenBinop(TokenType type) {
    return isTokenSubType(type, TokenType::BINOP_START, TokenType::BINOP_END);
  }
  static bool isBinaryOp(TokenType type) {
    static unordered_set<TokenType> operators{
      TokenType::Dot,
      TokenType::LeftParen,
      TokenType::LeftSquareBracket,
      TokenType::ExclusiveRange,
      TokenType::InclusiveRange,
      TokenType::Impl,
    };
    return isTokenSubType(
             type,
             TokenType::BINOP_START,
             TokenType::BINOP_ASSIGN_END
           ) ||
           operators.contains(type);
  }
  bool isClosingToken() const {
    static unordered_set<TokenType> closing{
      TokenType::StatementBreak,
      TokenType::Comma,
      TokenType::RightCurlyBrace,
      TokenType::RightSquareBracket,
      TokenType::RightParen,
      TokenType::Else,
      TokenType::Assign,
      TokenType::Colon,
      TokenType::FatArrow,
      TokenType::ThinArrow,
      TokenType::Subtype,
      TokenType::LeftCurlyBrace,
    };
    return closing.contains(type);
  }
  bool canApply() const {
    return !(isClosingToken() || isBinaryOp(type));
  }
  bool isBuiltin() const {
    return isTokenSubType(
             type,
             TokenType::BUILTIN_NumCast,
             TokenType::BUILTIN_Raw
           ) &&
           type != TokenType::BUILTIN_Crash &&
           type != TokenType::BUILTIN_CudaPtx &&
           type != TokenType::BUILTIN_Binclude;
  }
  bool isLiteral() const {
    static unordered_set<TokenType> literals{
      TokenType::String,         TokenType::Decimal,
      TokenType::Integer,        TokenType::NullTerminatedString,
      TokenType::True,           TokenType::False,
      TokenType::Identifier,     TokenType::Opaque,
      TokenType::CudaBlockIdxX,  TokenType::CudaBlockIdxY,
      TokenType::CudaBlockIdxZ,  TokenType::CudaBlockDimX,
      TokenType::CudaBlockDimY,  TokenType::CudaBlockDimZ,
      TokenType::CudaThreadIdxX, TokenType::CudaThreadIdxY,
      TokenType::CudaThreadIdxZ, TokenType::CudaGridDimX,
      TokenType::CudaGridDimY,   TokenType::CudaGridDimZ,
      TokenType::Char,           TokenType::Self,
      TokenType::HexInt,         TokenType::Undef,
      TokenType::Null,           TokenType::BUILTIN_Crash,
    };
    return literals.contains(type);
  }
  static optional<TokenType> binopFromCompoundAssignment(TokenType type) {
    auto binop = TokenType(
      static_cast<int>(type) - static_cast<int>(TokenType::BINOP_ASSIGN_START) +
      static_cast<int>(TokenType::BINOP_START)
    );
    return isOpenBinop(binop) ? optional<TokenType>(binop) : std::nullopt;
  }
  static optional<TokenType> compoundAssignmentFromBinop(TokenType type) {
    if (!isOpenBinop(type)) return std::nullopt;
    return TokenType(
      static_cast<int>(type) + static_cast<int>(TokenType::BINOP_ASSIGN_START) -
      static_cast<int>(TokenType::BINOP_START)
    );
  }
};

struct Tokenizer {
  struct ScanResult {
    u32 end;
    TokenType type;
    Identifier identifier{};
    bool emit = true;
  };

  static IdentifierInternPool& identifierPool();
  static std::unordered_map<string_view, TokenType> builtinFunctions;
  static std::unordered_map<string_view, TokenType> keywords;

  static bool isHex(char c) {
    return std::isxdigit(static_cast<unsigned char>(c));
  }

  Logger log{LogLevel::Tokenize};
  const string_view sourceCode;
  fs::path& inputFilePath;
  vector<Token> tokens;

  Tokenizer(const string_view& source, fs::path& inputFile)
      : sourceCode(source), inputFilePath(inputFile) {
    bool emittedStatementBreak = false;
    for (u32 start = 0; start < sourceCode.size();) {
      auto result = scan(start);
      if (result.end <= start) crash(start, "Tokenizer did not advance");
      if (
        result.emit &&
        !(result.type == TokenType::StatementBreak && emittedStatementBreak)
      ) {
        tokens.push_back(
          {.location = start,
           .identifier = result.identifier,
           .type = result.type}
        );
      }
      if (result.emit) {
        emittedStatementBreak = result.type == TokenType::StatementBreak;
      }
      start = result.end;
    }
    tokens.push_back(
      {.location = static_cast<u32>(sourceCode.size()),
       .type = TokenType::EndOfFile}
    );
  }

  string_view lexeme(const Token& token) const {
    if (token.type == TokenType::Identifier)
      return identifierPool().get(token.identifier);
    if (token.type == TokenType::EndOfFile) return {};
    auto end = scan(token.location).end;
    u32 start = token.location;
    if (token.type == TokenType::String || token.type == TokenType::Char) {
      start++;
      end--;
    } else if (token.type == TokenType::NullTerminatedString) {
      start += 2;
      end--;
    } else if (
      token.isBuiltin() || token.type == TokenType::CudaImport ||
      token.type == TokenType::BUILTIN_CudaPtx ||
      token.type == TokenType::BUILTIN_Binclude
    ) {
      start++;
    } else if (token.type == TokenType::MultiLineString) {
      start += 2;
    }
    return sourceCode.substr(start, end - start);
  }

  string_view lexeme(const Token* token) const {
    return lexeme(*token);
  }

  struct TokenLocation {
    u32 line;
    u32 column;
    string_view lineContents;
    string_view lexeme;

    void underline(std::ostream& out) const {
      fmt::println(out, "{: >8}| {}", line, lineContents);
      fmt::print(out, "{: >8}| ", "");
      fmt::println(out, "{: >{}}{:^>{}}", "", column, "", lexeme.size());
    }
  };

  TokenLocation locationOf(const Token& token) const {
    u32 line = 1;
    u32 lineStart = 0;
    for (u32 index = 0; index < token.location; index++) {
      if (sourceCode[index] == '\n') {
        line++;
        lineStart = index + 1;
      }
    }
    u32 lineEnd = token.location;
    while (lineEnd < sourceCode.size() && sourceCode[lineEnd] != '\n')
      lineEnd++;
    return {
      .line = line,
      .column = token.location - lineStart,
      .lineContents = sourceCode.substr(lineStart, lineEnd - lineStart),
      .lexeme = lexeme(token)
    };
  }

  TokenLocation locationOf(const Token* token) const {
    return locationOf(*token);
  }

  ScanResult scan(u32 start) const {
    if (start >= sourceCode.size()) return {start, TokenType::EndOfFile};
    const auto at = [this](u32 index) -> char {
      return index < sourceCode.size() ? sourceCode[index] : '\0';
    };
    const auto simple = [start](TokenType type) {
      return ScanResult{start + 1, type};
    };
    const auto compound = [&at, start](TokenType type) {
      if (
        auto assignment = Token::compoundAssignmentFromBinop(type);
        assignment && at(start + 1) == '='
      ) {
        return ScanResult{start + 2, *assignment};
      }
      return ScanResult{start + 1, type};
    };
    const auto identifierChar = [](char c) {
      return std::isalnum(static_cast<unsigned char>(c)) || c == '_';
    };
    const auto alphaUnder = [](char c) {
      return std::isalpha(static_cast<unsigned char>(c)) || c == '_';
    };

    char c = at(start);
    switch (c) {
    case ' ':
    case '\r':
    case '\t':
      return {start + 1, TokenType::EndOfFile, {}, false};
    case '\n':
      return simple(TokenType::StatementBreak);
    case '#': {
      u32 end = start + 1;
      while (at(end) && at(end) != '\n')
        end++;
      return {end, TokenType::EndOfFile, {}, false};
    }
    case '(':
      return simple(TokenType::LeftParen);
    case ')':
      return simple(TokenType::RightParen);
    case '{':
      return simple(TokenType::LeftCurlyBrace);
    case '}':
      return simple(TokenType::RightCurlyBrace);
    case ',':
      return simple(TokenType::Comma);
    case ':':
      return simple(TokenType::Colon);
    case '^':
      return simple(TokenType::Pointer);
    case '!':
      return at(start + 1) == '=' ? ScanResult{start + 2, TokenType::NotEqual}
                                  : simple(TokenType::Not);
    case '[':
      return at(start + 1) == '^' && at(start + 2) == ']'
               ? ScanResult{start + 3, TokenType::MultiPointer}
               : simple(TokenType::LeftSquareBracket);
    case ']':
      return simple(TokenType::RightSquareBracket);
    case '+':
      return compound(TokenType::Plus);
    case '*':
      return at(start + 1) == '*'
               ? (at(start + 2) == '='
                    ? ScanResult{start + 3, *Token::compoundAssignmentFromBinop(TokenType::Power)}
                    : ScanResult{start + 2, TokenType::Power})
               : compound(TokenType::Mult);
    case '/':
      return compound(TokenType::Div);
    case '%':
      return compound(TokenType::Remainder);
    case '&':
      return compound(TokenType::BitAnd);
    case '|':
      return compound(TokenType::BitOr);
    case '~':
      return compound(TokenType::Xor);
    case '\\': {
      if (at(start + 1) == '"') {
        u32 end = start + 2;
        while (at(end) && at(end) != '\n')
          end++;
        return {end, TokenType::MultiLineString};
      }
      return compound(TokenType::LeftDiv);
    }
    case '-':
      if (at(start + 1) == '>') return {start + 2, TokenType::ThinArrow};
      if (at(start + 1) == '-' && at(start + 2) == '-')
        return {start + 3, TokenType::Undef};
      return compound(TokenType::Minus);
    case '=':
      if (at(start + 1) == '=') return {start + 2, TokenType::DoubleEqual};
      if (at(start + 1) == '>') return {start + 2, TokenType::FatArrow};
      return simple(TokenType::Assign);
    case '<':
      if (at(start + 1) == '=') return {start + 2, TokenType::Leq};
      if (at(start + 1) == '<')
        return at(start + 2) == '='
                 ? ScanResult{start + 3, *Token::compoundAssignmentFromBinop(TokenType::ShiftLeft)}
                 : ScanResult{start + 2, TokenType::ShiftLeft};
      if (at(start + 1) == ':') return {start + 2, TokenType::Subtype};
      return simple(TokenType::Lt);
    case '>':
      if (at(start + 1) == '=') return {start + 2, TokenType::Geq};
      if (at(start + 1) == '>')
        return at(start + 2) == '='
                 ? ScanResult{start + 3, *Token::compoundAssignmentFromBinop(TokenType::ShiftRight)}
                 : ScanResult{start + 2, TokenType::ShiftRight};
      return simple(TokenType::Gt);
    case '.':
      if (at(start + 1) == '.') {
        if (at(start + 2) == '<') return {start + 3, TokenType::ExclusiveRange};
        if (at(start + 2) == '=') return {start + 3, TokenType::InclusiveRange};
        crash(start, "Expected '<' or '=' after '..'");
      }
      if (std::isdigit(static_cast<unsigned char>(at(start + 1))))
        return number(start, start + 1, true);
      return simple(TokenType::Dot);
    case '\'':
      return character(start);
    case '"':
      return string(start, TokenType::String, start + 1);
    case '@': {
      u32 end = start + 1;
      while (alphaUnder(at(end)))
        end++;
      auto found =
        builtinFunctions.find(sourceCode.substr(start + 1, end - start - 1));
      if (found == builtinFunctions.end())
        crash(
          start,
          "Unknown builtin '{}"
          "'",
          sourceCode.substr(start + 1, end - start - 1)
        );
      return {end, found->second};
    }
    default:
      break;
    }

    if (c == 'c' && at(start + 1) == '"')
      return string(start, TokenType::NullTerminatedString, start + 2);
    if (c == '0' && at(start + 1) == 'x') return hexadecimal(start);
    if (std::isdigit(static_cast<unsigned char>(c)))
      return number(start, start, false);
    if (alphaUnder(c)) {
      u32 end = start + 1;
      while (identifierChar(at(end)))
        end++;
      auto value = sourceCode.substr(start, end - start);
      if (auto keyword = keywords.find(value); keyword != keywords.end())
        return {end, keyword->second};
      return {end, TokenType::Identifier, identifierPool().intern(value)};
    }
    crash(start, "Unexpected character '{}'", c);
  }

  ScanResult string(u32 start, TokenType type, u32 current) const {
    bool escaping = false;
    while (current < sourceCode.size()) {
      char c = sourceCode[current++];
      if (!escaping && c == '"') return {current, type};
      escaping = !escaping && c == '\\';
    }
    crash(start, "Unterminated string");
  }

  ScanResult character(u32 start) const {
    u32 current = start + 1;
    if (current >= sourceCode.size())
      crash(start, "Unterminated character literal");
    if (sourceCode[current++] == '\\') {
      if (current >= sourceCode.size())
        crash(start, "Unterminated character escape");
      current++;
    }
    if (current >= sourceCode.size() || sourceCode[current] != '\'') {
      crash(start, "Expected terminating quote for character literal");
    }
    return {current + 1, TokenType::Char};
  }

  ScanResult hexadecimal(u32 start) const {
    u32 current = start + 2;
    u32 digits = current;
    while (isHex(current < sourceCode.size() ? sourceCode[current] : '\0'))
      current++;
    if (current == digits)
      crash(start, "Expected hexadecimal digits after '0x'");
    return {current, TokenType::HexInt};
  }

  ScanResult number(u32 start, u32 current, bool hasDecimal) const {
    while (current < sourceCode.size() &&
           std::isdigit(static_cast<unsigned char>(sourceCode[current])))
      current++;
    if (
      !hasDecimal && current < sourceCode.size() &&
      sourceCode[current] == '.' &&
      !(current + 1 < sourceCode.size() && sourceCode[current + 1] == '.')
    ) {
      hasDecimal = true;
      current++;
      while (current < sourceCode.size() &&
             std::isdigit(static_cast<unsigned char>(sourceCode[current])))
        current++;
    }
    if (
      current < sourceCode.size() &&
      (sourceCode[current] == 'e' || sourceCode[current] == 'E')
    ) {
      hasDecimal = true;
      current++;
      if (
        current < sourceCode.size() &&
        (sourceCode[current] == '+' || sourceCode[current] == '-')
      )
        current++;
      u32 exponent = current;
      while (current < sourceCode.size() &&
             std::isdigit(static_cast<unsigned char>(sourceCode[current])))
        current++;
      if (current == exponent) crash(start, "Expected digits after exponent");
    }
    return {current, hasDecimal ? TokenType::Decimal : TokenType::Integer};
  }

  template <typename... Args>
  [[noreturn]] void crash(
    u32 location,
    fmt::format_string<Args...> fmt,
    Args&&... args
  ) const {
    fmt::println(
      std::cerr,
      "Tokenizer error in file {} at byte {}",
      inputFilePath.string(),
      location
    );
    fmt::println(std::cerr, fmt, std::forward<Args>(args)...);
    abort();
  }
};
