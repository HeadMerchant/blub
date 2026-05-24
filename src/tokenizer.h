#pragma once
#include "common.h"
#include "fmt/base.h"
#include "fmt/ostream.h"
#include <cctype>
#include <iostream>
#include <optional>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

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

  // Control flow
  If,
  Else,
  For,
  While,
  Return,

  // Literals
  True,
  False,

  // CONSTANT_DECLARATION,
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

  // Keywords
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
  BUILTIN_CIncludeDir,
  BUILTIN_Link,
  BUILTIN_LinkDir,
  BUILTIN_CImport,
  BUILTIN_Type,
  BUILTIN_Align,
  BUILTIN_Local,
  BUILTIN_Shared,
  BUILTIN_Global,

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

struct Token {
public:
  const std::string_view lexeme;
  const int line;
  const TokenType type;

  static bool isTokenSubType(
    TokenType type,
    TokenType lowerBound,
    TokenType upperBound
  ) {
    return type <= upperBound && type >= lowerBound;
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
    static unordered_set<TokenType> ops{
      TokenType::Dot,
      TokenType::LeftParen,
      TokenType::LeftSquareBracket,
      TokenType::ExclusiveRange,
      TokenType::Impl,
    };

    return isTokenSubType(
             type,
             TokenType::BINOP_START,
             TokenType::BINOP_ASSIGN_END
           ) ||
           ops.contains(type);
  }

  bool isClosingToken() const {
    // TODO: update this as new closing tokens get added
    static std::unordered_set<TokenType> closingTokens{
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
    return closingTokens.contains(this->type);
  }

  bool canApply() const {
    return !(isClosingToken() || isBinaryOp(type));
  }

  bool isBuiltin() const {
    static unordered_set<TokenType> builtinTokens = {
      TokenType::BUILTIN_RegisterType,
      TokenType::BUILTIN_NumCast,
      TokenType::BUILTIN_BitCast,
      TokenType::BUILTIN_CDefine,
      TokenType::BUILTIN_CInclude,
      TokenType::BUILTIN_CIncludeDir,
      TokenType::BUILTIN_Link,
      TokenType::BUILTIN_LinkDir,
      TokenType::BUILTIN_CImport,
      TokenType::BUILTIN_Type,
    };

    return builtinTokens.contains(this->type);
  }

  bool isLiteral() const {
    static unordered_set<TokenType> literals = {
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
    };

    return literals.contains(this->type);
  }

  static optional<TokenType> binopFromCompoundAssignment(TokenType type) {
    auto rawBinop = TokenType(
      (int)type - (int)TokenType::BINOP_ASSIGN_START +
      (int)TokenType::BINOP_START
    );
    if (isOpenBinop(rawBinop)) return rawBinop;
    return std::nullopt;
  }

  static optional<TokenType> compoundAssignmentFromBinop(TokenType type) {
    if (isOpenBinop(type)) {
      return TokenType(
        (int)type + (int)TokenType::BINOP_ASSIGN_START -
        (int)TokenType::BINOP_START
      );
    }
    return std::nullopt;
  }
};

struct Tokenizer {
  static std::unordered_map<std::string_view, TokenType> builtinFunctions;
  static std::unordered_map<std::string_view, TokenType> keywords;
  Logger log;
  bool isDone = false;
  int start = 0;
  int current = 0;
  int line = 1;
  const std::string_view sourceCode;
  fs::path& inputFilePath;

  bool isAtEnd() {
    return current >= sourceCode.length();
  }

  bool isAlphaUnder(char c) {
    return isalpha(c) || c == '_';
  }

  void scanToken() {
    start = current;
    char c = advance();
    switch (c) {
    case '^': {
      addToken(TokenType::Pointer);
      break;
    }
    case '.': {
      // TODO: inclusive range
      if (peek() == '<') {
        advance();
        addToken(TokenType::ExclusiveRange);
      } else if (isdigit(peek())) {
        number(true);
      } else {
        addToken(TokenType::Dot);
      }
      break;
    }
    case '\'': {
      start++;
      if (peek() == '\\') {
        advance();
        if (peek() == '\\') {
          advance();
        } else {
          TODO("Char escape codes");
        }
      } else {
        advance();
      }
      if (peek() != '\'') {
        crash(
          "Expected terminating ' for character token, but found '{}'",
          peek()
        );
      }
      addToken(TokenType::Char);
      advance();
      break;
    }
    case '(': {
      addToken(TokenType::LeftParen);
      break;
    }
    case ')': {
      addToken(TokenType::RightParen);
      break;
    }
    case '{': {
      addToken(TokenType::LeftCurlyBrace);
      break;
    }
    case '}': {
      addToken(TokenType::RightCurlyBrace);
      break;
    }
    case ':': {
      addToken(TokenType::Colon);
      break;
    }
    case ' ':
    case '\r':
    case '\t': {
      // Ignore whitespace.
      break;
    }
    case '\n': {
      line++;
      firstCharacterOnLine.push_back(current);
      addToken(TokenType::StatementBreak);
      while (!isAtEnd() && peek() == '\n') {
        advance();
        line++;
        firstCharacterOnLine.push_back(current);
      }
      break;
    }
    case '"': {
      start++;
      string();
      break;
    }
    case '!': {
      if (peek() == '=') {
        advance();
        addToken(TokenType::NotEqual);
      } else {
        addToken(TokenType::Not);
      }
      break;
    }
    case ',': {
      addToken(TokenType::Comma);
      break;
    }
    case '[': {
      if (peek() == '^' && peek(1) && ']') {
        advance();
        advance();
        addToken(TokenType::MultiPointer);
      } else {
        addToken(TokenType::LeftSquareBracket);
      }
      break;
    }
    case ']': {
      addToken(TokenType::RightSquareBracket);
      break;
    }
    case '*': {
      addToken(TokenType::Mult);
      break;
    }
    case '/': {
      addToken(TokenType::Div);
      break;
    }
    case '+': {
      addToken(TokenType::Plus);
      break;
    }
    case '-': {
      if (peek() == '>') {
        advance();
        addToken(TokenType::ThinArrow);
      } else if (peek() == '-' && peek(1) == '-') {
        advance();
        advance();
        addToken(TokenType::Undef);
      } else {
        addToken(TokenType::Minus);
      }
      break;
    }
    case '%': {
      addToken(TokenType::Remainder);
      break;
    }
    // Comment
    case '#': {
      while (peek() != '\n')
        advance();
      break;
    }
    case '=': {
      if (peek() == '=') {
        advance();
        addToken(TokenType::DoubleEqual);
      } else if (peek() == '>') {
        advance();
        addToken(TokenType::FatArrow);
      } else {
        addToken(TokenType::Assign);
      }
      break;
    }
    case '|': {
      addToken(TokenType::BitOr);
      break;
    }
    case '&': {
      addToken(TokenType::BitAnd);
      break;
    }
    case '<': {
      if (peek() == '=') {
        advance();
        addToken(TokenType::Leq);
      } else if (peek() == '<') {
        advance();
        addToken(TokenType::ShiftLeft);
      } else if (peek() == ':') {
        advance();
        addToken(TokenType::Subtype);
      } else {
        addToken(TokenType::Lt);
      }
      break;
    }
    case '>': {
      if (peek() == '=') {
        advance();
        addToken(TokenType::Geq);
      } else if (peek() == '>') {
        advance();
        addToken(TokenType::ShiftRight);
      } else {
        addToken(TokenType::Gt);
      }
      break;
    }
    case '~': {
      addToken(TokenType::Xor);
      break;
    }
    case '@': {
      start++;
      while (isAlphaUnder(peek())) {
        advance();
      }

      if (builtinFunctions.contains(lexeme())) {
        addToken(builtinFunctions[lexeme()]);
      } else {
        crash("Unknown builtin: {}", lexeme());
      }

      break;
    }
    case '\\': {
      if (peek() == '"') {
        advance();
        start += 2;
        while (peek() != '\n')
          advance();
        addToken(TokenType::MultiLineString);
      } else {
        addToken(TokenType::LeftDiv);
      }
      break;
    }
    default:
      // c-style/null terminated string
      if (c == 'c' && peek() == '"') {
        advance();
        // Remove starting " from the token
        start += 2;
        string(TokenType::NullTerminatedString);
      } else if (isalpha(c) || c == '_') {
        identifier();
      } else if (c == '0' && peek() == 'x') {
        advance();
        start += 2;
        hex();
      } else if (isdigit(c)) {
        number();
      } else {
        crash("Unexpected character '{}'", c);
      }
    }
  }

  char advance() {
    return sourceCode[current++];
  }

  char peek(u32 ahead = 0) {
    return sourceCode[current + ahead];
  }

  void string(TokenType tokenType = TokenType::String, char endChar = '"') {
    bool isEscaping = false;
    while ((isEscaping || peek() != endChar) && !isAtEnd()) {
      if (peek() == '\n') {
        line++;
        firstCharacterOnLine.push_back(current);
      }
      if (!isEscaping) {
        isEscaping = advance() == '\\';
      } else {
        isEscaping = false;
        advance();
      }
    }

    if (isAtEnd()) {
      crash("Unterminated string");
    }

    addToken(tokenType);

    // Grab closing double quote
    advance();
  }

  void identifier() {
    while (!isAtEnd()) {
      char c = peek();
      if (!(isalnum(c) || c == '_')) break;
      advance();
    }
    if (keywords.contains(lexeme()) > 0) {
      addToken(keywords[lexeme()]);
      return;
    }
    addToken(TokenType::Identifier);
  }

  void number(bool hasDecimal = false) {
    while (!isAtEnd()) {
      char c = peek();
      if (!(isdigit(c) || c == '.')) break;
      if (c == '.') {
        if (peek(1) == '<') {
          addToken(TokenType::Integer);
          return;
        }
        if (hasDecimal) {
          crash("Encountered second decimal point when parsing number");
        }
        hasDecimal = true;
      }
      advance();
    }
    TokenType tokenType = hasDecimal ? TokenType::Decimal : TokenType::Integer;
    addToken(tokenType);
  }

  static bool isHex(char c) {
    return isdigit(c) || (c <= 'F' && c >= 'A') || (c <= 'f' && c >= 'a');
  }

  void hex() {
    while (!isAtEnd()) {
      char c = peek();
      if (!isHex(c)) break;
      advance();
    }
    addToken(TokenType::HexInt);
  }

  std::string_view lexeme() const {
    return sourceCode.substr(start, current - start);
  }

  void addToken(TokenType type) {
    auto binopAssign = Token::compoundAssignmentFromBinop(type);
    if (binopAssign && peek() == '=') {
      advance();
      Token token{
        .lexeme = lexeme(),
        .line = line,
        .type = binopAssign.value()
      };
      tokens.push_back(token);
      log("Adding binop=:{}; '{}'", (int)token.type, token.lexeme);
    } else {
      tokens.push_back({.lexeme = lexeme(), .line = line, .type = type});
    }
  }

public:
  Tokenizer(const std::string_view& sourceCode, fs::path& inputFile)
      : sourceCode(sourceCode), tokens(), firstCharacterOnLine(),
        inputFilePath(inputFile), log(LogLevel::Tokenize) {
    firstCharacterOnLine.push_back(0);
    while (!isAtEnd()) {
      scanToken();
    }
    firstCharacterOnLine.push_back(current);
  }

  std::vector<Token> tokens;
  std::vector<u32> firstCharacterOnLine;

  struct TokenLocation {
    u32 line;
    u32 column;
    std::string_view lineContents;
    std::string_view lexeme;

    void underline(std::ostream& out) {
      u32 tabCount = 0;
      for (auto c : lineContents.substr(0, column)) {
        if (c == '\t') tabCount++;
      }
      fmt::println(out, "{: >8}| {}", line, lineContents);
      fmt::print(out, "{: >8}| ", "");
      fmt::print(out, "{:\t>{}}", "", tabCount);
      fmt::println(
        out,
        "{: >{}}{:^>{}}",
        "",
        column - tabCount,
        "",
        lexeme.size()
      );
    }
  };

  TokenLocation locationOf(std::string_view lexeme) const {
    // i32 charIndex = lexeme.data() - sourceCode.data();
    u32 charIndex = lexeme.begin() - sourceCode.begin();
    auto line = std::lower_bound(
      firstCharacterOnLine.begin(),
      firstCharacterOnLine.end(),
      charIndex
    );
    u32 lineNumber = line - firstCharacterOnLine.begin();
    u32 lineStartIndex = (line - 1)[0];
    return {
      .line = lineNumber,
      .column = charIndex - lineStartIndex,
      .lineContents =
        sourceCode.substr(lineStartIndex, *line - lineStartIndex - 1),
      .lexeme = lexeme
    };
  }

  template <typename... Args>
  [[noreturn]] void crash(
    fmt::format_string<Args...> fmt,
    Args&&... args
  ) const {
    auto& out = std::cerr;
    auto location = locationOf(lexeme());
    fmt::println(
      out,
      "Tokenizer error in file {} at line {}:{}",
      inputFilePath.string(),
      location.line,
      location.column
    );
    location.underline(out);
    fmt::println(out, fmt, std::forward<Args>(args)...);
    abort();
  }
};

// TEST_CASE("Bro") {
//   fmt::println("Binop start: {}", (int)TokenType::BINOP_START);
//   fmt::println("Double equal: {}", (int)TokenType::DoubleEqual);
//   fmt::println("Binop end: {}", (int)TokenType::BINOP_END);
//   fmt::println("Shift right: {}", (int)TokenType::ShiftRight);
//   fmt::println("Binop=start: {}", (int)TokenType::BINOP_ASSIGN_START);
//   fmt::println("Binop=end: {}", (int)TokenType::BINOP_ASSIGN_END);
//   FAIL("failing");
// }
