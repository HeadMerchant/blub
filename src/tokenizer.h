#pragma once
#include "common.h"
#include "fmt/format.h"
#include "fmt/ostream.h"
#include <iostream>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace Tokenization {

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
  Dot,
  Not,

  // Control flow
  If,
  Else,
  For,
  While,

  // Literals
  True,
  False,

  // CONSTANT_DECLARATION,
  Assign,
  Subtype,

  DoubleEqual,
  NotEqual,
  Lt,
  Gt,
  Leq,
  Geq,

  // Binops
  Plus,
  Minus,
  Mult,
  Div,
  MatrixDiv,
  LogicAnd,
  LogicOr,
  BitAnd,
  BitOr,
  Xor,
  Remainder,
  ShiftLeft,
  ShiftRight,
  ThinArrow,

  // Keywords
  Function,
  Struct,
  Import,
  Union,
  Enum,
  Trait,
  Impl,
  Mut,
  Opaque,
  Generic,

  // Literal
  Identifier,
  String,
  NullTerminatedString,
  Decimal,
  Integer,
  MultiLineString,

  // Builtins
  BUILTIN_NumCast,
  BUILTIN_RegisterType,
  BUILTIN_CInclude,
  BUILTIN_CDefine,
  BUILTIN_CIncludeDir,
  BUILTIN_CLink,
  BUILTIN_CImport,

  // CUDA
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

  bool isArithmeticOperation() const {
    return type == TokenType::DoubleEqual || type == TokenType::NotEqual || type == TokenType::Lt || type == TokenType::Gt || type == TokenType::Leq ||
           type == TokenType::Geq || type == TokenType::Plus || type == TokenType::Minus || type == TokenType::Mult || type == TokenType::Div;
  }
};

struct Tokenizer {
  static std::unordered_map<std::string_view, TokenType> builtinFunctions;
  static std::unordered_map<std::string_view, TokenType> keywords;
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
      addToken(TokenType::Dot);
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
      addToken(TokenType::LeftSquareBracket);
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
      } else {
        addToken(TokenType::Minus);
      }
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
        string(TokenType::MultiLineString, '\n');
        firstCharacterOnLine.push_back(current);
      } else {
        addToken(TokenType::MatrixDiv);
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

  char peek() {
    return sourceCode[current];
  }

  void string(TokenType tokenType = TokenType::String, char endChar = '"') {
    bool isEscaping = false;
    while ((isEscaping || peek() != endChar) && !isAtEnd()) {
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

  void number() {
    bool hasDecimal = false;
    while (!isAtEnd()) {
      char c = peek();
      if (!(isdigit(c) || c == '.')) break;
      if (c == '.') {
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

  std::string_view lexeme() const {
    return sourceCode.substr(start, current - start);
  }

  void addToken(TokenType type) {
    tokens.push_back({.lexeme = lexeme(), .line = line, .type = type});
  }

public:
  Tokenizer(const std::string_view& sourceCode, fs::path& inputFile) : sourceCode(sourceCode), tokens(), firstCharacterOnLine(), inputFilePath(inputFile) {
    firstCharacterOnLine.push_back(0);
    while (!isAtEnd()) {
      scanToken();
    }
    firstCharacterOnLine.push_back(current);
  }

  std::vector<Token> tokens;
  std::vector<i32> firstCharacterOnLine;

  struct TokenLocation {
    i32 line;
    i32 column;
    std::string_view lineContents;
    std::string_view lexeme;

    void underline(std::ostream& out) {
      fmt::println(out, "{: >8}| {}", column, lineContents);
      // fmt::println(out, "{: >8}| {: >{}}{:^{}}", "", "", column, "", lexeme.size());
      fmt::println(out, "{: >8}| {: >{}}^^^^^", "", "", column);
    }
  };

  TokenLocation locationOf(std::string_view lexeme) const {
    // i32 charIndex = lexeme.data() - sourceCode.data();
    i32 charIndex = lexeme.begin() - sourceCode.begin();
    auto line = std::lower_bound(firstCharacterOnLine.begin(), firstCharacterOnLine.end(), charIndex);
    i32 lineNumber = line - firstCharacterOnLine.begin();
    i32 lineStartIndex = (line - 1)[0];
    return {.line = lineNumber, .column = charIndex - lineStartIndex, .lineContents = sourceCode.substr(lineStartIndex, *line - lineStartIndex - 1), .lexeme = lexeme};
  }

  template <typename... Args> [[noreturn]] void crash(fmt::format_string<Args...> fmt, Args&&... args) const {
    auto& out = std::cerr;
    auto location = locationOf(lexeme());
    fmt::println(out, "Tokenizer error in file {} at line {}:{}", inputFilePath.string(), location.line, location.column);
    location.underline(out);
    fmt::println(out, fmt, std::forward<Args>(args)...);
    exit(1);
  }
};
}; // namespace Tokenization
