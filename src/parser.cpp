#include "parser.h"
std::unordered_map<TokenType, UnaryOps> Parser::builtinUnary = {
  {TokenType::BUILTIN_Sizeof,  UnaryOps::SizeOf },
  {TokenType::BUILTIN_Alignof, UnaryOps::AlignOf},
  {TokenType::BUILTIN_Bitsize, UnaryOps::BitSize},
  {TokenType::BUILTIN_Type,    UnaryOps::Type   },
};
