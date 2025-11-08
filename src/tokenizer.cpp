#include "tokenizer.h"
using namespace Tokenization;
std::unordered_map<std::string_view, TokenType> Tokenizer::builtinFunctions = {
  {"numCast",      TokenType::BUILTIN_NumCast     },
  {"registerType", TokenType::BUILTIN_RegisterType},
  {"cDefine",      TokenType::BUILTIN_CDefine     },
  {"cInclude",     TokenType::BUILTIN_CInclude    },
  {"cIncludeDir",  TokenType::BUILTIN_CIncludeDir },
};
std::unordered_map<std::string_view, TokenType> Tokenizer::keywords = {
  {"fn",     TokenType::Function},
  {"struct", TokenType::Struct  },
  {"if",     TokenType::If      },
  {"else",   TokenType::Else    },
  {"true",   TokenType::True    },
  {"false",  TokenType::False   },
  {"and",    TokenType::LogicAnd},
  {"or",     TokenType::LogicOr },
  {"for",    TokenType::For     },
  {"while",  TokenType::While   },
  {"import", TokenType::Import  },
  {"union",  TokenType::Union   },
  {"enum",   TokenType::Enum    },
  {"trait",  TokenType::Trait   },
  {"impl",   TokenType::Impl    },
  {"mut",    TokenType::Mut     },
  {"opaque", TokenType::Opaque  },
  {"gn",     TokenType::Generic },
};
