#pragma once
#include <string>
#include <vector>

enum TokenType {
    // Symbols
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_CURLY_BRACE,
    RIGHT_CURLY_BRACE,
    COLON,
    CONSTANT_DECLARATION,
    DOUBLE_EQUAL,
    NOT_EQUAL,
    NOT,

    // Keywords
    FUNCTION,

    // Literal
    IDENTIFIER,
    STRING,
    NUMBER
};

struct Token {
    const TokenType type;
    const std::string lexeme;
    const int line;
};

std::vector<Token> tokenize(const std::string& srcFile);
