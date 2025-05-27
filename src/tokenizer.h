#pragma once
#include <vector>
#include <string_view>
#include <iostream>

enum class TokenType {
    // Symbols
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_CURLY_BRACE,
    RIGHT_CURLY_BRACE,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    COMMA,
    COLON,
    CONSTANT_DECLARATION,
    DOUBLE_EQUAL,
    NOT_EQUAL,
    NOT,

    // Binops
    PLUS,
    MINUS,
    MULT,
    DIV,

    // Keywords
    FUNCTION,

    // Literal
    IDENTIFIER,
    STRING,
    DECIMAL,
    INT,
    
    // TODO: figure out meaningful whitespace
    STATEMENT_BREAK,
    END_OF_FILE
};

struct Token {
    const std::string_view lexeme;
    const int line;
    const TokenType type;

    void print() const {
        std::printf("Token(type:%d, lexeme:\"%.*s\", line:%d)", type, lexeme.length(), lexeme.data(), line);
        std::cout << "\n";
    }
};

void tokenize(const std::string_view& srcFile, std::vector<Token>& tokensArray);
