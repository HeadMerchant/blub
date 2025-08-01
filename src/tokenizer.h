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

    // Unary ops
    POINTER,
    DOT,
    NOT,
    
    // Control flow
    IF,
    ELSE,
    FOR,
    WHILE,

    // Literals
    TRUE,
    FALSE,
    
    // CONSTANT_DECLARATION,
    ASSIGNMENT,
    DOUBLE_EQUAL,
    NOT_EQUAL,
    EQUALITY,
    LESS_THAN,
    GREATER_THAN,
    LEQ,
    GEQ,

    // Binops
    PLUS,
    MINUS,
    MULT,
    DIV,
    LOGICAL_AND,
    LOGICAL_OR,
    BITWISE_AND,
    BITWISE_OR,
    XOR,
    REMAINDER,
    SHIFT_LEFT,
    SHIFT_RIGHT,
    ARITHMETIC_SHIFT_RIGHT,

    // Keywords
    FUNCTION,
    STRUCT,

    // Literal
    IDENTIFIER,
    STRING,
    NULL_TERMINATED_STRING,
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
};

std::ostream& operator<<(std::ostream& os, const Token& token);

void tokenize(const std::string_view& srcFile, std::vector<Token>& tokensArray);
