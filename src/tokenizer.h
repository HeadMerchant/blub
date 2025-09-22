#pragma once
#include "common.h"
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
    SUBTYPE,

    DOUBLE_EQUAL,
    NOT_EQUAL,
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

    // Keywords
    FUNCTION,
    STRUCT,
    IMPORT,
    UNION,
    ENUM,
    TRAIT,
    IMPL,
    MUT,
    OPAQUE,

    // Literal
    IDENTIFIER,
    STRING,
    NULL_TERMINATED_STRING,
    DECIMAL,
    INT,

    // Builtins
    BUILTIN_NumCast,
    BUILTIN_RegisterType,
    
    // TODO: figure out meaningful whitespace
    STATEMENT_BREAK,
    END_OF_FILE
};

struct Token {
    public:
    const std::string_view lexeme;
    const int line;
    const TokenType type;

    bool isArithmeticOperation() const {
        return
            type == TokenType::DOUBLE_EQUAL ||
            type == TokenType::NOT_EQUAL ||
            type == TokenType::LESS_THAN ||
            type == TokenType::GREATER_THAN ||
            type == TokenType::LEQ ||
            type == TokenType::GEQ ||
            type == TokenType::PLUS ||
            type == TokenType::MINUS ||
            type == TokenType::MULT ||
            type == TokenType::DIV;
    }
};

std::ostream& operator<<(std::ostream& os, const Token& token);

void tokenize(const std::string_view& srcFile, std::vector<Token>& tokensArray, std::vector<i32>& linesArray);
