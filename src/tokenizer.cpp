#include <sstream>
#include <stdexcept>
#include <iostream>
#include <vector>
#include "tokenizer.h"
#include "logging.h"
#include <unordered_map>
#include <string_view>

bool isAlphaUnder(char c) {
    return isalpha(c) || c == '_';
}

namespace Log {
    std::ostream& log = logger(LogLevel::DEBUG);
}

std::unordered_map<std::string_view, TokenType> keywords = {
    {"fn", TokenType::FUNCTION},
    {"struct", TokenType::STRUCT},
    {"if", TokenType::IF},
    {"else", TokenType::ELSE},
    {"true", TokenType::TRUE},
    {"false", TokenType::FALSE},
    {"and", TokenType::LOGICAL_AND},
    {"or", TokenType::LOGICAL_OR},
    {"for", TokenType::FOR},
    {"while", TokenType::WHILE},
    {"xor", TokenType::XOR},
    // use
    // todo: union, enum, trait, import, impl, loop, match, if/else, mut, pointer
};

class Tokenizer {
    private:
        bool isDone = false;
        int start = 0;
        int current = 0;
        int line = 1;
        const std::string_view sourceCode;
    
    bool isAtEnd() {
        return current >= sourceCode.length();
    }

    void scanToken() {
        start = current;
        char c = advance();
        switch (c)
        {
        case '^': {
            addToken(TokenType::POINTER);
            break;
        }
        case '.': {
            addToken(TokenType::DOT);
            break;
        }
        case '(':{
            addToken(TokenType::LEFT_PAREN);
            break;
        }
        case ')':{
            addToken(TokenType::RIGHT_PAREN);
            break;
        }
        case '{':{
            addToken(TokenType::LEFT_CURLY_BRACE);
            break;
        }
        case '}':{
            addToken(TokenType::RIGHT_CURLY_BRACE);
            break;
        }
        case ':':{
            addToken(TokenType::COLON);
            break;
        }
        case ' ':
        case '\r':
        case '\t':{
            // Ignore whitespace.
            break;
        }
        case '\n':{
            line++;
            addToken(TokenType::STATEMENT_BREAK);
            while (!isAtEnd() && peek() == '\n') {
                advance();
                line++;
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
                addToken(TokenType::NOT_EQUAL);
            } else {
                addToken(TokenType::NOT);
            }
            break;
        }
        case ',': {
            addToken(TokenType::COMMA);
            break;
        }
        case '[': {
            addToken(TokenType::LEFT_BRACKET);
            break;
        }
        case ']': {
            addToken(TokenType::RIGHT_BRACKET);
            break;
        }
        case '*': {
            addToken(TokenType::MULT);
            break;
        }
        case '/': {
            addToken(TokenType::DIV);
            break;
        }
        case '+': {
            addToken(TokenType::PLUS);
            break;
        }
        case '-': {
            addToken(TokenType::MINUS);
            break;
        }
        // Comment
        case '#': {
            while (peek() != '\n') advance();
            break;
        }
        case '=': {
            if (peek() == '=') {
                advance();
                addToken(TokenType::EQUALITY);
            } else {
                addToken(TokenType::ASSIGNMENT);
            }
            break;
        }
        case '|': {
            addToken(TokenType::BITWISE_OR);
            break;
        }
        case '&': {
            addToken(TokenType::BITWISE_AND);
            break;
        }
        case '<': {
            if (peek() == '=') {
                advance();
                addToken(TokenType::LEQ);
            } else {
                addToken(TokenType::LESS_THAN);
            }
            break;
        }
        case '>': {
            if (peek() == '=') {
                advance();
                addToken(TokenType::GEQ);
            } else {
                addToken(TokenType::GREATER_THAN);
            }
            break;
        }
        default:
            // c-style/null terminated string
            if (c == 'c' && peek() == '"') {
                advance();
                // Remove starting " from the token
                start += 2;
                string(TokenType::NULL_TERMINATED_STRING);
            } else if(isalpha(c) || c == '_') {
                identifier();
            } else if (isdigit(c)) {
                number();
            } else {
                std::stringstream ss;
                ss << "Unexpected character: (" << c << ") at line " << line;
                throw std::invalid_argument(ss.str());
            }
        }
    }

    char advance() {
        return sourceCode[current++];
    }

    char peek() {
        return sourceCode[current];
    }

    void string(TokenType tokenType = TokenType::STRING) {
        while (peek() != '"' && !isAtEnd())
        {
            advance();
        }

        if (isAtEnd()) {
            throw std::invalid_argument("Unterminated string");
        }

        addToken(tokenType);

        // Grab closing double quote
        advance();
    }

    void identifier() {
        while (!isAtEnd())
        {
            char c = peek();
            if (!(isalnum(c) || c == '_')) break;
            advance();
        }
        if (keywords.count(lexeme()) > 0) {
            addToken(keywords[lexeme()]);
            return;
        }
        addToken(TokenType::IDENTIFIER);
    }

    void number() {
        bool hasDecimal = false;
        while (!isAtEnd())
        {
            char c = peek();
            if (!(isdigit(c) || c == '.')) break;
            if (c == '.') {
                if (hasDecimal) {
                   throw std::invalid_argument("Encountered second decimal when parsing number");
                }
                hasDecimal = true;
            }
            advance();
        }
        TokenType tokenType = hasDecimal ? TokenType::DECIMAL : TokenType::INT;
        addToken(tokenType);
    }

    std::string_view lexeme() {
        return sourceCode.substr(start, current-start);
    }

    void addToken(TokenType type) {
        tokens.push_back({
            .lexeme = lexeme(),
            .line = line,
            .type = type
        });
    }

    public: Tokenizer(const std::string_view& sourceCode, std::vector<Token>& tokens): sourceCode(sourceCode), tokens(tokens) {
        while (!isAtEnd())
        {
            scanToken();
        }
    }

    std::vector<Token>& tokens;
};

void tokenize(const std::string_view& srcFile, std::vector<Token>& tokensArray) {
    Log::log << "Source:\n" << srcFile << std::endl;
    Tokenizer(srcFile, tokensArray);
}

std::ostream& operator<<(std::ostream& os, const Token& token) {
    os << "Token(type: " << ((int) token.type) << ", lexeme: \"" << token.lexeme << "\", line: " << token.line << ")";
    return os;
}
