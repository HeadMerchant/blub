#include <string>
#include <stdexcept>
#include <iostream>
#include <vector>
#include "tokenizer.h"
#include <unordered_map>
#include <string_view>

bool isAlphaUnder(char c) {
    return isalpha(c) || c == '_';
}

std::unordered_map<std::string_view, TokenType> keywords = {
    {"fn", FUNCTION}
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
        case '(':{
            addToken(LEFT_PAREN);
            break;
        }
        case ')':{
            addToken(RIGHT_PAREN);
            break;
        }
        case '{':{
            addToken(LEFT_CURLY_BRACE);
            break;
        }
        case '}':{
            addToken(RIGHT_CURLY_BRACE);
            break;
        }
        case ':':{
            if (peek() == ':') {
                advance();
                addToken(CONSTANT_DECLARATION);
            } else {
                addToken(COLON);
            }
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
            // addToken(STATEMENT_BREAK);
            // while (peek() == '\n') {
            //     advance();
            //     line++;
            // }
            break;
        }
        case ';': {
            addToken(STATEMENT_BREAK);
            break;
        }
        case '"': {
            string();
            break;
        }
        case '!': {
            if (peek() == '=') {
                advance();
                addToken(NOT_EQUAL);
            } else {
                addToken(NOT);
            }
            break;
        }
        
        default:
            if(isalpha(c) || c == '_') {
                identifier();
            } else {
                throw std::invalid_argument("Unexpected character: (" + std::string {c} + ") at line "+std::to_string(line)+"\n");
            }
        }
    }

    char advance() {
        return sourceCode[current++];
    }

    char peek() {
        return sourceCode[current];
    }

    void string() {
        // Remove starting " from the token
        start++;
        while (peek() != '"' && !isAtEnd())
        {
            advance();
        }

        if (isAtEnd()) {
            throw std::invalid_argument("Unterminated string");
        }

        addToken(STRING);

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
        addToken(IDENTIFIER);
    }

    std::string_view lexeme() {
        return sourceCode.substr(start, current-start);
    }

    void addToken(TokenType type) {
        tokens.push_back({
            .type = type,
            .lexeme = lexeme(),
            .line = line
        });
        std::cout << tokens.size() << "\n";
    }

    public: Tokenizer(const std::string_view& sourceCode, std::vector<Token>& tokens): sourceCode(sourceCode), tokens(tokens) {
        std::cout << &tokens << "\n";
        while (!isAtEnd())
        {
            scanToken();
        }
    }

    std::vector<Token>& tokens;
};

void tokenize(const std::string_view& srcFile, std::vector<Token>& tokensArray) {
    std::cout << "Source:\n" << srcFile << std::endl;
    Tokenizer(srcFile, tokensArray);
}
