#include <string>
#include <stdexcept>
#include <iostream>
#include <vector>
#include "tokenizer.h"
#include <unordered_map>


bool isAlphaUnder(char c) {
    return isalpha(c) || c == '_';
}

std::unordered_map<std::string, TokenType> keywords = {
    {"fun", FUNCTION}
};

class Tokenizer {
    private:
        bool isDone = false;
        int start = 0;
        int current = 0;
        int line = 1;
        const std::string& sourceCode;
    
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
        while (peek() != '"' && !isAtEnd())
        {
            advance();
        }

        if (isAtEnd()) {
            throw std::invalid_argument("Unterminated string");
        }

        // Grab closing double quote
        advance();
        addToken(STRING);
    }

    void identifier() {
        while (!isAtEnd())
        {
            char c = peek();
            if (!(isalnum(c) || c == '_')) break;
            advance();
        }
        addToken(IDENTIFIER);
    }

    void addToken(TokenType type) {
        tokens.push_back({
            .type = type,
            .lexeme = sourceCode.substr(start, current-start),
            .line = line
        });
    }

    public: Tokenizer(const std::string& sourceCode): sourceCode(sourceCode) {
        while (!isAtEnd())
        {
            scanToken();
        }
    }

    std::vector<Token> tokens;
};

std::vector<Token> tokenize(const std::string& srcFile) {
    std::cout << "Source:\n" << srcFile << std::endl;
    return Tokenizer(srcFile).tokens;
}
