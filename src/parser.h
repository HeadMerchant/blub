// TODO: function calls
#pragma once
#include "tokenizer.h"
#include <vector>
#include <stdexcept>
#include <iostream>

enum class NodeType {
    DECLARATION,
    UNARY,
    LITERAL,
    IDENTIFIER,
    FUNCTION_LITERAL,
    FUNCTION_CALL,
    BLOCK,
    PARAMETER_LIST,
    CALL,
    ARGS_LIST,
    ARRAY_LITERAL,
    BINARY
};

class ASTNode {
    public:
    NodeType type;
    Token token;
    std::vector<ASTNode*> children;

    ASTNode(
        NodeType type,
        Token token,
        std::vector<ASTNode*> children
    ): type(type), token(token), children(children) {}

    ASTNode(
        NodeType type,
        Token token,
        std::initializer_list<ASTNode*> children 
    ): type(type), token(token), children(children) {}

    void print(int level = 0) {
        std::string indentation(level, '\t');
        std::cout << indentation << static_cast<int>(type) << ": ";
        token.print();
        for (auto child : children) {
            child->print(level+1);
        }
    }
};

typedef std::vector<ASTNode*> NodeList;

class Parser {
    public:
    Parser(std::vector<Token>& tokens) : tokens(tokens) {}

    private:
    const std::vector<Token>& tokens;
    int current = 0;
    
    const Token& peek(int ahead = 0) {
        return tokens[current+ahead];
    }
    
    const Token& advance() {
        if (!isAtEnd()) current++;
        const Token& token = previous();
        std::cout << "Advancing: " << token.lexeme << "\n";
        return token;
    }
    
    bool check(TokenType type, int ahead = 0) {
        if (isAtEnd(ahead)) {
            return false;
        }
        return peek(ahead).type == type;
    }
    
    bool isAtEnd(int ahead = 0) {
        return peek(ahead).type == EOF || current >= tokens.size();
    }
    
    const Token& previous() {
        return tokens[current - 1];
    }
    
    bool match(std::vector<TokenType>& types) {
        for (auto type : types) {
            if (check(type)) {
                advance();
                return true;
            }
        }
        return false;
    }
    
    const Token& consume(TokenType type, std::string message) {
        if (check(type)) return advance();
        advance().print();
        throw std::invalid_argument(message);
    }

    const Token& latestToken() {
        auto last = tokens.size() - 1;
        return tokens[last > current ? current : last];
    }
       
    public:
    std::vector<ASTNode*> parse() {
        std::vector<ASTNode*> statements;
        // try {
            while(!isAtEnd()) {
                statements.push_back(statement());
            }
        return statements;
        // } catch (const std::exception& e) {
        //     std::cerr << "Next token:\n";
        //     latestToken().print();
        //     throw e;
        // }
    }
   
    ASTNode* statement() {
        ASTNode* node;
        if (check(STATEMENT_BREAK)) {
            advance();
        }
        if (check(TokenType::IDENTIFIER) && check(TokenType::CONSTANT_DECLARATION, 1)) {
            node = declaration();
        } else {
            node = expression();
        }
        if (!isAtEnd()) {
            consume(TokenType::STATEMENT_BREAK, "Expected a breaking statement");
        }
        return node;
    }

    ASTNode* declaration() {
        if (!check(TokenType::IDENTIFIER)) {
            return expression();
        }

        auto name = new ASTNode(NodeType::LITERAL, advance(), {});
        auto token = consume(CONSTANT_DECLARATION, "Expected a :: for constant declaration");
        auto value = expression();
        NodeList children = {name, value};
        return new ASTNode(
            NodeType::DECLARATION,
            token,
            children
        );
    }

    ASTNode* expression() {
        return equality();
    }
    
    ASTNode* equality() {
        static std::vector<TokenType> types = {DOUBLE_EQUAL, NOT_EQUAL};
        
        ASTNode* expr = comparison();
        while (match(types)) {
            Token op = previous();
            ASTNode* right = comparison();
            ASTNode* left = expr;
            expr = new ASTNode(
                NodeType::BINARY,
                op,
                {left, right}
            );
        }
        return expr;
    }
    
    ASTNode* comparison() {
        static std::vector<TokenType> types = {};
        return term();
    }
    
    ASTNode* term() {
        static std::vector<TokenType> types = {PLUS, MINUS};
        auto expr = factor();
        while (match(types)) {
            Token op = previous();
            ASTNode* right = factor();
            ASTNode* left = expr;
            expr = new ASTNode(
                NodeType::BINARY,
                op,
                {left, right}
            );
        }
        return expr;
    }
    
    ASTNode* factor() {
        static std::vector<TokenType> types = {MULT, DIV};
        auto expr = unary();
        while (match(types)) {
            Token op = previous();
            ASTNode* right = unary();
            ASTNode* left = expr;
            expr = new ASTNode(
                NodeType::BINARY,
                op,
                {left, right}
            );
        }
        return expr;
    }
    
    ASTNode* unary() {
        static std::vector<TokenType> types = {NOT};
        if (match(types)) {
            Token op = previous();
            return new ASTNode(
                NodeType::UNARY,
                op,
                {call()}
            );
        }
    
        return call();
    }
   
    ASTNode* call() {
        ASTNode* expr = primary();
        static std::vector<TokenType> open = {LEFT_PAREN};
        if (match(open)) {
            std::cout << "Looking in here\n";
            auto token = previous();
            std::vector<ASTNode*> args = {expr};
            // TODO: add call args
            while (!check(RIGHT_PAREN)) {
                std::cout << "finding arg\n";
                args.push_back(expression());
            }

            consume(RIGHT_PAREN, "Expected a closing ')' after arguments for function call");

            return new ASTNode(
                NodeType::FUNCTION_CALL,
                token,
                args
            );
        }

        return expr;
    }

    ASTNode* primary() {
        static std::vector<TokenType> types = {STRING, DECIMAL, INT}; 
        if (match(types)) {
            return new ASTNode(
                NodeType::LITERAL,
                previous(),
                {}
            );
        }

        if (check(FUNCTION)) {
            return function();
        }

        if (check(IDENTIFIER)) {
            return new ASTNode(
                NodeType::IDENTIFIER,
                advance(),
                {}
            );
        }
        
        static std::vector<TokenType> leftParen = {LEFT_PAREN};
        if (match(leftParen)) {
            ASTNode* grouping = expression();
            consume(TokenType::RIGHT_PAREN, "Expected a closing parenthesis");
            return grouping;
        }

        if (check(LEFT_BRACKET)) {
            Token startToken = advance();
            std::vector<ASTNode*> items;

            // Array literal
            while (!check(RIGHT_BRACKET)) {
                if (items.size() > 0) {
                    consume(COMMA, "Expected separating comma between elements of array literal");
                }

                // Allow trailing comma
                if (check(RIGHT_BRACKET)){                    
                    break;
                }

                items.push_back(expression());
            }

            consume(RIGHT_BRACKET, "Unclosed array literal; Expected ']'");
            
            return new ASTNode(
                NodeType::ARRAY_LITERAL,
                startToken,
                items
            );
        }

        // peek().print();
        std::cout << "Last token:\n";
        latestToken().print();
        throw std::invalid_argument("Unable to parse");
    }

    ASTNode* function() {
        // Keyword
        consume(FUNCTION, "Expected 'fn' keyword");
        auto keyword = previous();
        // Params
        consume(LEFT_PAREN, "Expected '(' for parameter declaration");
        // TODO: add node for parameters
        auto params = new ASTNode(
            NodeType::PARAMETER_LIST,
            previous(),
            {}
        );
        consume(RIGHT_PAREN, "Expected ')' for declaration");
        
        // Block
        auto body = block();
        return new ASTNode(
            NodeType::FUNCTION_LITERAL,
            keyword,
            {params, body}
        );
    }

    ASTNode* block() {
        consume(TokenType::LEFT_CURLY_BRACE, "Expected '{'");
        auto startToken = previous();
        std::vector<ASTNode*> statements;
        std::vector<TokenType> types = {TokenType::RIGHT_CURLY_BRACE};
        while (!match(types)) {
            std::cout << "looping\n";
            // peek().print();
            // std::cout << "Finding statements\n";
            statements.push_back(statement());
        }
        // consume(TokenType::RIGHT_CURLY_BRACE, "Expected '}'");
        return new ASTNode(
            NodeType::BLOCK,
            startToken,
            statements
        );
    }
};

std::vector<ASTNode*> parseTokens(std::vector<Token>& tokens);