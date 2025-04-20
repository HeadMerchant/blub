#include "tokenizer.h"
#include "parser.h"
#include <vector>

struct ASTNode {
    ASTNodeType type;
    int tokenIndex;
};

enum ASTNodeType {
    EXPRESSION,
    EQUALITY,
    COMPARISON,
    TERM,
    FACTOR,
    UNARY,
    CALL,
    PRIMARY,
};

struct LiteralString : ASTNode {
    std::string& value;
};

struct LiteralInt : ASTNode {
    int value;
};

struct LiteralBool : ASTNode {
    bool value;
};

struct BinaryExpression : ASTNode {
    const ASTNode& left;
    const ASTNode& right;
};


class Parser {
    private:
        const std::vector<Token>& tokens;
        int current = 0;
        const Token& peek() {
            return tokens[current];
        }

        const Token& advance() {
            return tokens[current++];
        }

        bool check(TokenType type) {
            if (isAtEnd()) {
                return false;

            }
            return peek().type == type;
        }

        bool isAtEnd() {
            return peek().type == EOF;
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
    
    public:
        Parser(std::vector<Token>& tokens) : tokens(tokens) {}

        ASTNode parse() {
            return expression();
        }

    private:
        ASTNode expression() {
            return equality();
        }

        ASTNode equality() {
            return comparison();
        }

        ASTNode comparison() {
            return term();
        }

        ASTNode term() {
            return factor();
        }

        ASTNode factor() {
            return unary();
        }

        ASTNode unary() {
            return primary();
        }

        ASTNode primary() {
            if (check())
        }
};
