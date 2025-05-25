#include "tokenizer.h"
#include "parser.h"
#include <vector>

std::vector<ASTNode*> parseTokens(std::vector<Token>& tokens) {
    return Parser(tokens).parse();
}
