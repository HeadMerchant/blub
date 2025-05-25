#include "greeting.h"
#include "filereader.h"
#include <iostream>
#include <string>
#include <stdexcept>
#include "tokenizer.h"
#include "parser.h"
#include "interpreter.h"

int main(int argc, char *argv[]) {
    printGreeting();

    // no source file passed in
    if (argc < 2) {
        throw std::invalid_argument("missing source file input argument");
    }

    std::string sourceFile(argv[1]);

    std::string fileContents = readFile(sourceFile);

    std::vector<Token> tokens;
    tokenize(fileContents, tokens);
    std::cout << tokens.size() << "\n";
    std::cout << "Address: " << &tokens << "\n";

    for (auto token : tokens) {
        token.print();
    }

    std::vector<ASTNode*> program = parseTokens(tokens);
    for (auto ast : program) {
        ast->print();
    }

    Interpreter interpreter(program);
    interpreter.run();
}
