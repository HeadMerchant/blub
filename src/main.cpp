#include "greeting.h"
#include "filereader.h"
#include <iostream>
#include <string>
#include <stdexcept>
#include "tokenizer.cpp"

int main(int argc, char *argv[]) {
    printGreeting();

    // no source file passed in
    if (argc < 2) {
        throw std::invalid_argument("missing source file input argument");
    }

    std::string sourceFile(argv[1]);

    std::string fileContents = readFile(sourceFile);

    std::vector<Token> tokens = tokenize(fileContents);

    for (auto token : tokens) {
        std::printf("Token(type:%d, lexeme:\"%s\", line:%d)", token.type, token.lexeme.c_str(), token.line);
        std::cout << "\n";
    }
}
