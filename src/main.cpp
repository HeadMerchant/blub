#include "filereader.h"
#include <string>
#include <stdexcept>
#include "tokenizer.h"
#include "parser/parser.h"
#include "interpreter.h"

int main(int argc, char *argv[]) {
    // no source file passed in
    if (argc < 2) {
        throw std::invalid_argument("missing source file input argument");
    }

    std::string sourceFile(argv[1]);

    std::string fileContents = readFile(sourceFile);

    std::vector<Token> tokens;
    tokenize(fileContents, tokens);
    for (auto token : tokens) {
        token.print();
    }


    Parser parser(tokens);

    std::vector<NodeIndex> program = parser.parse();

    // for (auto ast : program) {
    //     ast->print();
    // }

    Interpreter interpreter(parser, program);
    interpreter.run();
}
