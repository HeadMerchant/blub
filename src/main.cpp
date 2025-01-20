#include "greeting.h"
#include "filereader.h"
#include <iostream>
#include <string>
#include <stdexcept>

int main(int argc, char *argv[]) {
    printGreeting();

    // no source file passed in
    if (argc < 2) {
        throw std::invalid_argument("missing source file input argument");
    }

    std::string sourceFile(argv[1]);

    std::cout << readFile(sourceFile) << std::endl;
}
