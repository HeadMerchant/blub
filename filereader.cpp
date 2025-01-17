#include <iostream>
#include <fstream>
#include <string>

std::string readFile() {
    std::ifstream inputFile("testinput");

    if (!inputFile.is_open()) {
        std::cerr << "Error: could not open the file." << std::endl;
        return "<couldn't read file>";
    }

    std::string line;
    std::string fileContents;
    std::getline(inputFile, fileContents);
    while (std::getline(inputFile, line)) {
        fileContents += "\n" + line;
    }

    return fileContents;
}