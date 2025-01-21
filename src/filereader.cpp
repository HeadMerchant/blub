#include <iostream>
#include <fstream>
#include <string>

std::string readFile(std::string filePath) {
    std::ifstream inputFile(filePath);

    if (!inputFile.is_open()) {
        std::cerr << "Error: could not open the file." << std::endl;
        return "<couldn't read file>";
    }

    std::string fileContents = std::string(
        std::istreambuf_iterator<char>(inputFile),
        std::istreambuf_iterator<char>()
    );

    return fileContents;
}