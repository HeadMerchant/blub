#include <fstream>
#include <stdexcept>
#include <string>

std::string readFile(std::string filePath) {
    std::ifstream inputFile(filePath);

    if (!inputFile.is_open()) {
        throw std::invalid_argument("Error: could not open the file.");
    }

    std::string fileContents = std::string(
        std::istreambuf_iterator<char>(inputFile),
        std::istreambuf_iterator<char>()
    );

    return fileContents;
}
