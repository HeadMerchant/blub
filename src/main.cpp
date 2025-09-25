#include <filesystem>
#include <fstream>
#include <string>
#include <stdexcept>
#include "llvm_comp.h"
#include "logging.h"
#include "unistd.h"

namespace fs = std::filesystem;
int main(int argc, char *argv[]) {
    std::ostream& log = logger(LogLevel::DEBUG);
    // no source file passed in
    if (argc < 2) {
        throw std::invalid_argument("missing source file input argument");
    }

    std::string sourceFile(argv[1]);
    std::string executable;
    if (argc > 3) {
        executable = argv[2];
    } else {
        executable = fs::path(sourceFile).stem().string();
    }

    std::string outFilename = fs::path(sourceFile).stem().string() + ".ll";
    std::ofstream outFile(outFilename, std::ofstream::out | std::ofstream::trunc);
    if (!outFile.is_open()) {
        throw std::invalid_argument("Unable to write llvm bytecode to " + outFilename);
    }
    std::cout << "Writing to file " << outFilename << "\n";
    LLVMCompiler::compile(sourceFile, outFile);
    outFile.close();
    std::cout << "Running clang\n";
    execl("clang", outFilename.c_str(), "-o",  executable.c_str());
    std::cout << "clanged";
}
