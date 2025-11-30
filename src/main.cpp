#include "fmt/base.h"
#include "fmt/format.h"
#include "llvm_comp.h"
#include "logging.h"
#include "unistd.h"
#include <cstddef>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <stdexcept>
#include <string>

int main(int argc, char* argv[]) {
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
  fmt::println("Writing to file {}", outFilename);
  outFile << "%.slice = type {ptr, i64}\n";
  fmt::println(outFile, "declare void @llvm.trap() nounwind");
  TranslationUnit::compile(sourceFile, outFile);
  outFile.close();
  fmt::println("Generating object file");
  execl("clang", "-c", outFilename.c_str(), "-o", "main.o");

  fmt::println("Generating executable");

  auto& clangArgs = CompilerContext::inst().c.clangArgs;

  // Create object from included C files
  bool cIncludes = !clangArgs.empty();

  std::string cIncludeObject;
  if (cIncludes) {
    cIncludeObject = "include.o";
    fmt::println("Compiling included C files");
    auto clangCommand = fmt::format("clang -x c {} -c /dev/null -o {}", fmt::join(clangArgs, " "), cIncludeObject);
    fmt::println("Clang args: {}", clangArgs);
    auto rc = std::system(clangCommand.c_str());
    if (rc != 0) {
      fmt::println("Error compiling include files");
      abort();
    }
  }

  auto& linkedLibararies = CompilerContext::inst().c.linkedLibaries;
  auto clangCommand = fmt::format("clang main.o {} {} -o {}", cIncludeObject, fmt::join(linkedLibararies, " "), executable);

  fmt::println("Linking with args: {}", clangCommand);
  auto rc = std::system(clangCommand.c_str());
  if (rc != 0) {
    fmt::println("Error linking libraries");
    abort();
  }
  fmt::println("clanged");
}
