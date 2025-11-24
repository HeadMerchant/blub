#include "fmt/format.h"
#include "llvm_comp.h"
#include "logging.h"
#include "unistd.h"
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
  execl("llc", "--file-type=obj", outFilename.c_str(), "-o", "main.o");

  fmt::println("Generating executable");
  std::vector<std::string> clangArgs;
  for (auto include : CompilerContext::inst().c.includes) {
    clangArgs.push_back("-include");
    clangArgs.push_back(std::string(include).data());
  }
  for (auto include : CompilerContext::inst().c.includeDirs) {
    clangArgs.push_back(fmt::format("-I{}", include).data());
  }
  for (auto include : CompilerContext::inst().c.defines) {
    clangArgs.push_back(fmt::format("-D{}", include).data());
  }
  for (auto library : CompilerContext::inst().c.linkLibraries) {
    clangArgs.push_back(fmt::format("-l{}", library).data());
  }
  std::vector<char*> cStringedArgs = {const_cast<char*>("main.o"), const_cast<char*>("-o"), executable.data()};
  for (auto arg : clangArgs) {
    cStringedArgs.push_back(arg.data());
  }
  execvp("clang", &cStringedArgs[0]);
  fmt::println("clanged");
}
