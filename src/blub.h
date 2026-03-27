#include "common.h"
#include "fmt/format.h"
#include "llvm_comp.h"
#include <cstdlib>
#include <fstream>
#include <stdexcept>
#include <string_view>

void compileBlub(string_view sourceFile, string_view buildDir, std::string outFilename) {
  fs::create_directories(buildDir);

  std::ofstream outFile(outFilename, std::ofstream::out | std::ofstream::trunc);
  if (!outFile.is_open()) {
    throw std::invalid_argument("Unable to write llvm bytecode to " + outFilename);
  }
  fmt::println("Writing to file {}", outFilename);
  std::string_view preamble = "%.slice = type {ptr, i64}\n"
                              "declare void @llvm.trap() nounwind\n"
                              "%.ctor = type { i32, ptr, ptr }\n"
                              "@llvm.global_ctors = appending global [1 x %.ctor] [%.ctor { i32 65535, ptr @.ctor, ptr null }]\n";
  outFile << preamble;
  TranslationUnit::compile(sourceFile, outFile, TargetType::Cpu);
  outFile << "define void @.ctor() {\n" << CompilerContext::inst().blub.globalInitialization.str() << "ret void\n}";
  outFile.close();
  auto objectCommand = fmt::format("clang -c {} -o main.o", outFilename);
  fmt::println("Generating object file: {}", objectCommand);
  if (auto rc = std::system(objectCommand.c_str())) {
    fmt::println(std::cerr, "Error generating object file (likely error in blub compiler)");
    abort();
  }

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

  auto& linkedLibararies = CompilerContext::inst().c.linkedLibraries;
  auto clangCommand = fmt::format("clang main.o {} {} -o {}", cIncludeObject, fmt::join(linkedLibararies, " "), executable);

  fmt::println("Linking with args: {}", clangCommand);
  if (auto rc = std::system(clangCommand.c_str())) {
    fmt::println(std::cerr, "Error linking libraries");
    abort();
  }
  fmt::println("clanged");
}
