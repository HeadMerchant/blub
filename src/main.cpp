#include "common.h"
#include "fmt/base.h"
#include "fmt/ostream.h"
#include "llvm_comp.h"
#include <filesystem>
#include <getopt.h>
#include <ranges>
#include <string>
#include <string_view>

#define DOCTEST_CONFIG_IMPLEMENT
#include "../deps/doctest.h"

int main(int argc, char** argv) {
  bool shouldRunTests = false;
  fmt::println("Arg count: {}", argc);
  if (argc > 1) {
    string_view firstArg(argv[1]);
    fmt::println("First arg: {}", firstArg);
    if (firstArg == "--test") {
      shouldRunTests = true;
      argv[1] = argv[0];
      argv++;
      argc--;
    }
  }

  if (shouldRunTests) {
    {
      auto args = span<char*>(argv, argc) | transform([](char* x) { return string_view(x); });
      fmt::println("Running tests with args: {}", fmt::join(args, " "));
    }
    // for (auto i = 0; i < argc; i++) {
    //   fmt::println("{}", string_view(argv[i]));
    // }
    doctest::Context ctx;
    ctx.applyCommandLine(argc, argv);
    int res = ctx.run();
    if (ctx.shouldExit()) {
      return res;
    }
    return 0;
  }

  std::string executable;
  int opt;
  while ((opt = getopt(argc, argv, "tpico:")) != -1) {
    switch (opt) {
    case 't':
      Logger::globalLevels = Logger::globalLevels | LogLevel::Tokenize;
      break;
    case 'p':
      Logger::globalLevels = Logger::globalLevels | LogLevel::Parsing;
      break;
    case 'i':
      Logger::globalLevels = Logger::globalLevels | LogLevel::CImport;
      break;
    case 'c':
      Logger::globalLevels = Logger::globalLevels | LogLevel::Compile;
      break;
    case 'o':
      executable = optarg;
      break;
    default: {
      fmt::println(std::cerr, "Unknown command line flag: {}", opt);
      return 1;
    }
    }
  }

  if (optind >= argc) {
    fmt::println(std::cerr, "Missing input source file");
    return 1;
  }

  std::string_view sourceFile(argv[optind]);

  if (executable.empty()) {
    executable = fs::path(sourceFile).stem().string();
  }

  fs::path inputFile(sourceFile);
  fs::path buildDir = inputFile.parent_path().append(".blub");
  fs::create_directories(buildDir);
  fmt::println("Build dir: {}", buildDir.string());

  std::string outFilename = buildDir.append("main.ll");

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
