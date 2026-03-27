#include "common.h"
#include "fmt/base.h"
#include <filesystem>
#include <getopt.h>
#include <string>

#define DOCTEST_CONFIG_IMPLEMENT
#include "../deps/doctest.h"

int main(int argc, char* argv[]) {
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
  std::string outFilename = buildDir.append("main.ll");
}
