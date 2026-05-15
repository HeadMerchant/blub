#include "common.h"
#include "fmt/base.h"
#include "fmt/format.h"
#include "fmt/ostream.h"
#include "llvmcomp.h"
#include <cstdlib>
#include <filesystem>
#include <getopt.h>
#include <iostream>
#include <ranges>
#include <string>
#include <string_view>

#define DOCTEST_CONFIG_IMPLEMENT
#include "../deps/doctest.h"

string_view kernelIr = "main.cu.ll";

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
      auto args = span<char*>(argv, argc) |
                  transform([](char* x) { return string_view(x); });
      fmt::println("Running tests with args: {}", fmt::join(args, " "));
    }

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
  enum class ArgFlags {
    output = 'o',
    log = 'l',
    cudaArch = 1001,
    cudaDir = 1002,
  };
  option longArgs[] = {
    {"cuda-arch", required_argument, 0, (int)ArgFlags::cudaArch},
    {"cuda-dir",  required_argument, 0, (int)ArgFlags::cudaDir },
    {"output",    required_argument, 0, (int)ArgFlags::output  },
    // TODO: log flags
    {"log",       optional_argument, 0, (int)ArgFlags::log     },
    {0,           0,                 0, 0                      },
  };
  string_view cudaArch = "sm_86";
  string_view cudaApiDir = "/opt/cuda";
  while ((opt = getopt_long(argc, argv, "o:l:", longArgs, nullptr)) != -1) {
    auto flag = (ArgFlags)opt;
    switch (flag) {
    case ArgFlags::output: {
      executable = optarg;
      break;
    }
    case ArgFlags::cudaArch: {
      cudaArch = optarg;
      break;
    }
    case ArgFlags::cudaDir: {
      cudaApiDir = optarg;
      break;
    }
    case ArgFlags::log: {
      if (!optarg) {
        fmt::println("Setting all log flags");
        Logger::globalLevels = LogLevel(-1);
        break;
      }
      string_view flags(optarg);
      for (auto c : flags) {
        int logLevel = 0;
        switch (c) {
        case 'i': {
          logLevel = (int)LogLevel::CImport;
          break;
        }
        case 'p': {
          logLevel = (int)LogLevel::Parsing;
          break;
        }
        case 'c': {
          logLevel = (int)LogLevel::Compile;
          break;
        }
        case 't': {
          logLevel = (int)LogLevel::TypeCheck;
          break;
        }
        default: {
          fmt::println(std::cerr, "Unknown logging flag: '{}'", c);
        }
        }
        Logger::globalLevels = LogLevel((int)Logger::globalLevels | logLevel);
      }
      break;
    }
    default: {
      fmt::println(
        std::cerr,
        "Usage: {} [--cuda-arch CUDA_VERSION] [--cuda-dir CUDA_DIR (parent of "
        "cuda lib and include folders)] [--output "
        " OUTPUT] [MAIN FILE]",
        argv[0]
      );
      abort();
    }
    }
  }

  if (optind >= argc) {
    fmt::println(std::cerr, "Missing input source file");
    return 1;
  }

  std::string_view sourceFile(argv[optind]);
  fmt::print("Input filename: ");
  fmt::println("{}", sourceFile);

  if (executable.empty()) {
    executable = fs::path(sourceFile).stem().string();
  }

  fs::path inputFile(sourceFile);
  fs::path buildDir = inputFile.parent_path().append(".blub");
  fs::create_directories(buildDir);
  std::string buildDirString = buildDir.string();
  string_view buildDirName = buildDirString;
  fmt::println("Build dir: {}", buildDirName);

  std::string outFilename = buildDir / "main.ll";
  std::string kernelFilename = buildDir / kernelIr;

  std::ofstream outFile(outFilename, std::ofstream::out | std::ofstream::trunc);
  if (!outFile.is_open()) {
    throw std::invalid_argument(
      "Unable to write llvm bytecode to " + outFilename
    );
  }
  std::ofstream outKernel(
    kernelFilename,
    std::ofstream::out | std::ofstream::trunc
  );
  if (!outKernel.is_open()) {
    throw std::invalid_argument(
      "Unable to write cuda llvm bytecode to " + kernelFilename
    );
  }

  fmt::println("Writing to file {}", outFilename);
  string_view sliceDef = "%.slice = type {ptr, i64}\n";
  std::string_view preamble =
    "declare void @llvm.trap() nounwind\n"
    "%.ctor = type { i32, ptr, ptr }\n"
    "@llvm.global_ctors = appending global [1 x %.ctor] [%.ctor { i32 65535, "
    "ptr @.ctor, ptr null }]\n"
    "@.doubleFmtString = global [3 x i8] c\"%f\\00\" align 1\n";
  outFile << sliceDef;
  outFile << preamble;
  // TODO: 32-bit
  std::string_view printDouble =
    "declare i32 @snprintf(ptr, i64, ptr, ...)\n"
    "define void @.doubleToStr(ptr %out, i64 %len, double %arg) {\n"
    "  %res = call i32 (ptr, i64, ptr, ...) @snprintf(ptr %out, i64 %len, ptr "
    "@.doubleFmtString, double "
    "%arg)\n"
    "  ret void\n"
    "}\n";
  outFile << printDouble;
  outKernel << sliceDef;

  auto& contextInst = CompilerContext::inst();
  contextInst.blub.outputFileStream = &outFile;
  contextInst.cuda.outputFileStream = &outKernel;
  Compiler::compile(sourceFile, TargetType::Cpu);
  outFile << "define void @.ctor() {\n"
          << CompilerContext::inst().blub.globalInitialization.str()
          << "ret void\n}";
  outFile.close();
  auto objectCommand = fmt::format("clang -c {} -o main.o", outFilename);
  fmt::println("Generating object file: {}", objectCommand);
  if (auto rc = std::system(objectCommand.c_str())) {
    fmt::println(
      std::cerr,
      "Error generating object file (likely error in blub compiler)"
    );
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
    auto clangCommand = fmt::format(
      "clang -x c {} -c /dev/null -o {}",
      fmt::join(clangArgs, " "),
      cIncludeObject
    );
    fmt::println("Clang args: {}", clangArgs);
    auto rc = std::system(clangCommand.c_str());
    if (rc != 0) {
      fmt::println("Error compiling include files");
      abort();
    }
  }

  auto& linkedLibararies = CompilerContext::inst().c.linkedLibraries;
  auto clangCommand = fmt::format(
    "clang main.o {} {} -o {}",
    cIncludeObject,
    fmt::join(linkedLibararies, " "),
    executable
  );

  fmt::println("Linking with args: {}", clangCommand);
  if (auto rc = std::system(clangCommand.c_str())) {
    fmt::println(std::cerr, "Error linking libraries");
    abort();
  }
  fmt::println("clanged");
}

void compileCuda(string_view buildDir, string_view cudaArch) {
  auto& importedFiles = CompilerContext::inst().cuda.linkedFiles;
  std::string kernelIr = fmt::format("{}/main.cu.ll", buildDir);
  std::string outPtx = fmt::format("{}/out.ptx", buildDir);
  std::string finalCubin = fmt::format("{}/kernel.cubin", buildDir);
  std::string cudaObjFile = fmt::format("{}/kernel_cubin.o", buildDir);

  // # 2. lower your IR to PTX
  auto compileCommand = fmt::format(
    "llc -march=nvptx64 -mcpu={} {} -o {}",
    cudaArch,
    kernelIr,
    outPtx
  );
  fmt::println("Compiling blub cuda code: {}", compileCommand);
  if (auto rc = std::system(compileCommand.c_str())) {
    fmt::println(std::cerr, "Error compiling blub LLVM IR to ptx");
    abort();
  }

  auto assembleCommand =
    fmt::format("ptxas -arch={} {} -o {}", cudaArch, outPtx, finalCubin);

  if (!importedFiles.empty()) {
    std::string linkedPtx = fmt::format("{}/linked.ptx", buildDir);
    std::string linkedCubin = fmt::format("{}/linked.cubin", buildDir);
    auto cudaImportCommand = fmt::format(
      "nvcc --ptx -arch={} {} -o {}",
      cudaArch,
      fmt::join(importedFiles, " "),
      linkedPtx
    );
    fmt::println("Compiling imported cuda files: {}", cudaImportCommand);
    if (auto rc = std::system(cudaImportCommand.c_str())) {
      fmt::println(std::cerr, "Error compiling linked imported cuda files");
      abort();
    }
    auto assembleImportCommand =
      fmt::format("ptxas -arch={} {} -o {}", cudaArch, linkedPtx, linkedCubin);
    if (auto rc = std::system(assembleImportCommand.c_str())) {
      fmt::println(std::cerr, "Error assembling imported cuda");
      abort();
    }
    auto linkCommand = fmt::format(
      "nvlink -arch={} {} {} -o {}",
      cudaArch,
      linkedCubin,
      finalCubin,
      finalCubin
    );
    if (auto rc = std::system(linkCommand.c_str())) {
      fmt::println(
        std::cerr,
        "Failed to link imported cuda cubin with blub cuda cubin"
      );
      abort();
    }
  }

  if (auto rc = std::system(assembleCommand.c_str())) {
    fmt::println(std::cerr, "Error assembling ptx to cubin");
    abort();
  }
}
