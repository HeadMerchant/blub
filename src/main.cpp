#include "common.h"
#include "fmt/base.h"
#include "fmt/format.h"
#include "fmt/ostream.h"
#include "llvmcomp.h"
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <getopt.h>
#include <iostream>
#include <ranges>
#include <string>
#include <string_view>

#define DOCTEST_CONFIG_IMPLEMENT
#include "doctest.h"

string_view kernelIr = "main.cu.ll";

void compileCuda(
  string_view buildDir,
  string_view cudaArch,
  const fs::path& cudaApiDir
);

namespace {

fs::path findLibdevice(const fs::path& cudaApiDir) {
  fs::path libdeviceDir = cudaApiDir / "nvvm" / "libdevice";
  fs::path defaultLibdevice = libdeviceDir / "libdevice.10.bc";
  if (fs::exists(defaultLibdevice)) {
    return defaultLibdevice;
  }

  for (const auto& entry : fs::directory_iterator(libdeviceDir)) {
    if (!entry.is_regular_file()) {
      continue;
    }
    auto filename = entry.path().filename().string();
    if (
      entry.path().extension() == ".bc" && filename.starts_with("libdevice")
    ) {
      return entry.path();
    }
  }

  throw std::invalid_argument(
    "Unable to find libdevice bitcode under " + libdeviceDir.string()
  );
}
} // namespace

void promoteStaticInlineDefinitions(
  const fs::path& irPath,
  const std::vector<Identifier>& functionNames
) {
  std::ifstream input(irPath);
  if (!input.is_open()) {
    throw std::invalid_argument(
      "Unable to read imported C LLVM IR from " + irPath.string()
    );
  }

  std::string ir{
    std::istreambuf_iterator<char>(input),
    std::istreambuf_iterator<char>()
  };
  for (const auto& functionName : functionNames) {
    auto symbol = fmt::format("@{}(", functionName);
    auto symbolIndex = ir.find(symbol);
    while (symbolIndex != std::string::npos) {
      auto lineStart = ir.rfind('\n', symbolIndex);
      lineStart = lineStart == std::string::npos ? 0 : lineStart + 1;
      auto linkageIndex = ir.find("define internal ", lineStart);
      if (linkageIndex != std::string::npos && linkageIndex < symbolIndex) {
        ir.replace(
          linkageIndex,
          std::string("define internal ").size(),
          "define dso_local "
        );
        break;
      }
      symbolIndex = ir.find(symbol, symbolIndex + symbol.size());
    }
  }

  std::ofstream output(irPath, std::ofstream::out | std::ofstream::trunc);
  if (!output.is_open()) {
    throw std::invalid_argument(
      "Unable to write imported C LLVM IR to " + irPath.string()
    );
  }
  output << ir;
}

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
  string_view cudaArch = "sm_75";
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
    "define i32 @.doubleToStr(ptr %out, i64 %len, double %arg) {\n"
    "  %res = call i32 (ptr, i64, ptr, ...) @snprintf(ptr %out, i64 %len, ptr "
    "@.doubleFmtString, double "
    "%arg)\n"
    "  ret i32 %res\n"
    "}\n";
  outFile << printDouble;
  outKernel << sliceDef;

  auto& contextInst = CompilerContext::inst();
  contextInst.blub.outputFileStream = &outFile;
  contextInst.cuda.outputFileStream = &outKernel;
  Compiler::compile(sourceFile, TargetType::Cpu);

  if (contextInst.cuda.embeddedPtxGlobal) {
    outKernel.flush();
    outKernel.close();
    compileCuda(buildDirName, cudaArch, fs::path(cudaApiDir));
    fmt::print(outFile, "@{} = ", contextInst.cuda.embeddedPtxGlobal->name);
    emitEmbeddedFile(outFile, fs::path(buildDir) / "out.ptx", true);
  }

  outFile << "define void @.ctor() {\n";
  CompilerContext::inst().blub.globalInitialization.drain(outFile);
  outFile << "ret void\n}";
  outFile.close();

  auto& clangArgs = CompilerContext::inst().c.clangArgs;
  auto& staticInlineFunctions = CompilerContext::inst().c.staticInlineFunctions;
  bool cIncludes = !clangArgs.empty();
  std::string finalIr = outFilename;

  if (cIncludes) {
    auto keepAliveSource = buildDir / "cimport_keep_alive.c";
    std::ofstream keepAliveFile(
      keepAliveSource,
      std::ofstream::out | std::ofstream::trunc
    );
    if (!keepAliveFile.is_open()) {
      throw std::invalid_argument(
        "Unable to write C import keep-alive source to " +
        keepAliveSource.string()
      );
    }

    for (size_t i = 0; i < staticInlineFunctions.size(); ++i) {
      fmt::println(
        keepAliveFile,
        "__attribute__((used)) static void* blub_keep_alive_{} = (void*)&{};",
        i,
        staticInlineFunctions[i]
      );
    }
    keepAliveFile.close();

    auto cImportsIr = buildDir / "cimports.ll";
    auto linkedIr = buildDir / "linked.bc";
    auto cIrCommand = fmt::format(
      "clang -S -emit-llvm -O0 -x c {} {} -o {}",
      fmt::join(clangArgs, " "),
      keepAliveSource.string(),
      cImportsIr.string()
    );
    fmt::println("Compiling included C files to LLVM IR");
    fmt::println("Clang args: {}", clangArgs);
    if (auto rc = std::system(cIrCommand.c_str())) {
      fmt::println(std::cerr, "Error compiling include files to LLVM IR");
      abort();
    }
    promoteStaticInlineDefinitions(cImportsIr, staticInlineFunctions);

    auto linkIrCommand = fmt::format(
      "llvm-link {} {} -o {}",
      outFilename,
      cImportsIr.string(),
      linkedIr.string()
    );
    fmt::println("Linking Blub and C LLVM IR");
    if (auto rc = std::system(linkIrCommand.c_str())) {
      fmt::println(std::cerr, "Error linking Blub and C LLVM IR");
      abort();
    }
    finalIr = linkedIr.string();
  }

  auto objectCommand = fmt::format("clang -c {} -o main.o", finalIr);
  fmt::println("Generating object file: {}", objectCommand);
  if (auto rc = std::system(objectCommand.c_str())) {
    fmt::println(
      std::cerr,
      "Error generating object file (likely error in blub compiler)"
    );
    abort();
  }

  fmt::println("Generating executable");

  auto& linkedLibararies = CompilerContext::inst().c.linkedLibraries;
  auto clangCommand = fmt::format(
    "clang main.o {} -o {}",
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

void compileCuda(
  string_view buildDir,
  string_view cudaArch,
  const fs::path& cudaApiDir
) {
  auto& importedFiles = CompilerContext::inst().cuda.linkedFiles;
  std::string kernelIr = fmt::format("{}/main.cu.ll", buildDir);
  std::string linkedKernelIr = fmt::format("{}/main.linked.cu.ll", buildDir);
  std::string outPtx = fmt::format("{}/out.ptx", buildDir);
  std::string finalCubin = fmt::format("{}/kernel.cubin", buildDir);
  fs::path libdevice = findLibdevice(cudaApiDir);

  auto linkCommand = fmt::format(
    "llvm-link {} {} -o {}",
    kernelIr,
    libdevice.string(),
    linkedKernelIr
  );
  fmt::println("Linking blub cuda IR with libdevice: {}", linkCommand);
  if (auto rc = std::system(linkCommand.c_str())) {
    fmt::println(std::cerr, "Error linking blub CUDA LLVM IR with libdevice");
    abort();
  }

  // Needed on older GPUs
  auto optCommand = fmt::format(
    "opt -passes='nvvm-reflect,default<O2>' {} -o {}",
    linkedKernelIr,
    linkedKernelIr
  );
  fmt::println("Running cuda opt pass: {}", optCommand);
  if (auto rc = std::system(optCommand.c_str())) {
    fmt::println(std::cerr, "Error running cuda opt pass");
    abort();
  }

  auto compileCommand = fmt::format(
    "llc -march=nvptx64 -mcpu={} {} -o {}",
    cudaArch,
    linkedKernelIr,
    outPtx
  );
  fmt::println("Compiling blub cuda code: {}", compileCommand);
  if (auto rc = std::system(compileCommand.c_str())) {
    fmt::println(std::cerr, "Error compiling blub LLVM IR to ptx");
    abort();
  }
  // rewritePtxSymbols(outPtx);

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
