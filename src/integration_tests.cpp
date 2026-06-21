#include "common.h"
#include "fmt/format.h"
#include <array>
#include <fstream>
#include <sstream>
#include <sys/wait.h>
#include <unistd.h>

namespace {

struct ProcessResult {
  int exitCode;
  bool exitedNormally;
  std::string output;
};

ProcessResult runProcess(
  const std::vector<std::string>& args,
  const fs::path& workingDirectory
) {
  int pipeFd[2];
  REQUIRE_EQ(pipe(pipeFd), 0);

  auto pid = fork();
  REQUIRE(pid >= 0);

  if (pid == 0) {
    close(pipeFd[0]);
    if (chdir(workingDirectory.c_str()) != 0) {
      _exit(126);
    }
    if (dup2(pipeFd[1], STDOUT_FILENO) != STDOUT_FILENO) {
      _exit(126);
    }
    if (dup2(pipeFd[1], STDERR_FILENO) != STDERR_FILENO) {
      _exit(126);
    }
    close(pipeFd[1]);

    std::vector<char*> argv;
    argv.reserve(args.size() + 1);
    for (const auto& arg : args) {
      argv.push_back(const_cast<char*>(arg.c_str()));
    }
    argv.push_back(nullptr);

    execv(argv[0], argv.data());
    _exit(127);
  }

  close(pipeFd[1]);

  std::string output;
  std::array<char, 4096> buffer;
  ssize_t bytesRead = 0;
  while ((bytesRead = read(pipeFd[0], buffer.data(), buffer.size())) > 0) {
    output.append(buffer.data(), bytesRead);
  }
  close(pipeFd[0]);

  int status = 0;
  REQUIRE_EQ(waitpid(pid, &status, 0), pid);

  return {
    .exitCode = WIFEXITED(status) ? WEXITSTATUS(status) : status,
    .exitedNormally = WIFEXITED(status),
    .output = std::move(output),
  };
}

std::string normalizeOutput(std::string text) {
  std::string normalized;
  normalized.reserve(text.size());
  for (size_t i = 0; i < text.size(); ++i) {
    if (text[i] == '\r' && i + 1 < text.size() && text[i + 1] == '\n') {
      continue;
    }
    normalized.push_back(text[i]);
  }
  while (!normalized.empty() && normalized.back() == '\n') {
    normalized.pop_back();
  }
  return normalized;
}

optional<fs::path> expectationPathFor(
  const fs::path& testPath,
  std::string_view suffix
) {
  std::array<fs::path, 2> candidates = {
    testPath.string() + std::string(suffix),
    testPath.parent_path() /
      fmt::format("{}{}", testPath.stem().string(), suffix),
  };

  for (const auto& candidate : candidates) {
    if (fs::exists(candidate)) {
      return candidate;
    }
  }
  return {};
}

std::string readFile(const fs::path& path) {
  std::ifstream file(path);
  REQUIRE(file.is_open());
  std::stringstream buffer;
  buffer << file.rdbuf();
  return buffer.str();
}

std::vector<fs::path> discoverBlubTests() {
  fs::path testsDir = fs::path(BLUB_SOURCE_DIR) / "tests";
  std::vector<fs::path> tests;
  for (const auto& entry : fs::directory_iterator(testsDir)) {
    if (!entry.is_regular_file()) {
      continue;
    }
    if (entry.path().extension() != ".blub") {
      continue;
    }
    tests.push_back(entry.path());
  }
  std::sort(tests.begin(), tests.end());
  return tests;
}

fs::path currentExecutablePath() {
  return fs::read_symlink("/proc/self/exe");
}

} // namespace

TEST_CASE("blub file tests") {
  auto tests = discoverBlubTests();
  REQUIRE_FALSE(tests.empty());

  fs::path executable = currentExecutablePath();
  fs::path projectRoot = BLUB_SOURCE_DIR;
  fs::path testsBuildDir = projectRoot / "tests" / ".blub";
  fs::create_directories(testsBuildDir);

  for (const auto& testPath : tests) {
    auto subcaseName = testPath.filename().string();
    SUBCASE(subcaseName.c_str()) {
      fs::path outputBinary = testsBuildDir / testPath.stem();
      auto compileResult = runProcess(
        {
          executable.string(),
          "--output",
          outputBinary.string(),
          testPath.string(),
        },
        projectRoot
      );

      INFO(compileResult.output);
      REQUIRE(compileResult.exitedNormally);
      REQUIRE_EQ(compileResult.exitCode, 0);

      auto runtimeResult = runProcess({outputBinary.string()}, projectRoot);
      INFO(runtimeResult.output);
      REQUIRE(runtimeResult.exitedNormally);

      if (auto expectedOutputPath = expectationPathFor(testPath, ".out")) {
        auto expected = normalizeOutput(readFile(expectedOutputPath.value()));
        auto actual = normalizeOutput(runtimeResult.output);
        CHECK_EQ(actual, expected);
      }

      if (
        auto expectedExitCodePath = expectationPathFor(testPath, ".exitcode")
      ) {
        auto expectedExitCodeText = readFile(expectedExitCodePath.value());
        auto expectedExitCode = std::stoi(expectedExitCodeText);
        CHECK_EQ(runtimeResult.exitCode, expectedExitCode);
      }
    }
  }
}
