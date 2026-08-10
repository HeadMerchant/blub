#include "common.h"
#include <fmt/ostream.h>
#include <stdexcept>

[[noreturn]] void TODO(std::string message) {
  throw std::runtime_error(message);
}

u32 packInt(u8 a, u8 b, u8 c, u8 d) {
  return (a << 24) | (b << 16) | (c << 8) | (d);
}

std::tuple<u8, u8, u8, u8> unpackInt(u32 value) {
  return {
    (value >> 24) & 255,
    (value >> 16) & 255,
    (value >> 8) & 255,
    value & 255
  };
}

// static char* StringPool::bytes = malloc(64 * 4096);

StringPool& StringPool::inst() {
  static StringPool pool(64 * 4096);
  return pool;
}

void emitEmbeddedFile(
  std::ostream& outFile,
  const fs::path& filePath,
  bool nullTerminated
) {
  std::ifstream input(filePath, std::ios::binary);
  if (!input.is_open()) {
    throw std::invalid_argument(
      "Unable to open embedded file " + filePath.string()
    );
  }

  std::string contents{
    std::istreambuf_iterator<char>(input),
    std::istreambuf_iterator<char>()
  };
  fmt::print(outFile, "global [{} x i8] c\"", contents.size() + nullTerminated);
  for (unsigned char c : contents) {
    if (c >= 32 && c <= 126 && c != '\\' && c != '"') {
      outFile << c;
    } else {
      fmt::print(outFile, "\\{:02X}", static_cast<unsigned int>(c));
    }
  }
  fmt::println(outFile, "{}\" align 1\n", nullTerminated ? "\\00" : "");
}
