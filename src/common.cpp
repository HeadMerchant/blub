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

  auto size = fs::file_size(filePath);
  fmt::print(outFile, "global [{} x i8] c\"", size + nullTerminated);
  char byte;
  while (input.get(byte)) {
    auto c = static_cast<unsigned char>(byte);
    if (c >= 32 && c <= 126 && c != '\\' && c != '"') {
      outFile << c;
    } else {
      fmt::print(outFile, "\\{:02X}", static_cast<unsigned int>(c));
    }
  }
  fmt::println(outFile, "{}\" align 1\n", nullTerminated ? "\\00" : "");
}

void IrCommandBuffer::drain(std::ostream& output) {
  for (const auto& command : commands) {
    std::visit(
      overloaded{
        [&](TextSpan span) {
          output.write(arena.data() + span.offset, span.length);
        },
        [&](const EmbeddedFile& file) {
          emitEmbeddedFile(output, file.path, file.nullTerminate);
        },
      },
      command
    );
  }
  commands.clear();
  arena.clear();
}

TEST_CASE("IR command buffer preserves FIFO text spans") {
  IrCommandBuffer commands;
  commands.push("first");
  fmt::print(commands.output(), "{}", "second");
  commands.push("third");

  std::stringstream rendered;
  commands.drain(rendered);
  CHECK(rendered.str() == "first\nsecondthird\n");
  CHECK(commands.empty());
}
