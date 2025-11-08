#include "logging.h"

class NullBuffer : public std::streambuf {
public:
  int overflow(int c) {
    return c;
  }
};

auto nullBuffer = NullBuffer();
std::ostream nullStream(&nullBuffer);

std::ostream& logger(LogLevel level) {
  if (level >= LogLevel::DEBUG) {
    return std::cout;
  }
  return nullStream;
}
