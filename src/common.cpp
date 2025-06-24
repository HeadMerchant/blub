#include "common.h"
#include <stdexcept>

[[ noreturn ]] void TODO(std::string message) {
  throw std::runtime_error(message);
}
