#pragma once

int puts(const char* message);

static inline const char* c_static_inline_message(void) {
  return "static inline";
}

int c_static_inline_puts(const char* message) {
  return puts(message);
}
