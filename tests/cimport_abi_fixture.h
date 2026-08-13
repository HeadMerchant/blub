#pragma once
#include <stdint.h>

typedef struct abi_rect {
  int x;
  int y;
  int width;
  int height;
} abi_rect;

typedef struct abi_mixed {
  double value;
  int tag;
} abi_mixed;

typedef struct abi_large {
  int64_t x;
  int64_t y;
  int64_t z;
} abi_large;

abi_rect blub_make_rect(int base);
int blub_sum_rect(abi_rect value);
abi_mixed blub_make_mixed(int base);
abi_large blub_make_large(int base);
uint8_t blub_increment_u8(uint8_t value);

abi_rect abi_c_make_rect(int base) {
  return (abi_rect){base, base + 1, base + 2, base + 3};
}

int abi_c_sum_rect(abi_rect value) {
  return value.x + value.y + value.width + value.height;
}

int abi_c_call_blub_rect(int base) {
  return abi_c_sum_rect(blub_make_rect(base));
}

int abi_c_call_blub_sum(int base) {
  return blub_sum_rect(abi_c_make_rect(base));
}

int abi_c_call_blub_mixed(int base) {
  abi_mixed value = blub_make_mixed(base);
  return (int)value.value + value.tag;
}

int64_t abi_c_call_blub_large(int base) {
  abi_large value = blub_make_large(base);
  return value.x + value.y + value.z;
}

uint8_t abi_c_increment_u8(uint8_t value) {
  return value + 1;
}

int abi_c_call_blub_u8(int base) {
  return blub_increment_u8((uint8_t)base);
}
