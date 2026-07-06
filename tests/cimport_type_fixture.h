#pragma once

typedef struct {
  float x;
  float y;
  float z;
  float w;
} ci_Quat;

ci_Quat ci_MulQ(ci_Quat left, ci_Quat right) {
  return left;
}
