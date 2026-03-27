#pragma once
#include "common.h"
#include "types.h"
#include "value.h"

struct OutContext {
  std::ostream& outputFile;
  Environment& environment;
};
