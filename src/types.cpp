#include "types.h"

Types::TypePool& Types::Pool() {
  static Types::TypePool Pool;
  return Pool;
}
