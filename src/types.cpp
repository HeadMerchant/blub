#include "types.h"

Types::TypePool& Types::Pool() {
  static Types::TypePool pool;
  return pool;
}
