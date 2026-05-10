#include "llvmcomp.h"
#include "parser.h"
#include "typechecker.h"
#include "types.h"

TypeIndex TypeChecker::materialize(NodeIndex nodeIndex) {
  return compiler.compile(nodeIndex).unboxType();
}
