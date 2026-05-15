#include "common.h"
#include "llvmcomp.h"
#include "parser.h"
#include "typechecker.h"
#include "types.h"

TypeIndex TypeChecker::materialize(NodeIndex nodeIndex) {
  return compiler.compile(nodeIndex).unboxType();
}

Reference TypeChecker::compile(NodeIndex index, TypeIndex expected) {
  return compiler.compile(index, expected);
}

Logger TypeChecker::log(LogLevel::TypeCheck);
