#include "common.h"
#include "llvmcomp.h"
#include "parser.h"
#include "typechecker.h"
#include "types.h"

TypeIndex TypeChecker::materialize(NodeIndex nodeIndex, string_view name) {
  auto frame = compiler.stackItems;
  if (!name.empty()) frame.name = name;
  auto guard = compiler.push(frame);
  auto type = compiler.compile(nodeIndex).unboxType();
  return type;
}

Reference TypeChecker::compile(NodeIndex index, TypeIndex expected) {
  return compiler.compile(index, expected);
}

Logger TypeChecker::log(LogLevel::TypeCheck);
