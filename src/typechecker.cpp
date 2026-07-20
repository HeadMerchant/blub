#include "common.h"
#include "llvmcomp.h"
#include "parser.h"
#include "typechecker.h"
#include "types.h"

TypeIndex TypeChecker::materialize(
  NodeIndex nodeIndex,
  string_view name,
  string_view linkageScope
) {
  auto frame = compiler.stackItems;
  if (!name.empty()) frame.name = name;
  if (!linkageScope.empty()) frame.linkageScope = linkageScope;
  auto guard = compiler.push(frame);
  auto type = compiler.compile(nodeIndex).unboxType();
  return type;
}

string_view TypeChecker::currentFunctionLinkageScope() {
  if (compiler.name.empty()) return compiler.linkageScope;
  return compiler.qualifyLinkageName(compiler.name);
}

Reference TypeChecker::compile(NodeIndex index, TypeIndex expected) {
  return compiler.compile(index, expected);
}

Logger TypeChecker::log(LogLevel::TypeCheck);
