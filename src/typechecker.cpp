#include "typechecker.h"
#include "common.h"
#include "llvmcomp.h"
#include "parser.h"
#include "types.h"

TypeIndex TypeChecker::materialize(
  NodeIndex nodeIndex,
  Identifier name,
  LinkageName linkageScope
) {
  auto frame = compiler.stackItems;
  if (!name.empty()) frame.name = name;
  if (linkageScope) frame.linkageScope = linkageScope;
  auto guard = compiler.push(frame);
  auto type = compiler.compile(nodeIndex).unboxType();
  return type;
}

LinkageName TypeChecker::currentFunctionLinkageScope() {
  if (compiler.name.empty()) return compiler.linkageScope;
  return compiler.qualifyLinkageName(compiler.name);
}

Reference TypeChecker::compile(NodeIndex index, TypeIndex expected) {
  return compiler.compile(index, expected);
}

Logger TypeChecker::log(LogLevel::TypeCheck);

TypeChecker::ReturnType TypeChecker::index(NodeIndex object, NodeIndex index) {
  auto objectType = check(object);
  if (objectType.type == Pool().generic) {
    auto generic = compiler.compile(object);
    auto boxedGeneric = generic.unbox<GenericValue>();
    if (!boxedGeneric) {
      crash(object, "Internal error: generic value was not available");
    }
    auto arguments = parser.getArgumentList(index);
    return {compiler.instantiateGeneric(*boxedGeneric, arguments).getType()};
  }
  if (parser.nodeType(index) == NodeType::ArgumentList) {
    auto arguments = parser.getArgumentList(index);
    if (arguments.positional.size() != 1 || !arguments.named.empty()) {
      crash(index, "Indexing requires exactly one positional argument");
    }
    index = arguments.positional[0];
  }
  if (auto sizedArray = Pool().sizedArray(objectType.type)) {
    return checkArrayIndex(
      index,
      sizedArray->dereferencedType,
      objectType.lValue
    );
  }
  if (auto sliceElement = Pool().sliceElementType(objectType.type)) {
    return checkArrayIndex(index, sliceElement, true);
  }
  if (auto dereffed = Pool().multiPointerElement(objectType.type)) {
    return {dereffed, true};
  }
  crash(nodeIndex, "Can't index {}", TypeName(objectType.type));
}
