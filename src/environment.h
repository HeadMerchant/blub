#pragma once
#include "interpreter/value.h"
#include <string_view>

class Environment {
  public:
  SymbolMap defs;
  std::vector<Environment*> imports;
  std::string prefix;
  // For LLVM
  private:
  i32 nextTemporary = 1;
  i32 nextAnonymousConstant = 0;
  public:

  // Optional
  Environment *parent;

  Environment(): parent(Environment::baseEnvironment()), imports(), defs(), prefix("") {}
  Environment(Environment* parent, std::string_view prefix): parent(parent), imports(), defs() {
    std::stringstream ss;
    ss << parent->prefix << prefix << ".";
    this->prefix = std::move(ss.str());
  }

  Environment(Map defs, Environment* parent = nullptr): parent(parent), defs(defs), imports() {}

  std::optional<Reference*> find(std::string_view name) {
    Environment *env = this;
    while (env) {
      if (env->defs.isDefined(name))
        return env->defs.symbolValues[name];
      env = env->parent;
    }
    return std::nullopt;
  }

  bool define(std::string_view name, Reference *value) {
    defs.define(name, value);
    return true;
  }

  i32 getNextTemporary() {
    i32 index = nextTemporary;
    nextTemporary += 1;
    return index;
  }

  std::string addTemporary() {
    return addTemporary(getNextTemporary());
  }

  std::string addTemporary(i32 index) {
    return fmt::format("%{}{}", parent->prefix, index);
  }

  std::string addLabel(std::string name) {
    return addLabel(std::move(name), getNextTemporary());
  }
  
  // labels only begin with "%" when used
  std::string addLabel(std::string name, i32 index) {
    return fmt::format("{}{}{}", parent->prefix, name, index);
  }

  std::pair<i32, std::string> addConstant() {
    return {nextAnonymousConstant++, fmt::format("@{}{}", prefix, nextAnonymousConstant)};
  }

  std::string addConstant(std::string_view name) {
    return fmt::format("@{}{}", prefix, name);
  }

  std::string addGlobal(std::string_view name) {
    return fmt::format("%{}{}", prefix, name);
  }

  std::string addGlobal() {
    return fmt::format("%{}{}", prefix, nextAnonymousConstant++);
  }

  // bool assign(std::string_view name, Reference *value) {
  //   Environment *env = this;
  //   while (env) {
  //     if (env->defs.count(name) > 0) {
  //       env->defs[name] = value;
  //     }
  //     env = env->parent;
  //   }

  //   std::stringstream ss;
  //   ss << "Attempted to assign to undefined name " << name;
  //   throw std::invalid_argument(ss.str());
  // }

  static Environment* baseEnvironment();
};
