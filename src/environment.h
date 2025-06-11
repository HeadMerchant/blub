#pragma once
#include "interpreter/value.h"
#include <string_view>

class Environment {
  public:
  SymbolMap defs;
  std::vector<Environment*> imports;

  // Optional
  Environment *parent;

  Environment(Environment* parent = nullptr): parent(parent), imports(), defs() {}
  Environment(Map defs, Environment* parent = nullptr): parent(parent), defs(defs), imports() {}

  Reference *find(std::string_view name) {
    Environment *env = this;
    while (env) {
      if (env->defs.isDefined(name))
        return env->defs.symbolValues[name];
      env = env->parent;
    }
    return nullptr;
  }

  bool define(std::string_view name, Reference *value) {
    defs.define(name, value);
    return true;
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

  static Environment baseEnvironment;
};
