#pragma once
#include "interpreter/value.h"
#include <stdexcept>
#include <string_view>
#include <sstream>
#include <unordered_map>

using Map = std::unordered_map<std::string_view, Reference *>;
class Environment {
  Map defs;
  std::vector<Environment*> imports;

  // Optional
  public:
  Environment *parent;

  Environment(Environment* parent = nullptr): parent(parent), imports(), defs() {}
  Environment(Map defs, Environment* parent = nullptr): parent(parent), defs(defs), imports() {}

  Reference *find(std::string_view name) {
    Environment *env = this;
    while (env) {
      if (env->defs.count(name) > 0)
        return env->defs[name];
      env = env->parent;
    }
    return nullptr;
  }

  bool define(std::string_view name, Reference *value) {
    if (defs.count(name) > 0) {
      std::stringstream ss;
      ss << "Attempted to redefine " << name;
      throw std::invalid_argument(ss.str());
    }

    defs[name] = value;
    return true;
  }

  bool assign(std::string_view name, Reference *value) {
    Environment *env = this;
    while (env) {
      if (env->defs.count(name) > 0) {
        env->defs[name] = value;
      }
      env = env->parent;
    }

    std::stringstream ss;
    ss << "Attempted to assign to undefined name " << name;
    throw std::invalid_argument(ss.str());
  }

  bool hasDefinition(std::string_view name) {
    return defs.count(name) > 0;
  }
  
  static Environment baseEnvironment;
};
