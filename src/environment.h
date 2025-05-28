#pragma once
#include "interpreter/value.h"
#include <string_view>
#include <unordered_map>

class Environment {
public:
  std::unordered_map<std::string_view, InterpreterValue *> defs;
  // Optional
  Environment *parent;

  InterpreterValue *find(std::string_view name) {
    Environment *env = this;
    while (env) {
      if (env->defs.count(name) > 0)
        return env->defs[name];
      env = env->parent;
    }
    return nullptr;
  }

  static Environment baseEnvironment;
};
