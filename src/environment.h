#pragma once
#include <algorithm>
#include <string_view>
#include <unordered_map>
#include "parser.h"

class Environment {
    public:
    std::unordered_map<std::string_view, ASTNode*> defs;
    // Optional
    Environment* parent;

    ASTNode* find(std::string_view name) {
        Environment* env = this;
        while (env) {
            if (env->defs.count(name) > 0) return env->defs[name];
            env = env->parent;
        }
        return nullptr;
    }
};
