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
    return std::move(addTemporary(getNextTemporary()));
  }

  std::string addTemporary(i32 index) {
    std::stringstream name;
    name << "%" << parent->prefix << index;
    return std::move(name.str());
  }

  std::string addLabel(std::string name) {
    return std::move(addLabel(std::move(name), getNextTemporary()));
  }
  
  // labels only begin with "%" when used
  std::string addLabel(std::string name, i32 index) {
    std::stringstream nameBuilder;
    nameBuilder << parent->prefix << name << index;
    return std::move(nameBuilder.str());
  }

  std::pair<i32, std::string> addConstant() {
    std::stringstream name;
    name << "@" << prefix << nextAnonymousConstant;
    return {nextAnonymousConstant++, std::move(name.str())};
  }

  std::string addConstant(std::string_view name) {
    std::stringstream ss;
    ss << "@" << prefix << name;
    return std::move(ss.str());
  }

  std::string addGlobal(std::string_view name) {
    std::stringstream ss;
    ss << "%" << prefix << name;
    return std::move(ss.str());
  }

  std::string addGlobal() {
    std::stringstream ss;
    ss << "%" << prefix << nextAnonymousConstant++;
    return std::move(ss.str());
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

  static Environment* baseEnvironment() {
    static Environment baseEnvironment(Map {
      {"printf", new Reference(
        std::move(LLVMFunction{.definition = "declare i32 @printf(ptr noalias nocapture, ...)", .usage = "i32 (i8*, ...) @printf"})
      )},
      {"malloc", new Reference(
        std::move(LLVMFunction{.definition = "declare ptr @malloc(i64)", .usage = "ptr (i64) @malloc"})
      )},
      {"free", new Reference(
        std::move(LLVMFunction{.definition = "declare void @free(ptr)", .usage = "void (ptr) @free"})
      )},
      {"bool", Reference::typeReference(Types::Pool().boolean)},
      {"s8", Reference::typeReference(Types::Pool().s8)},
      {"s16", Reference::typeReference(Types::Pool().s16)},
      {"s32", Reference::typeReference(Types::Pool().s32)},
      {"s64", Reference::typeReference(Types::Pool().s64)},
      {"u8", Reference::typeReference(Types::Pool().u8)},
      {"u16", Reference::typeReference(Types::Pool().u16)},
      {"u32", Reference::typeReference(Types::Pool().u32)},
      {"u64", Reference::typeReference(Types::Pool().u64)},
      {"f16", Reference::typeReference(Types::Pool().f16)},
      {"f32", Reference::typeReference(Types::Pool().f32)},
      {"f64", Reference::typeReference(Types::Pool().f64)},
      {"usize", Reference::typeReference(Types::Pool().usize)},
      {"isize", Reference::typeReference(Types::Pool().isize)},
    });

    return &baseEnvironment;
  }
};
