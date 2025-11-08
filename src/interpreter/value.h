#pragma once
#include "fmt/format.h"
#include "fmt/ostream.h"
#include "parser.h"
#include "types.h"
#include <cassert>
#include <optional>
#include <variant>
#include <vector>

struct Reference;

using StringType = std::string;
using ArrayType = std::vector<Reference*>;
using FloatType = float;

class Environment;
std::string_view getEnvPrefix(Environment* env);

using Definition = std::pair<std::string_view, TypeIndex>;

using IntType = int;

class LLVMFunction {
public:
  std::string definition;
  std::string usage;
};

enum class StorageType { REGISTER, STACK };

enum class CompTimeStatus { ComptimeOnly, Runtime, ComptimeKnown };

struct TranslationUnit;
class Generic {
public:
  TranslationUnit& translationUnit;
  Environment& definitionEnvironment;
  NodeIndex astNode;
  std::vector<std::string_view> parameterNames;
  std::unordered_map<Types::TupleIndex, Reference*, Types::TupleIndex::Hash> cache;
  std::string_view name;

  // Generic(Generic&& x): translationUnit(x.translationUnit),
  // definitionEnvironment(x.definitionEnvironment), astNode(x.astNode),
  // parameterNames(x.parameterNames), cache(x.cache), name(x.name) {}
};

struct IntLiteral {
  int64_t value;
  TypeIndex type;
};

struct FloatLiteral {
  double value;
};

template <class... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};
using RegisterName = std::variant<std::string_view, i32>;

struct RegisterValue {
  RegisterName name;
  TypeIndex type;
};

class StackValue {
public:
  RegisterName name;
  TypeIndex type;
};

// struct StubValue {
//     TypeIndex type;
//     std::string_view name;
// };

class Function {
public:
  Types::FunctionType type;
  std::string globalName;

  void llvmDeclaration(std::ostream& o, std::optional<std::span<std::string_view>> paramNames) {
    TypeIndex returnType = type.returnType;
    auto paramTypes = Types::Pool().tupleElements(type.parameters);

    auto forwardDeclare = !paramNames.has_value();
    if (paramNames.has_value()) {
      assert(paramNames->size() == paramTypes.size());
    }
    fmt::print(o, "{} ", (forwardDeclare ? "declare" : "define"));

    // TODO
    // o <<
  }
};

class BoundFunction {
public:
  Reference& self;
  Function& method;
};

class Global {
public:
  RegisterName name;
  TypeIndex type;
};

struct Never {};

struct Reference;
using UnderlyingValue =
  std::variant<TypeIndex, Environment*, Generic, bool, StackValue, FloatLiteral, RegisterValue, IntLiteral, Function, BoundFunction, Reference*, Global, Never>;

struct Reference {
  using Opt = Types::OptionalType;
  UnderlyingValue value;
  struct {
    int isMutable : 1 = false;
    int isInitialized : 1 = false;
  } flags;

  static Reference assigned(const Reference& other) {
    if (auto boxed = std::get_if<Reference*>(&other.value)) {
      return Reference(*boxed);
    }
    return Reference(other.value);
  }

  static Reference Void() {
    return Reference(Types::Pool()._void);
  }

  // Reference(TypeIndex type): type(type), isInitialized(false) {}
  // Reference(TypeIndex type, LiteralValue value): type(type),
  // value(std::move(value)) {} Reference(Types::Intrinsic type):
  // type(Types::indexOf(type)), isInitialized(false) {} Reference(LLVMFunction
  // function): type(Types::indexOf(Types::Intrinsic::LLVM_FUNCTION)),
  // value(std::move(function)), storageType(StorageType::REGISTER) {}
  // Reference(Environment* value):
  // type(Types::indexOf(Types::Intrinsic::ENVIRONMENT)), value(value),
  // isMutable(false), isInitialized(true) {}

  Opt unboxType() {
    if (auto type = std::get_if<TypeIndex>(&value)) return *type;
    if (auto type = std::get_if<Reference*>(&value)) return (*type)->unboxType();
    return std::nullopt;
  }

  std::optional<Function*> unboxFunction() {
    if (auto func = std::get_if<Function>(&value)) return func;
    if (auto func = std::get_if<Reference*>(&value)) return (*func)->unboxFunction();
    return std::nullopt;
  }

  bool isLiteral() {
    return isAny<IntLiteral, RegisterValue, FloatLiteral>(value);
  }

  Opt isAssignableTo(TypeIndex targetType) {
    static Types::TypePool& TypePool = Types::Pool();
    auto type = getType();
    return TypePool.isAssignable(type, targetType);
  }

  Reference coerceFloat() const {
    auto type = getType();
    if (Types::Pool().isFloat(type)) return *this;
    if (auto intLit = std::get_if<IntLiteral>(&value)) {
      return Reference(FloatLiteral(intLit->value));
    }
    if (auto ref = std::get_if<Reference*>(&value)) {
      return (*ref)->coerceFloat();
    }
    throw std::invalid_argument(fmt::format("Attempted to coerce value of type {} to float", TypeName(type)));
  }

  static optional<std::tuple<TypeIndex, Reference, Reference>> coerceType(Reference* a, Reference* b) {
    auto typeA = a->getType();
    auto typeB = b->getType();

    auto targetType = Types::Pool().coerce(typeA, typeB);
    if (!targetType) return std::nullopt;

    auto type = *targetType;
    if (type != Types::Pool().floatLiteral) {
      if (typeA == Types::Pool().intLiteral) {
        auto value = a->unbox<IntLiteral>().value()->value;
        return std::make_tuple(type, Reference(FloatLiteral(value)), Reference(b));
      }

      if (typeB == Types::Pool().intLiteral) {
        auto value = b->unbox<IntLiteral>().value()->value;
        return std::make_tuple(type, Reference(a), Reference(FloatLiteral(value)));
      }
    }

    return std::make_tuple(type, Reference(a), Reference(b));
  }

  TypeIndex getType() const {
    return std::visit(
      overloaded{
        [](RegisterValue x) { return x.type; },
        [](StackValue x) { return x.type; },
        [](bool x) { return Types::Pool()._bool; },
        [](Reference* x) { return x->getType(); },
        [](IntLiteral x) { return Types::Pool().intLiteral; },
        [](FloatLiteral x) { return Types::Pool().floatLiteral; },
        [](Never) { return Types::Pool().never; },
        [](TypeIndex) { return Types::Pool().type; },
        [](Environment*) { return Types::Pool().environment; },
        [](Generic) { return Types::Pool().generic; },
        [](Function x) { return Types::Pool().addFunction(x.type); },
        [](BoundFunction x) { return Types::Pool().addFunction(x.method.type); },
        [](Global x) { return x.type; },
      },
      value);
  }

  using OptStack = std::optional<StackValue>;
  OptStack lValue() {
    return std::visit(
      overloaded{[](StackValue x) -> OptStack { return x; }, [](Reference* x) -> OptStack { return x->lValue(); }, [](auto x) -> OptStack { return std::nullopt; }},
      value);
  }

  std::optional<i32*> structFieldIndex() {
    // if (i32* indexPointer = std::get_if<i32>(&value)) {
    //     return indexPointer;
    // } else {
    //     return std::nullopt;
    // }
    TODO("Struct field index");
  }

  std::optional<Generic*> generic() {
    TODO("Generic");
    // if (auto boxed = std::get_if<Generic>(&value)) {
    //     return boxed;
    // } else {
    //     return std::nullopt;
    // }
  }

  bool isComptime() {
    auto comptime = isAny<TypeIndex, IntLiteral, FloatLiteral, bool, Function, Generic>(value);
    if (comptime) {
      return true;
    } else if (auto ref = std::get_if<Reference*>(&value)) {
      return (*ref)->isComptime();
    }
    return false;
  }

  std::optional<bool> unboxBool() {
    if (auto x = std::get_if<bool>(&value)) return *x;
    if (auto x = std::get_if<Reference*>(&value)) return (*x)->unboxBool();
    return std::nullopt;
  }

  std::optional<int64_t> getInt() {
    if (auto x = std::get_if<IntLiteral>(&value)) return x->value;
    if (auto x = std::get_if<Reference*>(&value)) return (*x)->getInt();
    return std::nullopt;
  }

  std::optional<double> getFloat() {
    if (auto x = std::get_if<FloatLiteral>(&value)) return x->value;
    if (auto x = std::get_if<IntLiteral>(&value)) return x->value;
    if (auto x = std::get_if<Reference*>(&value)) return (*x)->getFloat();
    return std::nullopt;
  }

  std::optional<Environment*> unboxEnv() {
    if (auto x = std::get_if<Environment*>(&value)) return *x;
    if (auto x = std::get_if<Reference*>(&value)) return (*x)->unboxEnv();
    return std::nullopt;
  }

  template <typename T> std::optional<T*> unbox() {
    if (auto x = std::get_if<T>(&value)) return x;
    if (auto x = std::get_if<Reference*>(&value)) return (*x)->unbox<T>();
    return std::nullopt;
  }

  friend std::ostream& operator<<(std::ostream& o, const Reference& x) {
    std::visit(
      Types::overloaded{
        [&o](TypeIndex x) { o << LlvmName(x); },
        [&o](bool x) { o << (x ? "true" : "false"); },
        [&o](StackValue x) { fmt::print(o, "%{}", x.name); },
        [&o](FloatLiteral x) { fmt::print(o, "{:#f}", x.value); },
        [&o](RegisterValue x) { fmt::print(o, "%{}", x.name); },
        [&o](IntLiteral x) { o << x.value; },
        [&o](Function x) { o << x.globalName; },
        [&o](BoundFunction x) { o << x.method.globalName; },
        [&o](Reference* x) { o << *x; },
        [&o](Global x) { fmt::print(o, "@{}", x.name); },
        [&o](Environment* x) { TODO("Can't convert environments into llvm names"); },
        [&o](Generic x) { TODO("Can't convert environments into llvm names"); },
        [&o](Never x) { TODO("Can't convert environments into llvm names"); }},
      x.value);
    return o;
  }
};
template <> struct fmt::formatter<Reference> : ostream_formatter {};

class Environment {
public:
  std::unordered_map<std::string_view, Reference> defs;
  std::vector<Environment*> imports;
  std::string prefix;
  // For LLVM
private:
  i32 nextTemporary = 1;
  i32 nextAnonymousConstant = 0;
  bool quotePrefixedNames;

public:
  bool hasReturned = false;

  // Optional
  Environment* parent;

  std::string_view getPrefix() {
    return prefix;
  }

  Environment() : parent(Environment::baseEnvironment()), imports(), defs(), prefix("") {}
  Environment(Environment* parent, std::string_view prefix, bool quoteTemporaries = false)
      : parent(parent), imports(), defs(), quotePrefixedNames(quoteTemporaries | parent->quotePrefixedNames) {
    std::stringstream ss;
    ss << parent->prefix << prefix << ".";
    this->prefix = ss.str();
  }

  Environment(Environment* parent, std::string prefix, bool quoteTemporaries = false)
      : parent(parent), prefix(prefix), imports(), defs(), quotePrefixedNames(quoteTemporaries | parent->quotePrefixedNames) {}
  Environment(std::unordered_map<std::string_view, Reference> defs, Environment* parent = nullptr) : parent(parent), defs(defs), imports() {}

  std::optional<Reference*> find(std::string_view name) {
    Environment* env = this;
    while (env) {
      if (env->defs.contains(name)) {
        return &env->defs[name];
      }
      env = env->parent;
    }
    return std::nullopt;
  }

  bool isDefined(std::string_view name) {
    return defs.contains(name);
  }

  std::optional<Reference*> define(std::string_view name, Reference value) {
    auto [ref, succeeded] = defs.emplace(name, value);
    if (succeeded) {
      return &ref->second;
    } else {
      return std::nullopt;
    }
  }

  i32 addTemporary() {
    i32 index = nextTemporary;
    nextTemporary += 1;
    return index;
  }

  RegisterValue makeTemporary(TypeIndex type) {
    return RegisterValue(addTemporary(), type);
  }

  std::string addLabel(std::string name) {
    return addLabel(std::move(name), addTemporary());
  }

  // labels only begin with "%" when used
  std::string addLabel(std::string name, i32 index) {
    return fmt::format("{}{}{}", parent->prefix, name, index);
  }

  Reference makeGlobal(TypeIndex type) {
    return Reference(Global(nextAnonymousConstant++, type));
  }

  std::string addConstant(std::string_view name) {
    return fmt::format("@\"{}{}\"", prefix, name);
  }

  std::string addGlobal(std::string_view name) {
    return fmt::format("%\"{}{}\"", prefix, name);
  }

  std::string addGlobal() {
    return fmt::format("%{}{}", prefix, nextAnonymousConstant++);
  }

  i32 nextGlobalIndex() {
    return nextAnonymousConstant++;
  }

  static Environment* baseEnvironment();
};
