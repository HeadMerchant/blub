#pragma once
#include "common.h"
#include "fmt/format.h"
#include "fmt/ostream.h"
#include "parser.h"
#include "tokenizer.h"
#include "types.h"
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <tsl/ordered_map.h>
#include <unordered_map>
#include <variant>
#include <vector>

class Environment;

class LLVMFunction {
public:
  std::string definition;
  std::string usage;
};

enum class StorageType { REGISTER, STACK };

enum class CompTimeStatus { ComptimeOnly, Runtime, ComptimeKnown };

struct TranslationUnit;
class GenericValue {
public:
  TranslationUnit* translationUnit;
  Environment* definitionEnvironment;
  NodeIndex astNode;
  std::vector<std::string_view> parameterNames;
  std::unordered_map<TupleIndex, Reference*> cache;
  std::string_view name;
};

struct IntLiteral {
  int64_t value;
  TypeIndex type;

  IntLiteral(int64_t value) : value(value), type(Pool().intLiteral) {};
  IntLiteral(int64_t value, TypeIndex type) : value(value), type(type) {};
};

template <> struct fmt::formatter<IntLiteral> : fmt::formatter<int64_t> {
  template <typename FormatContext>
  auto format(const IntLiteral& obj, FormatContext& ctx) const {
    return fmt::formatter<int64_t>::format(obj.value, ctx);
  }
};
struct FloatLiteral {
  double value;
  Float::Precision precision;
  FloatLiteral(double value, Float::Precision precision = Float::Precision::f32)
      : value(value), precision(precision) {}
};

enum class ValueScope { Local, Global };
struct RegisterValue {
  RegisterName name;
  TypeIndex type;
  ValueScope scope;

  friend std::ostream& operator<<(std::ostream& o, const RegisterValue& x) {
    fmt::print(o, "{}{}", x.scope == ValueScope::Local ? "%" : "@", x.name);
    return o;
  }
};
template <> struct fmt::formatter<RegisterValue> : ostream_formatter {};

class StackValue {
public:
  RegisterName name;
  TypeIndex type;
  ValueScope scope;

  friend std::ostream& operator<<(std::ostream& o, const StackValue& x) {
    fmt::print(o, "{}{}", x.scope == ValueScope::Local ? "%" : "@", x.name);
    return o;
  }
};
template <> struct fmt::formatter<StackValue> : ostream_formatter {};

class Function {
public:
  FunctionType type;
  RegisterName globalName;
};

class BoundFunction {
public:
  std::variant<StackValue, RegisterValue> self;
  Function* method;
  Reference getSelf();
};

struct Never {
  TypeIndex type = TypeIndex::null();
};

using RangeBound = std::variant<IntLiteral, RegisterValue>;
struct Range {
  RangeBound lower;
  std::optional<RangeBound> upper;

  bool hasUpper() {
    return upper.has_value();
  }

  TypeIndex getType() {
    TypeIndex lowerType = Pool().intLiteral;
    TypeIndex upperType = Pool().intLiteral;

    if (auto regVal = std::get_if<RegisterValue>(&lower)) {
      lowerType = regVal->type;
    }
    if (upper) {
      if (auto regVal = std::get_if<RegisterValue>(&upper.value())) {
        upperType = regVal->type;
      }
    }

    return Pool().coerce(lowerType, upperType);
  }
};

struct VoidRef {};
struct ZeroInit {};
struct CudaEnv {
  Environment* env;
};
struct Kernel {
  u32 index;
  FunctionType function;
};
struct InstancedKernel {
  Kernel kernel;
  RegisterValue blockDim;
  RegisterValue gridDim;
};

using UnderlyingValue = std::variant<
  TypeIndex,
  Environment*,
  CudaEnv,
  GenericValue,
  bool,
  StackValue,
  FloatLiteral,
  RegisterValue,
  IntLiteral,
  Function,
  BoundFunction,
  Never,
  Range,
  VoidRef,
  ZeroInit,
  Kernel,
  InstancedKernel>;

struct Reference {
  using Opt = OptionalType;
  UnderlyingValue value;
  struct {
    int isMutable : 1 = false;
    int isInitialized : 1 = false;
  } flags;

  static Reference Void() {
    return Reference(VoidRef{});
  }

  Opt unboxType() {
    if (auto type = std::get_if<TypeIndex>(&value)) return *type;
    return TypeIndex::null();
  }

  Environment* unboxEnv() {
    if (auto env = unbox<Environment*>()) {
      return *env;
    }
    return nullptr;
  }

  Function* unboxFunction() {
    if (auto func = std::get_if<Function>(&value)) return func;
    return nullptr;
  }

  bool isLiteral() {
    return isAny<IntLiteral, RegisterValue, FloatLiteral>(value);
  }

  Opt isAssignableTo(TypeIndex targetType) {
    auto type = getType();
    return Pool().isAssignable(type, targetType);
  }

  Reference coerceFloat(Float::Precision precision) const {
    auto type = getType();
    if (auto literal = std::get_if<FloatLiteral>(&this->value)) {
      auto value = literal->value;
      return Reference(FloatLiteral(value, precision));
    }
    if (Pool().isFloat(type)) return *this;
    if (auto intLit = std::get_if<IntLiteral>(&value)) {
      return Reference(FloatLiteral(intLit->value));
    }
    throw std::invalid_argument(
      fmt::format(
        "Attempted to coerce value of type {} to float",
        TypeName(type)
      )
    );
  }

  TypeIndex getType() const {
    return std::visit(
      overloaded{
        [](RegisterValue x) { return x.type; },
        [](StackValue x) { return x.type; },
        [](bool x) { return Pool()._bool; },
        [](Reference* x) { return x->getType(); },
        [](IntLiteral x) { return x.type; },
        [](FloatLiteral x) { return Pool().floatLiteral; },
        [](Never x) { return x.type ? Pool().never : x.type; },
        [](TypeIndex) { return Pool().type; },
        [](Environment*) { return Pool().environment; },
        [](CudaEnv) { return Pool().environment; },
        [](GenericValue) { return Pool().generic; },
        [](Function x) { return Pool().addFunction(x.type); },
        [](BoundFunction x) { return Pool().boundFunction(x.method->type); },
        [](Range x) { return Pool().rangeLiteral; },
        [](VoidRef x) { return Pool()._void; },
        [](ZeroInit) { return Pool().never; },
        [](Kernel x) { return Pool().addFunction(x.function); },
        [](InstancedKernel x) { return Pool().addFunction(x.kernel.function); },
      },
      value
    );
  }

  StackValue* lValue() {
    return unbox<StackValue>();
  }

  bool isComptime() {
    auto comptime = isAny<
      TypeIndex,
      IntLiteral,
      FloatLiteral,
      bool,
      Function,
      GenericValue,
      Environment*,
      Kernel,
      CudaEnv>(value);
    if (comptime) {
      return true;
    }
    return false;
  }

  int64_t* getInt() {
    if (auto x = unbox<IntLiteral>()) return &x->value;
    return nullptr;
  }

  std::optional<double> getFloat() {
    if (auto x = std::get_if<FloatLiteral>(&value)) return x->value;
    if (auto x = std::get_if<IntLiteral>(&value)) return x->value;
    return std::nullopt;
  }

  template <typename T> T* unbox() {
    if (auto x = std::get_if<T>(&value)) return x;
    return nullptr;
  }

  double unboxFloat() {
    if (auto floatVal = unbox<FloatLiteral>()) {
      return floatVal->value;
    } else if (auto intVal = unbox<IntLiteral>()) {
      return intVal->value;
    }
    throw std::invalid_argument("Can't unbox float for non-float/int value");
  }

  bool* unboxBool() {
    return unbox<bool>();
  }

  friend std::ostream& operator<<(std::ostream& o, const Reference& x) {
    std::visit(
      overloaded{
        [&o](TypeIndex x) { o << LlvmName(x); },
        [&o](bool x) { o << x; },
        [&o](StackValue x) { o << x; },
        [&o](FloatLiteral x) {
          double exactValue;
          switch (x.precision) {
          case Float::Precision::f16: {
            TODO("Support f16/half-precision floats");
          }
          case Float::Precision::f32: {
            exactValue = (double)(float)x.value;
            break;
          }
          case Float::Precision::f64: {
            exactValue = x.value;
            break;
          }
          }
          auto val = std::bit_cast<u64>(exactValue);
          fmt::print(o, "0x{:016X}", val);
        },
        [&o](RegisterValue x) { o << x; },
        [&o](IntLiteral x) { o << x.value; },
        [&o](Function x) { fmt::print(o, "@\"{}\"", x.globalName); },
        [&o](BoundFunction x) {
          fmt::print(o, "@\"{}\"", x.method->globalName);
        },
        [&o](Reference* x) { o << *x; },
        [&o](Environment* x) {
          TODO("Can't convert environments into llvm names");
        },
        [&o](CudaEnv x) { TODO("Can't convert environments into llvm names"); },
        [&o](GenericValue x) {
          TODO("Can't convert environments into llvm names");
        },
        [&o](Never x) { o << "undef"; },
        [&o](Range x) { TODO("Can't convert ranges into llvm names"); },
        [&o](VoidRef x) { TODO("Can't convert void into llvm name"); },
        [&o](ZeroInit) { o << "zeroinitializer"; },
        [&o](Kernel x) { o << x.index; },
        [&o](InstancedKernel x) { o << x.kernel.index; },
      },
      x.value
    );
    return o;
  }

  RangeBound rangeBound() {
    return std::visit(
      overloaded{
        [](IntLiteral x) -> RangeBound { return x; },
        [](RegisterValue x) -> RangeBound {
          if (Pool().isInt(x.type)) return x;
          TODO("Error for value that can't be used as a range bound");
          return IntLiteral(0);
        },
        [](Reference* x) -> RangeBound { return x->rangeBound(); },
        [](auto& x) -> RangeBound {
          TODO("Error for value that can't be used as a range bound");
          return IntLiteral(0);
        }
      },
      value
    );
  }

  static Reference unboxBound(RangeBound& bound) {
    return std::visit(overloaded{[](auto x) { return Reference(x); }}, bound);
  }
};
template <> struct fmt::formatter<Reference> : ostream_formatter {};

enum class EnvType { Global, Function };

class Environment {
public:
  static Logger log;
  static u32 globalIndex;
  ordered_map<string_view, Reference> defs;
  vector<Tokenizer::TokenLocation> defLocations;
  std::vector<Environment*> imports;
  std::string prefix;
  std::vector<Environment*> usings;

  u32 nextTemporary = 1;
  u32 lastTemporary() {
    return nextTemporary - 1;
  }
  bool quotePrefixedNames;
  static u32 nextGlobalTemporary;
  static u32 nextStructIndex;

  u32 currentLabel = 0;
  bool hasReturned = false;

  using WitnessTable =
    unordered_map<TypeIndex, unordered_map<Identifier, Reference>>;

  struct Impls {
    WitnessTable witnesses;
  };
  Impls impls;
  vector<Impls*> importedImpls;

  struct Scope {
    u32 namesIndex = 0;
    OptionalType self = TypeIndex::null();
    OptionalType returnType = TypeIndex::null();
    struct {
      bool hasReturned : 1 = false;
    };
    EnvType envType = EnvType::Global;
  };
  std::vector<Scope> scopes;

  std::string_view getPrefix() {
    return prefix;
  }

  static unordered_map<string_view, Reference> defaults;

  bool isDefined(std::string_view name) {
    return defs.contains(name);
  }

  Reference* define(
    std::string_view name,
    Reference value,
    Tokenizer::TokenLocation location
  ) {
    if (log.canLog()) log("Defining {}: {}", name, TypeName(value.getType()));
    auto [ref, succeeded] = defs.emplace(name, value);
    if (succeeded) {
      defLocations.push_back(location);
      return &ref.value();
    }
    return nullptr;
  }

  Tokenizer::TokenLocation* definitionLocation(string_view name) {
    auto def = defs.find(name);
    if (def != defs.end()) {
      return &defLocations[std::distance(defs.begin(), def)];
    }
    return nullptr;
  }

private:
  Reference* findLocal(string_view name) {
    auto value = defs.find(name);
    if (value != defs.end()) {
      return &value.value();
    }
    return nullptr;
  }

public:
  Reference* find(string_view name) {
    if (auto found = findLocal(name)) {
      return found;
    }

    for (auto& import : usings) {
      if (auto found = import->findLocal(name)) {
        return found;
      }
    }

    auto value = defaults.find(name);
    if (value != defaults.end()) {
      return &value->second;
    }
    return nullptr;
  }

  EnvType envType() {
    if (scopes.empty()) return EnvType::Global;
    return scopes.back().envType;
  }

  u32 addTemporary() {
    switch (envType()) {
    case EnvType::Function: {
      u32 index = nextTemporary;
      nextTemporary += 1;
      return index;
    }
    case EnvType::Global: {
      u32 index = nextGlobalTemporary;
      nextGlobalTemporary += 1;
      return index;
    }
    default: {
      throw std::invalid_argument("Internal error: missed environment type");
    }
    }
  }

  RegisterValue makeTemporary(TypeIndex type) {
    return RegisterValue(addTemporary(), type);
  }

  StackValue makeGlobal(TypeIndex type) {
    return StackValue(globalIndex++, type, ValueScope::Global);
  }

  std::string addConstant(std::string_view name) {
    return fmt::format("{}{}", prefix, name);
  }

  std::string addGlobal(std::string_view name) {
    return fmt::format("%\"{}{}\"", prefix, name);
  }

  std::string addGlobal() {
    return fmt::format("%.anon.{}{}", prefix, globalIndex++);
  }

  u32 nextGlobalIndex() {
    return globalIndex++;
  }

  void debug(u32 depth = 0) {
    if (!log.canLog()) return;
    if (depth == 0) fmt::println("Symbols:");
    for (auto [name, _] : defs) {
      fmt::println("{: >{}}{}", "", depth * 2, name);
    }
    if (usings.empty()) return;
    if (depth == 0) log("Using:");
    for (auto x : usings) {
      x->debug(depth + 1);
    }
  }

  Reference* getStatic(Impls& impl, TypeIndex type, Identifier name) {
    auto typeAssociates = impl.witnesses.find(type);
    // if (name == "size") {
    //   auto result = typeAssociates->second.("size");
    //   return Reference(IntLiteral(sizing.byteSize));
    // } else if (name == "alignment") {
    //   return Reference(IntLiteral(sizing.alignment.byteAlignment()));
    // } else if (name == "bitSize") {
    //   return Reference(IntLiteral(sizing.bitSize));
    // }

    if (typeAssociates == impl.witnesses.end()) return nullptr;
    auto testValue = typeAssociates->second.find(name);
    if (testValue != typeAssociates->second.end()) {
      return &testValue->second;
    }
    log("Failed to find {}.{}; available statics:", TypeName(type), name);
    for (auto& [name, value] : typeAssociates->second) {
      log("\t{} = {}", name, value);
    }
    return nullptr;
  }

  pair<TypeIndex, Reference*> getMethod(TypeIndex type, Identifier fieldName) {
    auto staticValue = getStatic(type, fieldName);
    if (staticValue) {
      log("Testing if {}.{} is a method", TypeName(type), fieldName);
      log("{}", *staticValue);
      if (auto function = staticValue->unboxFunction()) {
        auto paramTypes = Pool().tupleElements(function->type.parameters);
        if (paramTypes.empty()) {
          return {{0}, nullptr};
        }

        auto selfType = paramTypes[0];
        if (selfType == type) {
          return {type, staticValue};
        }
        if (auto ptrType = Pool().dereference(selfType); type == ptrType) {
          TODO("Calling methods on pointers");
        }
        return {{0}, nullptr};
      } else {
        return {{0}, nullptr};
      }
    }
    for (auto targetType : Pool().coerceableTypes(type)) {
      if (auto method = getMethod(targetType, fieldName); method.second) {
        return method;
      }
    }
    return {{0}, nullptr};
  }

  Reference* getStatic(TypeIndex type, Identifier name) {
    log("Testing local impl for {}.{}", TypeName(type), name);
    if (auto val = getStatic(impls, type, name)) {
      log("Using local or parent definition");
      return val;
    }

    u32 debugI = 0;
    log("#imported impls: {}", importedImpls.size());
    for (auto impl : importedImpls) {
      log("Testing {}th imported impl", debugI);
      if (auto val = getStatic(*impl, type, name)) {
        log("Using imported definition: {}", *val);
        return val;
      }
      debugI++;
    }

    return nullptr;
  }

  bool hasLocalImpl(TypeIndex type) {
    auto typeAssociates = impls.witnesses.find(type);
    return typeAssociates != impls.witnesses.end();
  }

  struct [[nodiscard]] ScopeGuard {
    Environment& env;
    u32 scopeIndex;

    ScopeGuard(const ScopeGuard&) = delete;
    ScopeGuard& operator=(const ScopeGuard&) = delete;

    explicit ScopeGuard(Environment& env)
        : env(env), scopeIndex(env.scopes.size()) {
      log("Pushing scope");
    }

    ~ScopeGuard() {
      assert(env.scopes.size() == scopeIndex);
      log(
        "Popping scope: Current: {}, Prev: {}",
        env.defs.size(),
        env.scopes.back().namesIndex
      );
      env.popScope();
    }
  };

  ScopeGuard pushScope() {
    fmt::println("Creating pushed scope");
    u32 length = defs.size();
    if (scopes.empty()) {
      scopes.push_back({length});
    } else {
      auto currentScope = scopes.back();
      currentScope.namesIndex = length;
      scopes.push_back(currentScope);
    }
    return ScopeGuard(*this);
  }

private:
  void popScope() {
    assert(defLocations.size() == defs.size());
    u32 prevScopeSize = scopes.back().namesIndex;
    scopes.pop_back();
    while (defs.size() != prevScopeSize) {
      log("Removing '{}' from scope", defs.back().first);
      defLocations.pop_back();
      defs.pop_back();
    }
  }

public:
  TypeIndex returnType() {
    if (!scopes.empty()) {
      return scopes.back().returnType;
    }
    return TypeIndex::null();
  }

  TypeIndex selfType() {
    if (scopes.empty()) {
      return TypeIndex::null();
    }
    return scopes.back().self;
  }

  static u32 structIndex() {
    return nextStructIndex++;
  }
};
