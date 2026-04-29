#pragma once
#include "common.h"
#include "fmt/format.h"
#include "fmt/ostream.h"
#include "parser.h"
#include "types.h"
#include <cstdint>
#include <optional>
#include <string_view>
#include <unordered_map>
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
class GenericValue {
public:
  TranslationUnit& translationUnit;
  Environment& definitionEnvironment;
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

using RegisterName = std::variant<std::string_view, u32>;

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
  std::string_view globalName;
};

class BoundFunction {
public:
  std::variant<StackValue, RegisterValue> self;
  Function& method;
  Reference getSelf();
};

struct Never {};

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

    return Pool().coerce(lowerType, upperType).value();
  }
};

struct Reference;
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
  Reference*,
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

  static Reference assigned(const Reference& other) {
    if (auto boxed = std::get_if<Reference*>(&other.value)) {
      return Reference(*boxed);
    }
    return Reference(other.value);
  }

  static Reference Void() {
    return Reference(VoidRef{});
  }

  Opt unboxType() {
    if (auto type = std::get_if<TypeIndex>(&value)) return *type;
    if (auto type = std::get_if<Reference*>(&value))
      return (*type)->unboxType();
    return std::nullopt;
  }

  Function* unboxFunction() {
    if (auto func = std::get_if<Function>(&value)) return func;
    if (auto func = std::get_if<Reference*>(&value))
      return (*func)->unboxFunction();
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
    if (auto ref = std::get_if<Reference*>(&value)) {
      return (*ref)->coerceFloat(precision);
    }
    throw std::invalid_argument(
      fmt::format(
        "Attempted to coerce value of type {} to float",
        TypeName(type)
      )
    );
  }

  static optional<std::tuple<TypeIndex, Reference, Reference>> coerceType(
    Reference* a,
    Reference* b
  ) {
    auto typeA = a->getType();
    auto typeB = b->getType();

    auto targetType = Pool().coerce(typeA, typeB);
    if (!targetType) return std::nullopt;

    auto type = *targetType;
    if (type == Pool().floatLiteral) {
      if (typeA == Pool().intLiteral) {
        auto value = a->unbox<IntLiteral>()->value;
        return std::make_tuple(
          type,
          Reference(FloatLiteral(value)),
          Reference(b)
        );
      }

      if (typeB == Pool().intLiteral) {
        auto value = b->unbox<IntLiteral>()->value;
        return std::make_tuple(
          type,
          Reference(a),
          Reference(FloatLiteral(value))
        );
      }
    }

    return std::make_tuple(type, Reference(a), Reference(b));
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
        [](Never) { return Pool().never; },
        [](TypeIndex) { return Pool().type; },
        [](Environment*) { return Pool().environment; },
        [](CudaEnv) { return Pool().environment; },
        [](GenericValue) { return Pool().generic; },
        [](Function x) { return Pool().addFunction(x.type); },
        [](BoundFunction x) { return Pool().addFunction(x.method.type); },
        [](Range x) { return Pool().rangeLiteral; },
        [](VoidRef x) { return Pool()._void; },
        [](ZeroInit) { return Pool().never; },
        [](Kernel x) { return Pool().addFunction(x.function); },
        [](InstancedKernel x) { return Pool().addFunction(x.kernel.function); }
      },
      value
    );
  }

  using OptStack = std::optional<StackValue>;
  OptStack lValue() {
    return std::visit(
      overloaded{
        [](StackValue x) -> OptStack { return x; },
        [](Reference* x) -> OptStack { return x->lValue(); },
        [](auto x) -> OptStack { return std::nullopt; },
      },
      value
    );
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

  template <typename T> T* unbox() {
    if (auto x = std::get_if<T>(&value)) return x;
    if (auto x = std::get_if<Reference*>(&value)) return (*x)->unbox<T>();
    return nullptr;
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
        [&o](BoundFunction x) { o << x.method.globalName; },
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
  Logger log;
  static u32 globalIndex;
  std::unordered_map<std::string_view, Reference> defs;
  std::vector<Environment*> imports;
  std::string prefix;
  // TODO: scoping
  std::vector<std::vector<std::string_view>> scope;
  std::vector<Environment*> usings;
  EnvType envType;

  u32 nextTemporary = 1;
  u32 lastTemporary() {
    return nextTemporary - 1;
  }
  bool quotePrefixedNames;
  static u32 nextGlobalTemporary;
  u32 basicBlock;
  bool hasReturned = false;

  Environment* parent;
  using WitnessTable =
    unordered_map<TypeIndex, unordered_map<Identifier, Reference>>;
  struct Impls {
    WitnessTable witnesses;

    // ~Impls() {
    //   fmt::println("killing witnesses");
    // }
  };
  Impls impls;
  vector<Impls*> importedImpls;

  std::string_view getPrefix() {
    return prefix;
  }

  Environment()
      : parent(Environment::baseEnvironment()), imports(), defs(), prefix(""),
        envType(EnvType::Global), basicBlock(0u), log(LogLevel::Compile) {}

  Environment(
    Environment* parent,
    std::string_view prefix,
    bool quoteTemporaries = false
  )
      : parent(parent), imports(), defs(),
        quotePrefixedNames(quoteTemporaries | parent->quotePrefixedNames),
        envType(EnvType::Function), log(LogLevel::Compile) {
    std::stringstream ss;
    ss << parent->prefix << prefix << ".";
    this->prefix = ss.str();
  }

  Environment(
    Environment* parent,
    std::string prefix,
    bool quoteTemporaries = false
  )
      : parent(parent), prefix(prefix), imports(), defs(),
        quotePrefixedNames(quoteTemporaries | parent->quotePrefixedNames) {}
  Environment(
    std::unordered_map<std::string_view, Reference> defs,
    Environment* parent = nullptr
  )
      : parent(parent), defs(defs), imports() {}

  Reference* find(std::string_view name) {
    Environment* env = this;
    while (env) {
      auto it = env->defs.find(name);
      if (it != env->defs.end()) {
        return &it->second;
      }
      for (auto imported : env->usings) {
        auto it = imported->defs.find(name);
        if (it != imported->defs.end()) {
          return &it->second;
        }
      }
      env = env->parent;
    }

    return nullptr;
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

  u32 addTemporary() {
    switch (envType) {
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
    }
  }

  RegisterValue makeTemporary(TypeIndex type) {
    return RegisterValue(addTemporary(), type);
  }

  std::string addLabel(std::string name) {
    return addLabel(std::move(name), addTemporary());
  }

  // labels only begin with "%" when used
  std::string addLabel(std::string name, u32 index) {
    return fmt::format("{}{}{}", parent->prefix, name, index);
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

  static Environment* baseEnvironment();

  void debug(u32 depth = 0) {
    if (!(log.logLevel & log.globalLevels)) return;
    if (depth == 0) fmt::println("Symbols:");
    for (auto& [name, _] : defs) {
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

    if (parent) {
      log("Testing parent");
      return parent->getStatic(type, name);
    }

    return nullptr;
  }

  bool hasLocalImpl(TypeIndex type) {
    auto typeAssociates = impls.witnesses.find(type);
    return typeAssociates != impls.witnesses.end();
  }
};
