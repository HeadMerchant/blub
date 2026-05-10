#include "ast.h"
#include "common.h"
#include "parser.h"
#include "tokenizer.h"
#include "typechecker.h"
#include "types.h"
#include "value.h"
#include <ostream>
#include <utility>

namespace fs = std::filesystem;

// Translation unit
struct FunctionStub {
  std::string_view name;
  NodeIndex definitionNode;
  FunctionType functionType;
};

struct SwitchCase {
  stringstream instructions;
  Reference condition;
  Reference result;
  u32 entryBlock;
  RegisterName exitBlock;
  struct {
    bool returns : 1;
    bool breaks : 1;
    bool isNamed : 1;
    bool isDefault : 1;
  };
};

struct CompilerContext {
  struct {
    std::ofstream* outputFileStream;
    std::stringstream globalInitialization;
  } blub;
  struct {
    vector<std::string> linkedLibraries;
    vector<std::string> clangArgs;
  } c;
  struct {
    std::ofstream* outputFileStream;
    std::stringstream globalInitialization;
    vector<std::string> linkedFiles;
  } cuda;

  static CompilerContext& inst() {
    static CompilerContext instance;
    return instance;
  }
};

enum class TargetType { Cpu, Gpu };

#define COMPILER_STACK       \
  std::ostream* outputFile;  \
  OptionalType expectedType; \
  NodeIndex nodeIndex;

// Individual statement
struct Compiler {
  std::ofstream& outputFileStream;
  struct StackItems {
    COMPILER_STACK
  };
  union {
    StackItems stackItems;
    struct {
      COMPILER_STACK
    };
  };
  struct {
    OptionalType type;
    string_view aggregateTypename;
    // Used bc llvm return types with floats are sussy
    RegisterAssignment registers;
  } returns;

  fs::path inputFilePath;
  Environment fileEnvironment;
  std::span<NodeIndex> program;
  std::queue<std::string> globalsStack;
  Logger log;
  TargetType targetType;
  std::vector<FunctionStub> functionStubs;

  using ReturnType = Reference;
  Environment environment;
  optional<string_view> name;

  TypeChecker typeChecker;

  Parser& parser;

  struct TypeValue {
    TypeIndex type;
    bool lValue = false;
  };
  Compiler& compiler;
  Environment& env;
  span<ReturnType> astTypes;

  void setVisitedNode(NodeIndex index) {
    nodeIndex = index;
  }

  ReturnType compile(
    NodeIndex index,
    std::ostream& outFile,
    TypeIndex targetType
  ) {
    auto oldStack = stackItems;
    stackItems =
      {.outputFile = &outFile, .expectedType = targetType, .nodeIndex = index};
    auto result = astVisit(index, parser, *this);
    stackItems = oldStack;
    return result;
  }

  ReturnType compile(
    NodeIndex index,
    TypeIndex targetType = TypeIndex::null()
  ) {
    return compile(index, *outputFile, targetType);
  }

  template <typename... Args>
  void emitLine(fmt::format_string<Args...> fmt, Args&&... args) {
    fmt::println(*outputFile, fmt, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void emit(fmt::format_string<Args...> fmt, Args&&... args) {
    fmt::print(*outputFile, fmt, std::forward<Args>(args)...);
  }

  ReturnType block(Encodings::Block& node) {
    auto scope = env.pushScope();
    for (auto child : node.elements) {
      compile(child);
    }
    auto hasReturned = env.hasReturned;
    if (hasReturned && !env.scopes.empty()) {
      env.scopes.back().hasReturned = true;
    }
    return Reference::Void();
  }

  ReturnType arrayLiteral(Encodings::Block& node) {
    std::vector<Reference> elements;
    elements.reserve(node.elements.size());
    auto type = Pool().infer;

    auto resultType = typeChecker.check(nodeIndex).type;

    if (elements.empty()) {
      parser.crash(nodeIndex, "Empty array literal");
    }

    for (auto element : node.elements) {
      elements.push_back(compile(element));
    }

    auto resultArray = Reference(ZeroInit{});
    u32 i = 0;
    for (auto& element : elements) {
      auto loaded = toRegister(&element);
      auto newArray = environment.makeTemporary(resultType);
      emitLine(
        "{} = insertvalue {} {}, {} {}, {}",
        newArray,
        LlvmName(resultType),
        resultArray,
        LlvmName(type),
        loaded,
        i
      );
      resultArray.value = newArray;
      i++;
    }

    return Reference(resultArray);
  }

  ReturnType when(
    NodeIndex conditionIndex,
    span<pair<NodeIndex, NodeIndex>> caseNodes
  ) {
    TypeIndex resultType;
    bool lValue;
    {
      auto checkResult = typeChecker.check(nodeIndex, expectedType);
      resultType = checkResult.type;
      lValue = checkResult.lValue;
    }

    auto condition = compile(conditionIndex);
    auto loadedCondition = toRegister(&condition);
    auto caseType = condition.getType();

    if (auto enumType = Pool().getEnum(caseType)) {
      // TODO: Exhaustiveness checking
    }

    vector<SwitchCase> cases;
    cases.reserve(caseNodes.size());
    bool hasDefault;
    for (auto [condition, body] : caseNodes) {
      stringstream instruction;
      // Reserve one for loading stack values
      environment.addTemporary();
      u32 block = environment.addTemporary();
      environment.basicBlock = block;
      // Else/default block
      if (condition.value == body.value) {
        hasDefault = true;
        cases.push_back({
          .result = compile(body),
          .entryBlock = block,
          .exitBlock = environment.basicBlock,
          .returns = environment.hasReturned,
          .isDefault = true,
        });
        cases.back().instructions = std::move(instruction);
        environment.hasReturned = false;
        break;
      }
      cases.push_back({
        .condition = compile(condition),
        .result = compile(body),
        .entryBlock = block,
        .exitBlock = environment.basicBlock,
        .returns = environment.hasReturned,
      });
      environment.hasReturned = false;
      SwitchCase& switchCase = cases.back();
      switchCase.instructions = std::move(instruction);

      // if (switchCase.condition.getType() != caseType) {
      //   parser.crash(
      //     condition,
      //     "Expected type for case condition was {}, but was given {}",
      //     TypeName(caseType),
      //     TypeName(switchCase.condition.getType())
      //   );
      // }

      // TODO: non-comptime cases
      if (!switchCase.condition.isComptime()) {
        parser.crash(
          condition,
          "Condition for switch case conditions must be comptime known"
        );
      }

      // TODO: default
    }

    // Reserve one for loading stack values
    // environment.addTemporary();

    auto endBlock = environment.addTemporary();
    auto defaultBlock = hasDefault ? cases.back().entryBlock : endBlock;

    emit(
      "switch {} {}, label %{} [",
      LlvmName(caseType),
      loadedCondition,
      defaultBlock
    );
    for (auto& caseBlock : cases) {
      if (caseBlock.isDefault) break;
      emit(
        " {} {}, label %{}",
        LlvmName(caseType),
        caseBlock.condition,
        caseBlock.entryBlock
      );
    }
    emitLine("]");

    for (auto& caseBlock : cases) {
      emit("{}:\n{}", caseBlock.entryBlock, caseBlock.instructions.str());
      if (!caseBlock.returns) {
        emitLine("br label %{}", endBlock);
      }
    }

    emitLine("{}:", endBlock);
    if (resultType != Pool()._void) {
      auto resultRegister = environment.addTemporary();
      if (lValue) {
        emit("%{} = phi ptr ", resultRegister);
      } else {
        emit("%{} = phi {} ", resultRegister, LlvmName(resultType));
      }

      bool hasMultiple = false;
      for (auto& caseBlock : cases) {
        if (caseBlock.returns) {
          continue;
        }
        if (hasMultiple) {
          emit(", ");
        }
        hasMultiple = true;
        emit("[{}, %{}]", caseBlock.result, caseBlock.exitBlock);
      }
      emitLine("");

      return lValue ? Reference(StackValue(resultRegister, resultType))
                    : Reference(RegisterValue(resultRegister, resultType));
    }

    return Reference::Void();
  }

  ReturnType declaration(Encodings::Declaration& node) {
    // NOTE: don't support using non-identifiers
    bool compileTime = parser.getToken(nodeIndex)->type == TokenType::Colon;

    auto definition = parser.getDefinition(node.definition);
    auto definitionName = definition.name->lexeme;
    if (environment.isDefined(definitionName)) {
      parser.crash(nodeIndex, "Attempt to redefine name '{}'", definitionName);
    }

    if (compileTime) {
      if (definition.type) {
        TODO("Explicitly-typed comptime constants");
      }
      auto value = compile(node.value);

      if (!value.isComptime()) {
        parser.crash(
          nodeIndex,
          "Unable to make comptime constant out of "
          "non-comptime value assigned to '{}'",
          definitionName
        );
      }

      // Allow double define because we checked earlier; functions are bound
      // to environment for recursion
      // TODO: stubbing for mutual recursion
      environment.define(definitionName, value);
      return Reference::Void();
    } else {
      Reference definition = compile(node.definition);

      if (auto assignee = definition.lValue()) {
        TypeIndex expectedType = assignee->type;
        log("Making value: '{}: {}'", assignee->name, TypeName(assignee->type));
        auto value = compile(node.value);
        auto assignedType = value.isAssignableTo(expectedType);
        if (!assignedType) {
          parser.crash(
            nodeIndex,
            "Unable to assign value of type '{}' to to variable '{}' of type "
            "'{}'",
            TypeName(value.getType()),
            assignee->name,
            TypeName(assignee->type)
          );
        }
        assignee->type = assignedType;
        auto byteAlignment =
          Pool().getSizing(assignedType).alignment.byteAlignment();
        switch (assignee->scope) {
        case ValueScope::Local: {
          emitLine(
            "{} = alloca {}, align {}",
            definition,
            LlvmName(assignedType),
            byteAlignment
          );
          break;
        }
        case ValueScope::Global: {
          globalsStack.push(
            fmt::format(
              "{} = global {} undef align {}",
              definition,
              LlvmName(assignedType),
              byteAlignment
            )
          );
          break;
        }
        }

        auto loaded = toRegister(&value);
        emitLine(
          "store {} {}, ptr {}",
          LlvmName(assignedType),
          loaded,
          definition
        );
      } else {
        parser.crash(
          nodeIndex,
          "Internal compiler error: definition didn't result in stack or "
          "global value"
        );
      }
    }

    // TODO: support assignment as expression???
    return Reference::Void();
  }

  ReturnType definition(Encodings::Definition node) {
    auto name = node.name->lexeme;
    TypeIndex type = Pool().infer;
    if (node.type) {
      if (auto typeIndex = compile(node.type).unboxType()) {
        type = typeIndex;
      } else {
        parser.crash(
          nodeIndex,
          "Type for identifier '{}' is not a type",
          node.name->lexeme
        );
      }
    }
    if (
      Reference* definition = environment.define(
        name,
        Reference(StackValue(
          node.name->lexeme,
          type,
          environment.envType == EnvType::Global ? ValueScope::Global
                                                 : ValueScope::Local
        ))
      )
    ) {
      return Reference(definition);
    }

    parser.crash(
      nodeIndex,
      "Definition for identifier '{}' already exists",
      node.name->lexeme
    );
  }

  ReturnType character(TokenPointer token) {
    return Reference(IntLiteral(token->lexeme[0], Pool()._u8));
  }

  std::pair<std::string, u32> escapeSourceString(
    std::string_view str,
    TokenPointer token
  ) {
    std::string escaped;
    u32 byteLength = 0;
    escaped.reserve(str.size());

    bool isEscaping = false;

    // TODO: unicode support
    // for (auto c : str) {
    for (u32 i = 0; i < str.size(); i++) {
      auto c = str[i];
      if (!isEscaping) {
        if (c == '\\') {
          isEscaping = true;
        } else if (c == '\n') {
          escaped.append("\\0A");
          byteLength++;
        } else {
          escaped.push_back(c);
          byteLength++;
        }
      } else {
        switch (c) {
        case '\'':
          escaped.push_back('\'');
          break;
        case 't':
          escaped.append("\\09");
          break;
        case '"':
          escaped.append("\\22");
          break;
        case 'n':
          escaped.append("\\0A");
          break;
        case '\n':
          // Allows for continuing on next line
          break;
        case 'x': {
          char hex1 = str[++i];
          char hex2 = str[++i];
          if (!Tokenizer::isHex(hex1)) {
            parser.crash(
              token,
              "Invalid hex escape '\\x{}{}': {} is not a hexidecimal digit "
              "0-9,A-F",
              hex1,
              hex2,
              hex1
            );
          }
          if (!Tokenizer::isHex(hex2)) {
            parser.crash(
              token,
              "Invalid hex escape '\\x{}{}': {} is not a hexidecimal digit "
              "0-9,A-F",
              hex1,
              hex2,
              hex2
            );
          }
          escaped.append(
            fmt::format("\\{:c}{:c}", toupper(hex1), toupper(hex2))
          );
        } break;
        default:
          parser.crash(token, "Unknown escape sequence \\{}", c);
        }
        isEscaping = false;
        if (c != '\n') {
          byteLength++;
        }
      }
    }
    return {escaped, byteLength};
  }

  Reference makeSliceValue(Reference& dataPointer, Reference& length) {
    auto lengthLoaded = toRegister(&length);
    auto dataType = dataPointer.getType();
    auto type = Pool().sliceOf(dataType);
    auto intermediateResult = env.makeTemporary(type);
    emitLine(
      "{} = insertvalue {} undef, ptr {}, 0",
      intermediateResult,
      LlvmName(type),
      dataPointer
    );
    auto result = env.makeTemporary(Pool().sliceOf(dataType));
    emitLine(
      "{} = insertvalue {} {}, {} {}, 1",
      result,
      LlvmName(type),
      intermediateResult,
      LlvmName(Pool()._usize),
      lengthLoaded
    );
    return Reference(result);
  }

  ReturnType string(TokenPointer token) {
    auto global = environment.makeGlobal(Pool()._u8);
    auto [stringValue, length] = escapeSourceString(token->lexeme, token);
    // TODO: use string types instead of C strings
    globalsStack.push(
      fmt::format(
        "{} = global [{} x i8] c\"{}\" align 1\n",
        global,
        length,
        stringValue
      )
    );
    auto lengthValue = Reference(IntLiteral(length));
    auto ref = Reference(global);
    return makeSliceValue(ref, lengthValue);
  }

  ReturnType nullString(TokenPointer token) {
    auto [stringValue, length] = escapeSourceString(token->lexeme, token);
    auto global = environment.makeGlobal(Pool()._u8);
    // auto global = environment.makeGlobal(Pool().sizedArrayOf(Pool()._u8,
    // length));
    static std::string nullByte = "\\00";
    globalsStack.push(
      fmt::format(
        "{} = global [{} x i8] c\"{}{}\" align 1\n",
        global,
        length + 1,
        stringValue,
        nullByte
      )
    );
    return Reference(
      RegisterValue(global.name, Pool().pointerTo(global.type), global.scope)
    );
  }

  ReturnType decimal(TokenPointer token) {
    float floatValue = std::stof(token->lexeme.data());
    auto type = typeChecker.check(nodeIndex).type;
    if (auto floatType = Pool().unbox<Float>(type)) {
      return Reference(FloatLiteral(floatValue));
    }
    parser.crash(
      nodeIndex,
      "Unable to use float literal for non-float type {}",
      TypeName(type)
    );
  }

  ReturnType integer(TokenPointer token) {
    int64_t intVal = std::stoi(token->lexeme.data());
    auto type = typeChecker.check(nodeIndex).type;
    if (auto floatType = Pool().unbox<Float>(type)) {
      return Reference(FloatLiteral((double)intVal, floatType->precision));
    }
    return Reference(IntLiteral(intVal, type));
  }

  ReturnType hexInt(TokenPointer token) {
    int64_t intVal = std::stoi(token->lexeme.data(), 0, 16);
    return Reference(IntLiteral(intVal));
  }

  ReturnType boolean(bool value) {
    return Reference(value);
  }

  ReturnType identifier(TokenPointer token) {
    auto name = token->lexeme;
    if (auto value = environment.find(name)) {
      return Reference(*value);
    }
    environment.debug();
    parser.crash(nodeIndex, "Identifier \"{}\" not defined", name);
  }

  ReturnType opaque(TokenPointer token) {
    // TODO: naming opaque types
    TypeIndex type = Pool().addOpaque(std::string("Anonymous Opaque"));
    return Reference(type);
  }

  ReturnType self(TokenPointer token) {
    if (auto type = env.selfType()) {
      return Reference(type);
    }
    parser.crash(nodeIndex, "No type 'Self' in context");
  }

  ReturnType undefined(TokenPointer token) {
    return Reference(Never{});
  }

  ReturnType cudaBuiltin(TokenPointer token, string_view callName) {
    if (targetType != TargetType::Gpu) {
      parser.crash(
        nodeIndex,
        "Unable to use cuda builtin '@{}' in non-gpu target",
        token->lexeme
      );
    }
    auto result = environment.makeTemporary(Pool()._u32);
    emitLine("{} = call i32 {}", result, cudaBuiltins[token->type]);
    return Reference(result);
  }

  void doAssignment(Reference assignee, Reference value) {
    if (!assignee.lValue()) {
      parser.crash(nodeIndex, "Unable to assign to non l-value");
    }
    auto loadedValue = toRegister(&value);
    emitLine(
      "store {} {}, ptr {}",
      TypeName(assignee.getType()),
      loadedValue,
      assignee
    );
  }

  ReturnType assign(NodeIndex left, NodeIndex right) {
    auto targetType = typeChecker.check(left);
    if (!targetType.lValue) {
      parser.crash(left, "Unable to assign to non l-value");
    }
    typeChecker.check(right, targetType.type);

    auto value = compile(right, targetType.type);
    auto assignee = compile(left, targetType.type);
    doAssignment(assignee, value);

    // TODO: consider value
    return Reference::Void();
  }

  ReturnType binopAssign(NodeIndex left, NodeIndex right, TokenType binop) {
    typeChecker.check(nodeIndex);
    auto value = binopVisit(left, right, nodeIndex, binop, parser, *this);
    auto assignee = compile(left);
    doAssignment(assignee, value);
    return Reference::Void();
  }

  template <typename T> bool literalComparison(T a, T b, TokenType opType) {
    bool comparison;
    switch (opType) {
    case TokenType::DoubleEqual: {
      comparison = a == b;
      break;
    }
    case TokenType::NotEqual: {
      comparison = a != b;
      break;
    }
    case TokenType::Lt: {
      comparison = a < b;
      break;
    }
    case TokenType::Leq: {
      comparison = a <= b;
      break;
    }
    case TokenType::Gt: {
      comparison = a > b;
      break;
    }
    case TokenType::Geq: {
      comparison = a >= b;
      break;
    }
    default: {
      parser.crash(nodeIndex, "Unknown comparison operation");
    }
    }
    return comparison;
  }

  ReturnType comparison(
    NodeIndex a,
    NodeIndex b,
    TokenType opType,
    string_view instructionName
  ) {
    typeChecker.check(nodeIndex);
    auto aVal = compile(a);
    auto bVal = compile(b);
    if (auto type = Pool().coerce(aVal.getType(), bVal.getType())) {
      if (type == Pool().intLiteral) {
        auto a = aVal.unbox<IntLiteral>()->value;
        auto b = bVal.unbox<IntLiteral>()->value;
        return Reference(literalComparison(a, b, opType));
      } else if (type == Pool().floatLiteral) {
        auto a = aVal.unbox<FloatLiteral>()->value;
        auto b = bVal.unbox<FloatLiteral>()->value;
        return Reference(literalComparison(a, b, opType));
      }

      aVal = toRegister(&aVal);
      bVal = toRegister(&bVal);

      char typePrefix;
      char opPrefix;
      if (Pool().isFloat(type)) {
        typePrefix = 'f';
        opPrefix = 'u';
      } else if (Pool().isSignedInt(type)) {
        typePrefix = 'i';
        opPrefix = 's';
      } else if (Pool().isUnsignedInt(type)) {
        typePrefix = 'i';
        opPrefix = 'u';
      } else {
        TODO("Non-primitive comparison operations");
      }
      RegisterValue result = environment.makeTemporary(Pool()._bool);
      emitLine(
        "{} = {}cmp {}{} {} {}, {}",
        result,
        typePrefix,
        opPrefix,
        instructionName,
        TypeName(type),
        aVal,
        bVal
      );
      return Reference(result);
    } else {
      TODO("Non-primitive comparison operations");
    }
  }

  ReturnType equal(NodeIndex a, NodeIndex b) {
    return comparison(a, b, TokenType::DoubleEqual, "eq");
  }

  ReturnType notEqual(NodeIndex a, NodeIndex b) {
    return comparison(a, b, TokenType::NotEqual, "ne");
  }

  ReturnType lt(NodeIndex a, NodeIndex b) {
    return comparison(a, b, TokenType::Lt, "lt");
  }

  ReturnType gt(NodeIndex a, NodeIndex b) {
    return comparison(a, b, TokenType::Gt, "gt");
  }

  ReturnType leq(NodeIndex a, NodeIndex b) {
    return comparison(a, b, TokenType::Leq, "le");
  }

  ReturnType geq(NodeIndex a, NodeIndex b) {
    return comparison(a, b, TokenType::Geq, "ge");
  }

  Reference callFunction(
    Function* func,
    NodeIndex nodeIndex,
    Environment& environment,
    std::ostream& outputFile,
    Reference* selfArg = nullptr
  ) {
    log(
      "Calling function {} with return type {}",
      func->globalName,
      TypeName(func->type.returnType)
    );
    std::vector<std::string> parameters;

    auto funcType = func->type;
    auto parameterTypes = Pool().tupleElements(funcType.parameters);
    std::vector<bool> setArguments(parameterTypes.size(), false);

    auto [arguments, namedArguments] = getArguments(
      parameterTypes,
      argsNode,
      environment,
      outputFile,
      namedArgs,
      selfArg,
      nodeIndex
    );

    // TODO: optional arguments
    for (auto [name, value] : namedArguments) {
      TODO("Named arguments for function calls");
    }

    if (arguments.size() != parameterTypes.size()) {
      crash(
        nodeIndex,
        "Passed {} arguments, but expected {}",
        arguments.size(),
        parameterTypes.size()
      );
    }

    OutContext callCtx{.outputFile = outputFile, .environment = environment};

    vector<Reference> loadedArgs;
    loadedArgs.reserve(arguments.size());
    for (auto& arg : arguments) {
      loadedArgs.push_back(toRegister(&arg, outputFile, environment));
    }
    // TODO: ZST
    u32 resultRegister = callAbiFunctionWithArgs(callCtx, *func, loadedArgs);
    if (Pool().isVoid(func->type.returnType)) {
      return Reference::Void();
    }
    return Reference(
      RegisterValue{
        .name = resultRegister,
        .type = func->type.returnType,
        .scope = ValueScope::Local
      }
    );
  }

  ReturnType findOperator(
    Reference object,
    TypeIndex objectType,
    string_view methodName,
    span<NodeIndex> arguments = {}
  ) {
    // TODO: multiple resolutions
    if (auto [aType, method] = env.getMethod(objectType, methodName); method) {
      auto function = method->unboxFunction();
      auto params = Pool().tupleElements(function->type.parameters);
      if (params.size() - 1 != arguments.size()) {
        parser.crash(
          nodeIndex,
          "Unable to use method '{}.{}' as a(n) {} operator. "
          "{} operators must take {} arguments, but this method takes {}",
          TypeName(aType),
          methodName,
          methodName,
          methodName,
          methodName,
          arguments.size(),
          params.size()
        );
      }

      if (auto dereffedType = Pool().dereference(params[0])) {
        if (auto lValue = object.lValue()) {
          object = Reference(RegisterValue(
            lValue->name,
            Pool().pointerTo(dereffedType),
            lValue->scope
          ));
        } else {
          parser.crash(nodeIndex, "");
        }
      }
    }

    vector<TypeName> argTypes;
    argTypes.push_back(TypeName(objectType));
    for (auto arg : arguments) {
      argTypes.push_back(TypeName(check(arg).type));
    }
    // TODO: highlight symbol
    parser.crash(
      nodeIndex,
      "No method or built-in operator '{}' for type(s) {}",
      methodName,
      fmt::join(argTypes, ", ")
    );
  }

  ReturnType arithmeticOperation(
    NodeIndex a,
    NodeIndex b,
    string_view methodName
  ) {}

  ReturnType add(NodeIndex a, NodeIndex b) {}

  ReturnType subtract(NodeIndex a, NodeIndex b) {}

  ReturnType multiply(NodeIndex a, NodeIndex b) {}

  ReturnType divide(NodeIndex a, NodeIndex b) {}

  ReturnType leftDivide(NodeIndex a, NodeIndex b) {}

  ReturnType remainder(NodeIndex a, NodeIndex b) {}

  ReturnType logicOp(NodeIndex a, NodeIndex b) {}

  ReturnType logicAnd(NodeIndex a, NodeIndex b) {}

  ReturnType logicOr(NodeIndex a, NodeIndex b) {}

  ReturnType bitwiseOp(NodeIndex a, NodeIndex b) {}

  ReturnType bitwiseAnd(NodeIndex a, NodeIndex b) {}

  ReturnType bitwiseOr(NodeIndex a, NodeIndex b) {}

  ReturnType xorOp(NodeIndex a, NodeIndex b) {}

  ReturnType shiftOp(NodeIndex a, NodeIndex b) {}

  ReturnType shiftLeft(NodeIndex a, NodeIndex b) {}

  ReturnType shiftRight(NodeIndex a, NodeIndex b) {}

  ReturnType whileLoop(NodeIndex condition, NodeIndex body) {}

  ReturnType sizedArray(NodeIndex length, NodeIndex type) {}

  void checkArrayIndex(NodeIndex index) {}

  ReturnType index(NodeIndex object, NodeIndex index) {}

  ReturnType call(NodeIndex function, Encodings::ArgumentList& args) {}

  ReturnType exclusiveRange(NodeIndex nodeIndex) {}

  ReturnType align(NodeIndex nodeIndex) {}

  ReturnType impl(NodeIndex nodeIndex) {}

  ReturnType functionLiteral(NodeIndex nodeIndex) {}

  ReturnType numCast(Encodings::UnaryOp& node) {}

  ReturnType bitCast(Encodings::UnaryOp& node) {}

  ReturnType cImport(Encodings::UnaryOp& node) {}

  ReturnType cDefine(Encodings::UnaryOp& node) {}

  ReturnType cInclude(Encodings::UnaryOp& node) {}

  ReturnType cIncludeDir(Encodings::UnaryOp& node) {}

  ReturnType link(Encodings::UnaryOp& node) {}

  ReturnType linkDir(Encodings::UnaryOp& node) {}

  ReturnType type(Encodings::UnaryOp& node) {}

  ReturnType import(Encodings::UnaryOp& node) {}

  ReturnType dereference(Encodings::UnaryOp& node) {}

  ReturnType reference(Encodings::UnaryOp& node) {}

  ReturnType unaryNot(Encodings::UnaryOp& node) {}

  ReturnType sliceType(Encodings::UnaryOp& node) {}

  // [^]a
  ReturnType multiPointerTo(Encodings::UnaryOp& node) {}

  // a[^]
  ReturnType multiPointerFrom(Encodings::UnaryOp& node) {}

  ReturnType unaryMinus(Encodings::UnaryOp& node) {}

  ReturnType bitwiseNot(Encodings::UnaryOp& node) {}

  ReturnType makeSlice(Encodings::UnaryOp& node) {}

  ReturnType returnExpr(Encodings::UnaryOp& node) {}

  ReturnType usingExpr(Encodings::UnaryOp& node) {}

  ReturnType cudaImport(Encodings::UnaryOp& node) {}

  ReturnType ifExpr(Encodings::If& node) {}

  ReturnType structExpr(NodeIndex nodeIndex) {}

  ReturnType dotAccess(NodeIndex nodeIndex) {}

  ReturnType argList(NodeIndex nodeIndex) {}

  ReturnType enumExpr(Encodings::Enum& node) {}

  ReturnType multiLineString(NodeIndex nodeIndex) {}

  ReturnType forLoop(NodeIndex nodeIndex) {}

  ReturnType apply(NodeIndex functionNode, NodeIndex argNode) {}

  Reference toRegister(Reference* value) {
    if (value->isLiteral()) {
      return *value;
    } else if (auto lValue = value->lValue()) {
      auto type = value->getType();
      auto registerIndex = environment.makeTemporary(type);
      auto registerValue = Reference(registerIndex);
      emitLine("{} = load {}, ptr {}", registerValue, LlvmName(type), *value);
      return Reference(registerIndex);
    } else if (auto recursive = std::get_if<Reference*>(&value->value)) {
      return toRegister(*recursive);
    } else {
      return *value;
    }
  }
};

static_assert(AstVisitor<Compiler>, "Compiler must implement AstVisitor");
