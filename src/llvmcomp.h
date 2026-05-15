#include "abi.h"
#include "ast.h"
#include "cimport.h"
#include "common.h"
#include "compilercontext.h"
#include "fmt/ostream.h"
#include "parser.h"
#include "tokenizer.h"
#include "typechecker.h"
#include "types.h"
#include "value.h"
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace fs = std::filesystem;

struct FunctionStub {
  RegisterName name;
  NodeIndex definitionNode;
  FunctionType functionType;
  TypeIndex selfType;
};

struct SwitchCase {
  stringstream instructions;
  Reference condition;
  Reference result;
  u32 entryBlock;
  u32 exitLabel;
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
  NodeIndex nodeIndex;       \
  string_view name;

// Individual statement
struct Compiler {

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
  // TODO: should this move to std::stringstream w/ rdbuf?
  std::queue<std::string> globalsStack;
  static Logger log;
  TargetType targetType;
  std::vector<FunctionStub> functionStubs;

  using ReturnType = Reference;
  Environment environment;

  TypeChecker typeChecker;

  Parser& parser;

  struct TypeValue {
    TypeIndex type;
    bool lValue = false;
  };

  void setVisitedNode(NodeIndex index) {
    nodeIndex = index;
  }

  ReturnType compile(
    NodeIndex index,
    std::ostream& outFile,
    TypeIndex targetType = Pool().infer
  ) {
    log("Compiling node: {}", index.value);
    auto oldStack = stackItems;
    stackItems =
      {.outputFile = &outFile, .expectedType = targetType, .nodeIndex = index};
    auto result = astVisit(index, parser, *this);
    stackItems = oldStack;
    return result;
  }

  ReturnType compile(NodeIndex index, TypeIndex targetType = Pool().infer) {
    return compile(index, *outputFile, targetType);
  }

  template <typename... Args>
    requires LlvmNamesOnly<Args...>
  void emitLine(fmt::format_string<Args...> fmt, Args&&... args) {
    fmt::println(*outputFile, fmt, std::forward<Args>(args)...);
  }

  void emitLine(std::stringstream& stream) {
    *outputFile << stream.rdbuf();
  }

  template <typename... Args>
    requires LlvmNamesOnly<Args...>
  void emit(fmt::format_string<Args...> fmt, Args&&... args) {
    fmt::print(*outputFile, fmt, std::forward<Args>(args)...);
  }

  ReturnType block(Encodings::Block node) {
    auto scope = environment.pushScope();
    for (auto child : node.elements) {
      compile(child);
    }
    auto hasReturned = environment.hasReturned;
    if (hasReturned) {
      environment.scopes.back().hasReturned = true;
    }
    return Reference::Void();
  }

  ReturnType arrayLiteral(Encodings::Block node) {
    std::vector<Reference> elements;
    elements.reserve(node.elements.size());
    auto type = Pool().infer;

    auto resultType = typeChecker.check(nodeIndex).type;

    if (elements.empty()) {
      crash(nodeIndex, "Empty array literal");
    }

    for (auto element : node.elements) {
      elements.push_back(compile(element));
    }

    auto resultArray = Reference(ZeroInit{});
    u32 i = 0;
    for (auto& element : elements) {
      auto loaded = toRegister(element);
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

    auto loadedCondition = toRegister(compile(conditionIndex));
    auto caseType = loadedCondition.getType();

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
      environment.currentLabel = block;
      // Else/default block
      if (condition.value == body.value) {
        hasDefault = true;
        cases.push_back({
          .result = compile(body),
          .entryBlock = block,
          .exitLabel = environment.currentLabel,
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
        .exitLabel = environment.currentLabel,
        .returns = environment.hasReturned,
      });
      environment.hasReturned = false;
      SwitchCase& switchCase = cases.back();
      switchCase.instructions = std::move(instruction);

      // if (switchCase.condition.getType() != caseType) {
      //   crash(
      //     condition,
      //     "Expected type for case condition was {}, but was given {}",
      //     TypeName(caseType),
      //     TypeName(switchCase.condition.getType())
      //   );
      // }

      // TODO: non-comptime cases
      if (!switchCase.condition.isComptime()) {
        crash(
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
        emit("[{}, %{}]", caseBlock.result, caseBlock.exitLabel);
      }
      emitLine("");

      return lValue ? Reference(StackValue(resultRegister, resultType))
                    : Reference(RegisterValue(resultRegister, resultType));
    }

    return Reference::Void();
  }

  ReturnType declaration(Encodings::Declaration node) {
    // NOTE: don't support using non-identifiers

    auto definitionNode = parser.getDefinition(node.definition);
    auto definitionName = definitionNode.name->lexeme;

    StackItems frame = stackItems;
    frame.name = definitionName;
    auto guard = push(frame);
    log("Using name: {}", name);

    if (environment.isDefined(definitionName)) {
      crash(nodeIndex, "Attempt to redefine name '{}'", definitionName);
    }

    auto assignmentToken = parser.getToken(nodeIndex)->type;
    // Comptime
    if (assignmentToken == TokenType::Colon) {
      if (definitionNode.type) {
        TODO("Explicitly-typed comptime constants");
      }
      auto value = compile(node.value);

      if (!value.isComptime()) {
        crash(
          nodeIndex,
          "Unable to make comptime constant out of "
          "non-comptime value assigned to '{}'",
          definitionName
        );
      }

      environment.define(definitionName, value);
      return Reference::Void();
    } else if (assignmentToken == TokenType::Assign) {
      // TODO: pointer stability might be sussy; consider index
      auto assignee = makeDefinition(definitionNode.name, definitionNode.type);

      TypeIndex assignedType = assignee->type;
      assignedType = Pool().isAssignable(
        typeChecker.check(node.value, assignedType).type,
        assignedType
      );
      if (!assignedType) {
        crash(
          nodeIndex,
          "Unable to assign value of type '{}' to to variable '{}' of type "
          "'{}'",
          TypeName(typeChecker.check(node.value).type),
          assignee->name,
          TypeName(assignee->type)
        );
      }
      assignee->type = assignedType;
      emitDefinition(*assignee);
      log("Making value: '{}: {}'", assignee->name, TypeName(assignee->type));
      typeChecker.pushType(node.value, assignedType);
      auto value = toRegister(compile(node.value));

      emitLine("store {} {}, ptr {}", LlvmName(assignedType), value, *assignee);
    } else {
      parser.crash(
        nodeIndex,
        "Internal parser error: token for declaration must be '=' or ':'"
      );
    }

    // TODO: support assignment as expression???
    return Reference::Void();
  }

  void emitDefinition(StackValue assignee) {
    auto byteAlignment =
      Pool().getSizing(assignee.type).alignment.byteAlignment();

    switch (assignee.scope) {
    case ValueScope::Local: {
      emitLine(
        "{} = alloca {}, align {}",
        assignee,
        LlvmName(assignee.type),
        byteAlignment
      );
      break;
    }
    case ValueScope::Global: {
      globalsStack.push(
        fmt::format(
          "{} = global {} undef align {}",
          assignee,
          LlvmName(assignee.type),
          byteAlignment
        )
      );
      break;
    }
    }
  }

  StackValue* makeDefinition(TokenPointer name, NodeIndex typeNode) {
    TypeIndex type = Pool().infer;
    if (typeNode) {
      if (auto typeIndex = compile(typeNode).unboxType()) {
        type = typeIndex;
      } else {
        crash(typeNode, "Type for identifier '{}' is not a type", name->lexeme);
      }
    }
    Reference* definition = environment.define(
      name->lexeme,
      Reference(StackValue(
        environment.nextTemporary,
        type,
        environment.envType() == EnvType::Global ? ValueScope::Global
                                                 : ValueScope::Local
      ))
    );
    if (definition) {
      return definition->lValue();
    }

    crash(name, "Definition for identifier '{}' already exists", name->lexeme);
  }

  ReturnType definition(Encodings::Definition node) {
    assert(node.type);
    StackItems frame = stackItems;
    frame.name = node.name->lexeme;
    auto guard = push(frame);
    log("Using name: {}", name);
    StackValue* def = makeDefinition(node.name, node.type);
    emitDefinition(*def);
    doAssignment(Reference(ZeroInit{}), Reference(*def));
    return Reference::Void();
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
            crash(
              token,
              "Invalid hex escape '\\x{}{}': {} is not a hexidecimal digit "
              "0-9,A-F",
              hex1,
              hex2,
              hex1
            );
          }
          if (!Tokenizer::isHex(hex2)) {
            crash(
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
          crash(token, "Unknown escape sequence \\{}", c);
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
    auto lengthLoaded = toRegister(length);
    auto dataType = dataPointer.getType();
    auto type = Pool().sliceOf(dataType);
    auto intermediateResult = environment.makeTemporary(type);
    emitLine(
      "{} = insertvalue {} undef, ptr {}, 0",
      intermediateResult,
      LlvmName(type),
      dataPointer
    );
    auto result = environment.makeTemporary(Pool().sliceOf(dataType));
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
    crash(
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
    crash(nodeIndex, "Identifier \"{}\" not defined", name);
  }

  ReturnType opaque(TokenPointer token) {
    // TODO: naming opaque types
    TypeIndex type = Pool().addOpaque(std::string("Anonymous Opaque"));
    return Reference(type);
  }

  ReturnType self(TokenPointer token) {
    if (auto type = environment.selfType()) {
      return Reference(type);
    }
    crash(nodeIndex, "No type 'Self' in context");
  }

  ReturnType undefined(TokenPointer token) {
    return Reference(Never{});
  }

  ReturnType cudaBuiltin(TokenPointer token, string_view callName) {
    if (targetType != TargetType::Gpu) {
      crash(
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
      crash(nodeIndex, "Unable to assign to non l-value");
    }
    auto loadedValue = toRegister(value);
    emitLine(
      "store {} {}, ptr {}",
      LlvmName(assignee.getType()),
      loadedValue,
      assignee
    );
  }

  ReturnType assign(NodeIndex left, NodeIndex right) {
    auto targetType = typeChecker.check(left);
    if (!targetType.lValue) {
      crash(left, "Unable to assign to non l-value");
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
      crash(nodeIndex, "Unknown comparison operation");
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

      aVal = toRegister(aVal);
      bVal = toRegister(bVal);

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
        LlvmName(type),
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

  static FieldMap defaultFields;

  Reference callFunction(
    Function* func,
    span<NodeIndex> positionalArguments,
    Encodings::NamedValues namedArguments,
    const FieldMap& fields = defaultFields,
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

    auto args = getArguments(
      parameterTypes,
      positionalArguments,
      namedArguments,
      fields,
      selfArg
    );

    // TODO: optional arguments
    for (auto _ : namedArguments) {
      TODO("Named arguments for function calls");
    }

    if (args.positional.size() != parameterTypes.size()) {
      crash(
        nodeIndex,
        "Passed {} arguments, but expected {}",
        args.positional.size(),
        parameterTypes.size()
      );
    }

    OutContext callCtx{.outputFile = *outputFile, .environment = environment};

    // TODO: ZST
    u32 resultRegister =
      callAbiFunctionWithArgs(callCtx, *func, args.positional);
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
    if (
      auto [aType, method] = environment.getMethod(objectType, methodName);
      method
    ) {
      auto function = method->unboxFunction();
      auto params = Pool().tupleElements(function->type.parameters);
      if (params.size() - 1 != arguments.size()) {
        crash(
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
          crash(nodeIndex, "");
        }
      }
    }

    vector<TypeName> argTypes;
    argTypes.push_back(TypeName(objectType));
    for (auto arg : arguments) {
      argTypes.push_back(TypeName(typeChecker.check(arg).type));
    }
    // TODO: highlight symbol
    crash(
      nodeIndex,
      "No method or built-in operator '{}' for type(s) {}",
      methodName,
      fmt::join(argTypes, ", ")
    );
  }

  template <typename T> T literalArithmetic(T a, T b, TokenType opType) {
    T comparison;
    switch (opType) {
    case TokenType::Plus: {
      comparison = a + b;
      break;
    }
    case TokenType::Minus: {
      comparison = a - b;
      break;
    }
    case TokenType::Mult: {
      comparison = a * b;
      break;
    }
    case TokenType::Div: {
      comparison = a / b;
      break;
    }
    case TokenType::LeftDiv: {
      comparison = b / a;
      break;
    }
    case TokenType::Remainder: {
      comparison = a % b;
      break;
    }
    default: {
      crash(nodeIndex, "Unknown arithmetic operation");
    }
    }
    return comparison;
  }

  struct ArithmeticOperator {
    TokenType type;
    string_view instructionName;
    string_view methodName;
    struct {
      bool signedUnsignedDistinction = false;
      bool swapArgs = false;
    };
  };

  ReturnType arithmeticOperation(
    NodeIndex a,
    NodeIndex b,
    ArithmeticOperator op
  ) {
    typeChecker.check(nodeIndex);
    auto aVal = compile(a);
    auto bVal = compile(b);
    if (auto type = Pool().coerce(aVal.getType(), bVal.getType())) {
      if (type == Pool().intLiteral) {
        auto a = aVal.unbox<IntLiteral>()->value;
        auto b = bVal.unbox<IntLiteral>()->value;
        return Reference(literalComparison(a, b, op.type));
      } else if (type == Pool().floatLiteral) {
        auto a = aVal.unbox<FloatLiteral>()->value;
        auto b = bVal.unbox<FloatLiteral>()->value;
        return Reference(literalComparison(a, b, op.type));
      }

      aVal = toRegister(aVal);
      bVal = toRegister(bVal);

      string_view typePrefix;
      if (Pool().isFloat(type)) {
        typePrefix = "f";
      } else if (Pool().isSignedInt(type)) {
        typePrefix = op.signedUnsignedDistinction ? "s" : "";
      } else if (Pool().isUnsignedInt(type)) {
        typePrefix = op.signedUnsignedDistinction ? "u" : "";
      } else {
        TODO("Non-primitive comparison operations");
      }

      if (op.swapArgs) {
        std::swap(aVal, bVal);
      }

      RegisterValue result = environment.makeTemporary(type);
      emitLine(
        "{} = {}{} {} {}, {}",
        result,
        typePrefix,
        op.instructionName,
        LlvmName(type),
        aVal,
        bVal
      );
      return Reference(result);
    } else {
      // TODO: multiple resolutions
      if (
        auto [aType, method] =
          environment.getMethod(typeChecker.check(a).type, op.methodName);
        method
      ) {
        auto function = method->unboxFunction();
        auto params = Pool().tupleElements(function->type.parameters);
        // TODO: can probably avoid these checks because they should be caught
        // in typechecker
        if (params.size() != 2) {
          crash(
            a,
            "Unable to use method '{}.{}' as a(n) {} operator. "
            "{} operators must take 2 arguments, but this takes {}",
            TypeName(aType),
            op.methodName,
            op.methodName,
            op.methodName,
            op.methodName,
            params.size()
          );
        }
        auto bType = params[1];
        typeChecker.pushType(b, bType);
        span<NodeIndex> args(&b, 1);
        Reference aVal = compile(a, aType);
        return callFunction(function, args, {}, defaultFields, &aVal);
      } else {
        crash(
          a,
          "TODO: compiler error when unable to find method for operators"
        );
      }
    }
  }

  ReturnType add(NodeIndex a, NodeIndex b) {
    static ArithmeticOperator op{
      .type = TokenType::Plus,
      .instructionName = "add",
      .methodName = "add"
    };
    return arithmeticOperation(a, b, op);
  }

  ReturnType subtract(NodeIndex a, NodeIndex b) {
    static ArithmeticOperator op{TokenType::Minus, "sub", "subtract"};
    return arithmeticOperation(a, b, op);
  }

  static ArithmeticOperator multOp;
  ReturnType multiply(NodeIndex a, NodeIndex b) {
    return arithmeticOperation(a, b, multOp);
  }

  ReturnType divide(NodeIndex a, NodeIndex b) {
    static ArithmeticOperator op{TokenType::Div, "div", "divide", true};
    return arithmeticOperation(a, b, op);
  }

  ReturnType leftDivide(NodeIndex a, NodeIndex b) {
    static ArithmeticOperator
      op{TokenType::LeftDiv, "div", "divide", true, true};
    return arithmeticOperation(a, b, op);
  }

  ReturnType remainder(NodeIndex a, NodeIndex b) {
    static ArithmeticOperator
      op{TokenType::Remainder, "rem", "remainder", true};
    return arithmeticOperation(a, b, op);
  }

  ReturnType logicAnd(NodeIndex a, NodeIndex b) {
    auto startLabel = environment.currentLabel;
    auto boolType = Pool()._bool;
    typeChecker.check(nodeIndex, boolType);
    auto leftVal = toRegister(compile(a, boolType));
    if (auto literalValue = leftVal.unbox<bool>()) {
      if (*literalValue) {
        return compile(b, boolType);
      }
      return Reference(false);
    }

    auto trueLabel = environment.addTemporary();
    std::stringstream rightInstruction;
    environment.currentLabel = trueLabel;
    auto rightVal = compile(b, rightInstruction, boolType);
    auto falseLabel = environment.addTemporary();

    emitLine(
      "br i1 {}, label %{}, label %{}\n{}:",
      leftVal,
      trueLabel,
      falseLabel,
      trueLabel
    );

    emitLine(rightInstruction);

    if (auto type = rightVal.getType(); type != Pool()._bool) {
      crash(
        b,
        "Operands to logical 'and' need to be of type bool. Right operand "
        "was of type '{}'",
        TypeName(type)
      );
    }

    emitLine("br label %{}\n{}:", trueLabel, trueLabel);
    auto result = environment.makeTemporary(Pool()._bool);
    emitLine(
      "{} = phi i1 [false, %{}], [{}, %{}]",
      result,
      startLabel,
      rightVal,
      trueLabel
    );
    environment.currentLabel = falseLabel;
    return Reference(result);
  }

  ReturnType logicOr(NodeIndex a, NodeIndex b) {
    auto startLabel = environment.currentLabel;
    auto boolType = Pool()._bool;
    typeChecker.check(nodeIndex, boolType);
    auto leftVal = compile(a, boolType);
    if (auto literalValue = leftVal.unbox<bool>()) {
      if (*literalValue) {
        return Reference(true);
      }
      return compile(b, boolType);
    }
    leftVal = toRegister(leftVal);

    auto falseLabel = environment.addTemporary();
    std::stringstream rightInstruction;
    environment.currentLabel = falseLabel;
    auto rightVal = compile(b, rightInstruction, boolType);
    auto trueLabel = environment.addTemporary();

    emitLine(
      "br i1 {}, label %{}, label %{}\n{}:",
      leftVal,
      trueLabel,
      falseLabel,
      falseLabel
    );

    emitLine(rightInstruction);

    if (auto type = rightVal.getType(); type != Pool()._bool) {
      crash(
        b,
        "Operands to logical 'or' need to be of type bool. Right operand "
        "was of type '{}'",
        TypeName(type)
      );
    }

    emitLine("br label %{}\n{}:", trueLabel, trueLabel);
    auto result = environment.makeTemporary(Pool()._bool);
    emitLine(
      "{} = phi i1 [true, %{}], [{}, %{}]",
      result,
      startLabel,
      rightVal,
      falseLabel
    );
    environment.currentLabel = trueLabel;
    return Reference(result);
  }

  struct BitwiseOp {
    TokenType type;
    string_view instruction;
  };

  ReturnType bitwiseOp(NodeIndex a, NodeIndex b, BitwiseOp op) {
    auto type = typeChecker.check(nodeIndex, expectedType).type;
    auto aVal = compile(a, type);
    auto bVal = compile(b, type);

    auto aLit = aVal.unbox<IntLiteral>();
    auto bLit = bVal.unbox<IntLiteral>();
    if (aLit && bLit) {
      switch (op.type) {
      case TokenType::BitAnd: {
        return Reference(IntLiteral(aLit->value & bLit->value));
      }
      case TokenType::BitOr: {
        return Reference(IntLiteral(aLit->value | bLit->value));
      }
      case TokenType::Xor: {
        return Reference(IntLiteral(aLit->value ^ bLit->value));
      }
      default: {
        crash(nodeIndex, "Unknown bitwise op");
      }
      }
    }
    aVal = toRegister(aVal);
    bVal = toRegister(bVal);
    auto result = environment.makeTemporary(type);
    emitLine(
      "{} = {} {} {}, {}",
      result,
      op.instruction,
      LlvmName(type),
      aVal,
      bVal
    );
    return Reference(result);
  }

  ReturnType bitwiseAnd(NodeIndex a, NodeIndex b) {
    return bitwiseOp(a, b, {TokenType::BitAnd, "and"});
  }

  ReturnType bitwiseOr(NodeIndex a, NodeIndex b) {
    return bitwiseOp(a, b, {TokenType::BitOr, "or"});
  }

  ReturnType xorOp(NodeIndex a, NodeIndex b) {
    return bitwiseOp(a, b, {TokenType::Xor, "xor"});
  }

  using ShiftOp = BitwiseOp;
  // TODO: log base-2 sized shift operators
  ReturnType shiftOp(NodeIndex a, NodeIndex b, ShiftOp op) {
    return bitwiseOp(a, b, op);
  }

  ReturnType shiftLeft(NodeIndex a, NodeIndex b) {
    return bitwiseOp(a, b, {TokenType::ShiftLeft, "shl"});
  }

  // TODO: arithemetic vs logical shift
  ReturnType shiftRight(NodeIndex a, NodeIndex b) {
    return bitwiseOp(a, b, {TokenType::ShiftRight, "shr"});
  }

  ReturnType whileLoop(NodeIndex condition, NodeIndex body) {
    typeChecker.check(nodeIndex);
    auto loopHeader = environment.addTemporary();
    emitLine("br label %{}\n{}:", loopHeader, loopHeader);
    environment.currentLabel = loopHeader;
    auto conditionLiteral = compile(condition, Pool()._bool);
    conditionLiteral = toRegister(conditionLiteral);
    auto loopBody = environment.addTemporary();
    environment.currentLabel = loopBody;

    std::stringstream bodyInstruction;
    compile(body, bodyInstruction, Pool().infer);

    auto endLabel = environment.addTemporary();
    emitLine(
      "br i1 {}, label %{}, label %{}\n{}:",
      conditionLiteral,
      loopBody,
      endLabel,
      loopBody
    );
    emitLine(bodyInstruction);
    emitLine("br label %{}\n{}:", loopHeader, endLabel);
    // TODO: consider value expression (see
    // https://ziglang.org/documentation/master/#while)
    return Reference::Void();
  }

  ReturnType sizedArray(NodeIndex length, NodeIndex type) {
    typeChecker.check(length, Pool().intLiteral);
    typeChecker.check(type, Pool().type);

    auto lengthVal = compile(length);
    auto elementType = compile(type);
    if (auto arrayLength = lengthVal.unbox<IntLiteral>()) {
      if (auto typeIndex = elementType.unboxType()) {
        return Reference(Pool().sizedArrayOf(typeIndex, arrayLength->value));
      }
      crash(
        nodeIndex,
        "Element type of sized array type must be a compile-time known type"
      );
    }
    crash(
      nodeIndex,
      "Length of sized array type must be a compile-time known integer"
    );
  }

  void checkArrayIndex(NodeIndex index) {}

  pair<RegisterValue, RegisterValue> getSliceElements(RegisterValue slice) {
    auto elementType = Pool().sliceElementType(slice.type);
    assert(elementType);
    auto dataPointer = environment.makeTemporary(elementType);
    auto length = environment.makeTemporary(Pool()._usize);
    emitLine(
      "{} = extractvalue {} {}, 0",
      dataPointer,
      LlvmName(slice.type),
      slice
    );
    emitLine("{} = extractvalue {} {}, 1", length, LlvmName(slice.type), slice);
    return {dataPointer, length};
  }

  ReturnType index(NodeIndex object, NodeIndex index) {
    auto list = compile(object);
    auto listType = list.getType();
    // TODO: Generic instantiation
    // if (auto boxedGeneric = std::get_if<GenericValue>(&list.value)) {
    //   auto arguments = parser.getArgumentList(index);
    //   auto expectedArgLength = boxedGeneric->parameterNames.size();
    //   {
    //     auto actualArgLength = arguments.requiredArgs.size();
    //     if (expectedArgLength != actualArgLength)
    //       crash(
    //         node.right,
    //         "Expected {} generic arguments, but {} were provided",
    //         expectedArgLength,
    //         actualArgLength
    //       );
    //   }
    //   Environment genericEnvironment(
    //     &boxedGeneric->definitionEnvironment,
    //     std::string(""),
    //     true
    //   );
    //   std::vector<TypeIndex> argTypes(expectedArgLength);

    //   // TODO: optional inputs
    //   for (auto i = 0; i < boxedGeneric->parameterNames.size(); i++) {
    //     auto argNode = arguments.requiredArgs[i];
    //     auto argValue = interpret(argNode, environment, outputFile, context);

    //     auto paramName = boxedGeneric->parameterNames[i];
    //     if (auto typeIndex = argValue.unboxType()) {
    //       genericEnvironment.define(paramName, argValue);
    //       argTypes.push_back(*typeIndex);
    //     } else {
    //       TODO("Non-type generic parameters");
    //     }
    //   }

    //   for (auto param : arguments.optionalArgs) {
    //     Todo(parser.getToken(param.token), "Named generic parameters");
    //   }

    //   auto [tupleType, tupleIndex] = TypePool().tupleOf(std::move(argTypes));

    //   if (boxedGeneric->cache.contains(tupleIndex)) {
    //     return Reference(boxedGeneric->cache[tupleIndex]);
    //   } else {
    //     auto tupleName = fmt::format("{}", TypeName(tupleType));
    //     std::string genericName =
    //       fmt::format("{}{}", boxedGeneric->name, tupleName);
    //     genericName[genericEnvironment.prefix.size() - 1] = ']';
    //     genericName[genericEnvironment.prefix.size() - tupleName.size()] =
    //     '[';

    //     StatementContext genericContext = {
    //       .name = genericName,
    //       .expectedType = context.expectedType
    //     };
    //     // Match source syntax when debugging if possible
    //     genericEnvironment.prefix =
    //       boxedGeneric->definitionEnvironment.prefix + genericName;
    //     boxedGeneric->translationUnit.interpret(
    //       boxedGeneric->astNode,
    //       genericEnvironment,
    //       outputFile,
    //       genericContext
    //     );
    //     auto genericValue = interpret(
    //       boxedGeneric->astNode,
    //       genericEnvironment,
    //       outputFile,
    //       genericContext
    //     );
    //     auto cached = new Reference(genericValue);
    //     boxedGeneric->cache[tupleIndex] = cached;
    //     return Reference(cached);
    //   }
    // }

    // Bracket access
    auto indexVal = compile(index);
    auto indexType = indexVal.getType();

    // TODO: pointers to slice?
    if (auto dereffedType = Pool().unboxReference(listType)) {
      auto ptr = toRegister(list);
      auto loaded =
        StackValue(std::get<RegisterValue>(ptr.value).name, *dereffedType);
      list.value = loaded;
      listType = *dereffedType;
    }

    if (auto sliceElement = Pool().sliceElementType(listType)) {
      TypeIndex elementType = sliceElement;
      auto leftLiteral = toRegister(list);
      auto [dataPointer, length] =
        getSliceElements(std::get<RegisterValue>(leftLiteral.value));
      auto lengthBound = RangeBound(length);
      auto dataPointerRef = Reference(dataPointer);

      if (Pool().isInt(indexType)) {
        auto index = toRegister(indexVal);
        auto result = StackValue(environment.addTemporary(), elementType);

        if (Pool().isSignedInt(indexType)) {
          guardLowerBound(index);
        }
        guardIndexInBounds(index, lengthBound);
        emitLine(
          "{} = getelementptr {}, ptr {}, {} {}",
          result,
          LlvmName(elementType),
          leftLiteral,
          LlvmName(indexType),
          index
        );
        return Reference(result);
      } else if (auto range = indexVal.unbox<Range>()) {
        return sliceRange(*range, index, length, dataPointerRef);
      }
      crash(
        index,
        "Index must be an integer or range, but was of type '{}'",
        TypeName(indexType)
      );
    } else if (auto elementType = Pool().multiPointerElement(listType)) {
      auto dataPointer = toRegister(list);
      if (Pool().isInt(indexType)) {
        auto rightLiteral = toRegister(indexVal);
        auto result =
          Reference(StackValue(environment.addTemporary(), elementType));
        emitLine(
          "{} = getelementptr ptr, ptr {}, {} {}",
          result,
          dataPointer,
          LlvmName(indexType),
          rightLiteral
        );
        return result;
      } else if (auto range = indexVal.unbox<Range>()) {
        return sliceRange(*range, index, std::nullopt, dataPointer);
      }
      crash(
        index,
        "Index must be an integer or range, but was of type '{}'",
        TypeName(indexType)
      );
    } else if (auto sizedArray = Pool().sizedArray(listType)) {
      auto length = IntLiteral(sizedArray->length);
      auto elementType = sizedArray->dereferencedType;
      if (!list.lValue()) {
        TODO("Indexing sized array not in stack value");
      }

      if (Pool().isInt(indexType)) {
        auto index = toRegister(indexVal);
        if (Pool().isSignedInt(indexType)) {
          guardLowerBound(index);
        }
        auto lengthRef = RangeBound(length);
        guardIndexInBounds(index, lengthRef);
        auto result = StackValue(environment.addTemporary(), elementType);
        emitLine(
          "{} = getelementptr {}, ptr {}, {} {}",
          result,
          LlvmName(elementType),
          list,
          LlvmName(indexType),
          index
        );
        return Reference(result);
      } else if (auto range = indexVal.unbox<Range>()) {
        Reference dataPointer;
        if (auto stackVal = indexVal.unbox<StackValue>()) {
          dataPointer.value = StackValue(stackVal->name, elementType);
        } else {
          crash(nodeIndex, "Compiler state failure when slicing array");
        }
        return sliceRange(*range, index, length, dataPointer);
      }
      crash(
        index,
        "Index must be an integer or range, but was of type '{}'",
        TypeName(indexType)
      );
    } else {
      crash(nodeIndex, "Unable to index object of type {}", TypeName(listType));
    }
  }

  void guardLowerBound(Reference& index) {
    auto isNegative = environment.makeTemporary(Pool()._bool);
    auto crashBlockId = environment.addTemporary();
    auto continueBlockId = environment.addTemporary();
    emitLine(
      "{} = icmp slt {} {}, 0",
      isNegative,
      LlvmName(index.getType()),
      index
    );
    emitLine(
      "br i1 {}, label %{}, label %{}",
      isNegative,
      crashBlockId,
      continueBlockId
    );
    emitLine("{}:", crashBlockId);
    crashInstruction();
    emitLine("{}:", continueBlockId);
  }

  void crashInstruction() {
    emitLine("call void @llvm.trap()\nunreachable");
  }

  Reference sliceRange(
    Range& range,
    NodeIndex rangeNode,
    std::optional<RangeBound> baseLength,
    Reference& dataPointer
  ) {
    if (!(range.hasUpper() || baseLength)) {
      crash(
        rangeNode,
        "Ranges for slicing multipointers must have an upper bound"
      );
    }
    auto usize = Pool()._usize;
    auto lower = Reference::unboxBound(range.lower);
    auto upperBound =
      range.hasUpper() ? range.upper.value() : baseLength.value();
    auto upper = Reference::unboxBound(upperBound);

    auto type = range.getType();
    if (Pool().isSignedInt(type)) {
      TODO("Error for signed slices");
    }

    extendToUsize(lower);
    extendToUsize(upper);

    if (baseLength && range.hasUpper()) {
      guardExclusiveInBounds(upper, baseLength.value());
    }

    auto lengthRef = Reference(guardNonnegativeLength(lower, upperBound));
    auto newStartPoint = environment.makeTemporary(dataPointer.getType());
    emitLine(
      "{} = getelementptr {}, ptr {}, {} {}",
      newStartPoint,
      LlvmName(newStartPoint.type),
      dataPointer,
      LlvmName(usize),
      lower
    );
    auto startPointer = Reference(newStartPoint);
    return makeSlice(startPointer, lengthRef);
  }
  Reference makeSlice(Reference& dataPointer, Reference& length) {
    auto lengthLoaded = toRegister(length);
    auto dataType = dataPointer.getType();
    auto type = Pool().sliceOf(dataType);
    auto intermediateResult = environment.makeTemporary(type);
    emitLine(
      "{} = insertvalue {} undef, ptr {}, 0",
      intermediateResult,
      LlvmName(type),
      dataPointer
    );
    auto result = environment.makeTemporary(Pool().sliceOf(dataType));
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

  void extendToUsize(Reference& index) {
    auto usize = Pool()._usize;
    if (
      auto type = index.getType(); Pool().isUnsignedInt(type) && type != usize
    ) {
      auto extended = environment.makeTemporary(Pool()._usize);
      emitLine(
        "{} = zext {} {} to {}",
        extended,
        LlvmName(type),
        index,
        LlvmName(usize)
      );
      index.value = extended;
    }
  }

  struct Arguments {
    std::vector<Reference> positional;
    std::vector<pair<Reference, u32>> named;
  };

  // Returned args are all loaded into registers
  template <TypeRange Types>
  Arguments getArguments(
    Types parameterTypes,
    span<NodeIndex> positionalArguments,
    Encodings::NamedValues namedArguments,
    const FieldMap& fields,
    Reference* selfArg = nullptr
  ) {
    Arguments result;
    auto& positionalArgs = result.positional;
    positionalArgs.reserve(positionalArguments.size() + (selfArg != nullptr));
    if (selfArg) {
      if (Pool().dereference(parameterTypes[0])) {
        if (auto lValue = selfArg->lValue()) {
          positionalArgs.push_back(Reference(RegisterValue(lValue->name)));
        } else {
          crash(
            nodeIndex,
            "Unable to treat temporary value as reference for first argument "
            "in method call. Expected type '{}'",
            TypeName(parameterTypes[0])
          );
        }
      } else {
        positionalArgs.push_back(*selfArg);
      }
    }
    u32 i = positionalArgs.size();
    for (auto argNode : positionalArguments) {
      auto paramType = parameterTypes[i];
      auto targetType = typeChecker.check(argNode, paramType).type;
      auto argument = toRegister(compile(argNode, paramType));
      targetType = argument.isAssignableTo(paramType);
      // TODO: remove?
      if (!targetType) {
        crash(
          argNode,
          fmt::runtime(
            "Unable to assign argument of type '{}' to parameter of type '{}' "
            "(ids {} and {})"
          ),
          TypeName(argument.getType()),
          TypeName(paramType),
          argument.getType().value,
          paramType.value
        );
      }
      positionalArgs.push_back(argument);
      i++;
    }

    auto& namedArgs = result.named;
    for (auto [name, value] : namedArguments) {
      auto nameToken = parser.getToken(name);
      auto field = fields.find(nameToken->lexeme);
      if (field == fields.end()) {
        crash(nameToken, "Unknown named argument '{}'", nameToken->lexeme);
      }

      u32 argIndex = std::distance(fields.begin(), field);

      if (argIndex < positionalArguments.size()) {
        auto original = parser.locationOf(positionalArguments[argIndex]);
        fmt::println(
          "Orginally assigned at: {}:{}",
          original.line,
          original.column
        );
        original.underline(std::cerr);
        crash(nameToken, "Duplicate named argument '{}'", nameToken->lexeme);
      }

      auto expectedType = typeChecker.check(value, field->second).type;
      auto argument = compile(value);
      auto targetType = argument.isAssignableTo(expectedType);
      if (!targetType) {
        crash(
          value,
          "Unable to assign argument of type '{}' to parameter of type "
          "'{}'",
          TypeName(argument.getType()),
          TypeName(field->second)
        );
      }
      Reference loadedArg = toRegister(argument);
      namedArgs.push_back({loadedArg, argIndex});
    }
    for (auto& arg : positionalArgs) {
      arg = toRegister(arg);
    }
    return result;
  }

  Reference constructStruct(
    TypeIndex type,
    ChildSpan positionalArguments,
    Encodings::NamedValues namedArguments = {}
  ) {
    // TODO: unions: named and unnamed
    auto structDefinition = Pool().getStruct(type);
    if (!structDefinition) {
      crash(nodeIndex, "Can't construct non-struct type {}", TypeName(type));
    }
    auto structLlvmName = LlvmName(type);

    auto fieldTypes = structDefinition->fieldTypes();
    auto args = getArguments(
      fieldTypes,
      positionalArguments,
      namedArguments,
      structDefinition->fields
    );
    std::vector<bool> setArguments(structDefinition->fields.size(), false);

    Reference structVal = Reference(ZeroInit{});
    // TODO: default values that aren't zero initialized; using 0 for now to
    // avoid initializing all fields in C structs
    for (u32 i = 0; i < args.positional.size(); i++) {
      auto fieldValue = args.positional[i];
      auto fieldTypeLlvmName = LlvmName(fieldTypes[i]);
      setArguments[i] = true;
      Reference prevStruct = structVal;
      structVal.value = environment.makeTemporary(type);
      Reference ref(structVal);
      emitLine(
        "{} = insertvalue {} {}, {} {}, {}",
        ref,
        structLlvmName,
        prevStruct,
        fieldTypeLlvmName,
        fieldValue,
        i
      );
    }

    u32 i = 0;
    for (auto [value, fieldIndex] : args.named) {
      if (setArguments[fieldIndex]) {
        // auto location = parser.locationOf(positionalArguments[i]);
        // fmt::println(std::cerr, "Field '{}' originally assigned here", name);
        // location.underline(std::cerr);
        auto token = parser.toPointer(namedArguments[i].token);

        crash(token, "Duplicate assignment for field '{}'", token->lexeme);
      }
      setArguments[fieldIndex] = true;
      Reference prevStruct = structVal;
      structVal.value = environment.makeTemporary(type);
      emitLine(
        "{} = insertvalue {} {}, {} {}, {}",
        structVal,
        structLlvmName,
        prevStruct,
        LlvmName(fieldTypes[fieldIndex]),
        value,
        fieldIndex
      );
      i++;
    }
    return Reference(structVal);
  }

  ReturnType call(NodeIndex functionNode, Encodings::ArgumentList args) {
    typeChecker.check(nodeIndex, expectedType);
    auto functionType = typeChecker.check(functionNode).type;
    if (Pool().functionType(functionType)) {
      auto function = compile(functionNode).unboxFunction();
      if (!function) {
        crash(nodeIndex, "Internal error; expected function");
      }
      return callFunction(function, args.positional, args.named);
    } else if (Pool().boundFunctionType(functionType)) {
      auto bound = compile(functionNode).unbox<BoundFunction>();
      if (!bound) {
        crash(nodeIndex, "Internal error; expected bound method");
      }
      auto ref = bound->getSelf();
      return callFunction(
        bound->method,
        args.positional,
        args.named,
        defaultFields,
        &ref
      );
    } else if (functionType == Pool().type) {
      auto type = compile(functionNode, Pool().type).unboxType();
      if (!type) {
        crash(nodeIndex, "Internal error; expected type");
      }
      if (auto structDefinition = Pool().getStruct(type)) {
        return constructStruct(type, args.positional, args.named);
      } else {
        crash(nodeIndex, "Can't construct non-struct type {}", TypeName(type));
      }
    } else if (args.named.empty() && args.positional.size() == 1) {
      return multiply(functionNode, args.positional[0]);
    } else {
      crash(
        nodeIndex,
        "Unable to call value of type '{}' as a function",
        TypeName(functionType)
      );
    }
  }

  ReturnType exclusiveRange(
    NodeIndex lowerBoundIndex,
    NodeIndex upperBoundIndex
  ) {
    if (!(lowerBoundIndex || upperBoundIndex)) {
      crash(
        nodeIndex,
        "At least one of upper and lower bound on a range must be set"
      );
    }
    RangeBound lowerBound = IntLiteral(0);
    TypeIndex lowerType = Pool().intLiteral;
    if (lowerBoundIndex) {
      auto result = compile(lowerBoundIndex);
      lowerType = result.getType();
      if (!Pool().isInt(lowerType)) {
        crash(
          lowerBoundIndex,
          "Provided range bounds must be of an integer type, but were {}",
          TypeName(lowerType)
        );
      }
      lowerBound = toRegister(result).rangeBound();
    }
    std::optional<RangeBound> upperBound = std::nullopt;
    if (upperBoundIndex) {
      auto result = compile(upperBoundIndex);
      auto upperType = result.getType();
      if (!Pool().isInt(upperType)) {
        crash(
          upperBoundIndex,
          "Provided range bounds must be of an integer type, but were {}",
          TypeName(upperType)
        );
      }
      auto rangeType = Pool().coerce(lowerType, upperType);
      if (!rangeType) {
        crash(
          nodeIndex,
          "Range bounds must be of the same type, but were {} and {}",
          TypeName(lowerType),
          TypeName(upperType)
        );
      }
      upperBound = toRegister(result).rangeBound();
    }
    return Reference(Range(lowerBound, upperBound));
  }

  ReturnType align(NodeIndex alignmentIndex, NodeIndex valueIndex) {
    auto boxedAlignment = compile(alignmentIndex, Pool().intLiteral);
    auto boxedType = compile(valueIndex);

    u64 byteAlign;
    if (auto intLit = boxedAlignment.unbox<IntLiteral>()) {
      byteAlign = intLit->value;
      if ((byteAlign & (byteAlign - 1)) != 0) {
        crash(
          alignmentIndex,
          "Alignment value must be a power-of-two, but was {}",
          byteAlign
        );
      }
    } else {
      crash(
        alignmentIndex,
        "Alignment argument needs to be a compile-time known integer"
      );
    }

    if (auto baseType = boxedType.unboxType()) {
      return Reference(
        Pool().alignType(baseType, Log2Alignment::fromByteSize(byteAlign))
      );
    } else {
      crash(
        valueIndex,
        "Type argument for @align must be a compile-time known type"
      );
    }
  }

  ReturnType impl(NodeIndex typeNode, NodeIndex blockIndex) {
    TypeIndex targetType;
    auto boxedType = compile(typeNode, Pool().type);
    if (auto type = boxedType.unboxType()) {
      targetType = type;
    } else {
      crash(
        nodeIndex,
        "'impl' block must operate on comptime-known type, but was given "
        "'{}' value",
        TypeName()
      );
    }

    if (environment.hasLocalImpl(targetType)) {
      crash(
        nodeIndex,
        "Existing 'impl' block in current scope for type '{}'",
        TypeName(targetType)
      );
    }

    log("impl for {}", TypeName(targetType));
    auto block = parser.getBlock(blockIndex);
    auto scope = environment.pushScope();
    environment.scopes.back().selfType = targetType;
    std::unordered_map<Identifier, Reference> statics;
    for (auto index : block.elements) {
      auto declaration = parser.getDeclaration(index);
      auto nameToken = parser.getDefinition(declaration.definition).name;
      auto name = nameToken->lexeme;
      auto value = compile(declaration.value);
      log("impl {}.{} = {}", TypeName(targetType), name, value);
      auto [_, succeeded] = statics.emplace(name, value);
      if (!succeeded) {
        crash(nameToken, "Duplicate member in impl block '{}'", name);
      }
      if (
        parser.getToken(index)->type != TokenType::Colon || !value.isComptime()
      )
        crash(index, "TODO: non-comptime values");
    }

    environment.impls.witnesses[targetType] = std::move(statics);
    if (log.canLog()) {
      for (auto& [name, value] : environment.impls.witnesses[targetType]) {
        log("impl {}.{} = {}", TypeName(targetType), name, value);
      }
    }

    return Reference(targetType);
  }

  ReturnType functionLiteral(
    Encodings::ParameterList parameters,
    NodeIndex returnIndex,
    NodeIndex body
  ) {
    TypeIndex returnType = Pool()._void;
    if (returnIndex) {
      returnType = compile(returnIndex, Pool().type).unboxType();
      if (!returnType) {
        crash(
          nodeIndex,
          "Return type of function must be a compile-time known type"
        );
      }
    }

    bool forwardDeclare = !body;
    std::vector<TypeIndex> parameterTypes;
    for (NodeIndex parameterIndex : parameters.requiredParameters) {
      auto parameterDefinition = parser.getDefinition(parameterIndex);
      if (!parameterDefinition.type) {
        crash(parameterIndex, "Parameters must have a type");
      }

      auto parameterType = compile(parameterDefinition.type).unboxType();
      if (!parameterType) {
        crash(
          parameterIndex,
          "Parameter type must be a compile time-known type"
        );
      }

      parameterTypes.push_back(parameterType);
    }
    auto [_, tupleType] = Pool().tupleOf(std::move(parameterTypes));
    auto functionType = FunctionType(tupleType, returnType);

    // auto token = parser.getTokenIndex()
    auto token = parser.getToken(nodeIndex);
    token++;
    RegisterName llvmName;
    if (token->type == TokenType::String) {
      llvmName = token->lexeme;
    } else {
      if (forwardDeclare) {
        crash(
          nodeIndex,
          "Can't forward declare anoymnous function; Anonymous functions "
          "require a body"
        );
      }
      llvmName = environment.nextGlobalIndex();
    }
    log("Compiling function: {}", name);

    if (!forwardDeclare) {
      functionStubs.push_back(
        {.name = llvmName,
         .definitionNode = nodeIndex,
         .functionType = functionType,
         .selfType = environment.selfType()}
      );
    } else {
      bool isKernel = parser.getToken(nodeIndex)->type == TokenType::Kernel;
      functionType
        .forwardDeclare(llvmName, globalsStack, isKernel ? "ptx_kernel " : "");
    }

    return Reference(Function(functionType, llvmName));
  }

  ReturnType builtinCall(Encodings::ArgumentList argList) {
    auto builtinToken = parser.getToken(parser.getNode(nodeIndex).token);
    auto argumentNodes = argList.positional;
    auto namedArgs = argList.named;
    switch (builtinToken->type) {
    case TokenType::BUILTIN_NumCast: {
      auto arguments =
        argumentNodes |
        std::views::transform([this](NodeIndex x) { return compile(x); });
      if (!(arguments.size() == 1 || arguments.size() == 2)) {
        crash(
          nodeIndex,
          "Expected 1 or 2 arguments for builtin @numCast, but received "
          "{}",
          arguments.size()
        );
      }
      auto object = arguments[0];
      TypeIndex targetType;
      if (arguments.size() == 2) {
        if (auto type = arguments[1].unboxType()) {
          targetType = type;
        } else {
          crash(
            argumentNodes[1],
            "Second arguments for builtin extend needs to be a "
            "compile-time known type"
          );
        }
      } else if (expectedType) {
        targetType = expectedType;
      } else {
        crash(
          nodeIndex,
          "Builtin @numCast must either take a second argument for the "
          "target type, or have an inferrable target"
        );
      }

      object = toRegister(object);

      auto objectType = arguments[0].getType();
      Reference resultName(environment.makeTemporary(targetType));
      bool isTrunc = objectType.value > targetType.value;
      std::string_view instructionName = isTrunc ? "trunc" : "ext";
      if (objectType == targetType) {
        crash(
          nodeIndex,
          "Unnecessary cast from {} to {}",
          TypeName(objectType),
          TypeName(targetType)
        );
      }
      std::string_view typePrefix;

      if (Pool().isFloat(objectType) && Pool().isFloat(targetType)) {
        typePrefix = "fp";
      } else if (
        Pool().isSignedInt(objectType) && Pool().isSignedInt(objectType)
      ) {
        typePrefix = isTrunc ? "" : "s";
      } else if (
        Pool().isUnsignedInt(objectType) && Pool().isUnsignedInt(targetType)
      ) {
        typePrefix = isTrunc ? "" : "z";
      } else {
        crash(
          nodeIndex,
          "Unable to cast from {} to {}",
          TypeName(objectType),
          TypeName(targetType)
        );
      }
      emitLine(
        "{} = {}{} {} {} to {}",
        resultName,
        typePrefix,
        instructionName,
        LlvmName(objectType),
        object,
        LlvmName(targetType)
      );

      return resultName;
      break;
    }
    case TokenType::BUILTIN_BitCast: {
      auto arguments =
        argumentNodes |
        std::views::transform([this](const NodeIndex x) { return compile(x); });
      if (!(arguments.size() == 1 || arguments.size() == 2)) {
        crash(
          nodeIndex,
          "Expected 1 or 2 arguments for builtin @bitCast, but received "
          "{}",
          arguments.size()
        );
      }
      auto object = arguments[0];
      TypeIndex targetType;
      if (arguments.size() == 2) {
        if (auto type = arguments[1].unboxType()) {
          targetType = type;
        } else {
          crash(
            argumentNodes[1],
            "Second arguments for builtin extend needs to be a "
            "compile-time known type"
          );
        }
      } else if (expectedType) {
        targetType = expectedType;
      } else {
        crash(
          nodeIndex,
          "Builtin @numCast must either take a second argument for the "
          "target type, or have an inferrable target"
        );
      }

      auto objectLiteral = toRegister(object);

      auto objectType = arguments[0].getType();

      Reference resultName(environment.makeTemporary(targetType));
      emitLine(
        "{} = bitcast {} {} to {}",
        resultName,
        LlvmName(objectType),
        object,
        LlvmName(targetType)
      );

      return resultName;
      break;
    }
    case TokenType::BUILTIN_CImport: {
      if (argumentNodes.size() != 2) {
        crash(
          nodeIndex,
          "Builtin '@cImport' must take 2 literal arguments, but was "
          "given {}",
          argumentNodes.size()
        );
      }

      auto includeFile =
        fs::weakly_canonical(inputFilePath.parent_path().append(
          parser.getToken(argumentNodes[0])->lexeme
        ));
      auto fileName = includeFile.string();
      std::unordered_map<std::string_view, TypeIndex> definedTypes;
      for (auto [name, value] : namedArgs) {
        auto valueType = compile(value, Pool().type);
        if (auto type = valueType.unboxType()) {
          definedTypes[parser.getToken(name)->lexeme] = type;
        } else {
          TODO("Error for passing non-type into types");
        }
      }
      CompilerContext::inst().c.clangArgs.push_back("-include");
      CompilerContext::inst().c.clangArgs.push_back(std::move(fileName));

      auto prefix = std::string(parser.getToken(argumentNodes[1])->lexeme);
      return Reference(
        cBindings(std::move(includeFile), prefix, globalsStack, definedTypes)
      );
      break;
    }
    case TokenType::BUILTIN_CDefine: {
      if (argumentNodes.empty() || argumentNodes.size() > 2) {
        crash(
          nodeIndex,
          "Builtin '@cDefine' must have 1 or 2 arguments, but {}",
          argumentNodes.size()
        );
      }
      std::string arg =
        fmt::format("-D{}", parser.getToken(argumentNodes[0])->lexeme);
      if (argumentNodes.size() == 2) {
        arg =
          fmt::format("{}={}", arg, parser.getToken(argumentNodes[1])->lexeme);
      }
      CompilerContext::inst().c.clangArgs.push_back(std::move(arg));
      return Reference::Void();
    }
    case TokenType::BUILTIN_CInclude: {
      if (
        argumentNodes.size() == 1 &&
        parser.nodeType(argumentNodes[0]) == NodeType::Literal
      ) {
        auto fileName =
          parser.getToken(parser.getNode(argumentNodes[0]).token)->lexeme;
        CompilerContext::inst().c.clangArgs.push_back("-include");
        CompilerContext::inst().c.clangArgs.push_back(
          concatPath(fileName).string()
        );
      } else {
        crash(nodeIndex, "Builtin '@cInclude' must take one literal argument");
      }
      return Reference::Void();
    }
    case TokenType::BUILTIN_CIncludeDir: {
      if (
        argumentNodes.size() == 1 &&
        parser.nodeType(argumentNodes[0]) == NodeType::Literal
      ) {
        auto fileName =
          parser.getToken(parser.getNode(argumentNodes[0]).token)->lexeme;
        CompilerContext::inst().c.clangArgs.push_back(
          "-I" + concatPath(fileName).string()
        );
      } else {
        crash(
          nodeIndex,
          "Builtin '@cIncludeDir' must take one literal argument"
        );
      }
      return Reference::Void();
    }
    case TokenType::BUILTIN_Link: {
      if (
        argumentNodes.size() == 1 &&
        parser.nodeType(argumentNodes[0]) == NodeType::Literal
      ) {
        auto libName =
          parser.getToken(parser.getNode(argumentNodes[0]).token)->lexeme;
        CompilerContext::inst().c.linkedLibraries.push_back(
          fmt::format("-l{}", libName)
        );
      } else {
        crash(nodeIndex, "Builtin '@link' must take one literal argument");
      }
      return Reference::Void();
    }
    case TokenType::BUILTIN_LinkDir: {
      if (
        argumentNodes.size() == 1 &&
        parser.nodeType(argumentNodes[0]) == NodeType::Literal
      ) {
        auto libName =
          parser.getToken(parser.getNode(argumentNodes[0]).token)->lexeme;
        CompilerContext::inst().c.linkedLibraries.push_back(
          fmt::format("-L{}", libName)
        );
      } else {
        crash(nodeIndex, "Builtin '@linkDir' must take one literal argument");
      }
      return Reference::Void();
    }
    case TokenType::BUILTIN_Type: {
      if (argumentNodes.size() != 1) {
        crash(
          nodeIndex,
          "Builtin '@type' must take one expression argument, but {} "
          "were provided",
          argumentNodes.size()
        );
      }
      auto arg = compile(argumentNodes[0]);
      return Reference(arg.getType());
    }
    default: {
      crash(nodeIndex, "Malformed builtin '@{}'", builtinToken->lexeme);
    }
    }
  }

  ReturnType numCast(Encodings::ArgumentList args) {
    return builtinCall(args);
  }

  fs::path concatPath(std::string_view path) {
    return fs::weakly_canonical(inputFilePath.parent_path().append(path));
  }

  ReturnType bitCast(Encodings::ArgumentList args) {
    return builtinCall(args);
  }

  ReturnType cImport(Encodings::ArgumentList args) {
    return builtinCall(args);
  }

  ReturnType cDefine(Encodings::ArgumentList args) {
    return builtinCall(args);
  }

  ReturnType cInclude(Encodings::ArgumentList args) {
    return builtinCall(args);
  }

  ReturnType cIncludeDir(Encodings::ArgumentList args) {
    return builtinCall(args);
  }

  ReturnType link(Encodings::ArgumentList args) {
    return builtinCall(args);
  }

  ReturnType linkDir(Encodings::ArgumentList args) {
    return builtinCall(args);
  }

  ReturnType type(Encodings::ArgumentList args) {
    return builtinCall(args);
  }

  ReturnType import(TokenPointer fileName) {
    auto filePath = inputFilePath.parent_path().append(fileName->lexeme);
    log("Importing: {}", filePath.string());
    Environment* import = compile(filePath, targetType);
    if (!import->impls.witnesses.empty()) {
      environment.importedImpls.push_back(&import->impls);
    }
    return Reference(import);
  }

  Reference dereference(Reference pointer) {
    auto type = pointer.getType();
    if (Pool().isPointer(type) || Pool().multiPointerElement(type)) {
      if (auto registerValue = pointer.unbox<RegisterValue>()) {
        OptionalType dereferencedType = Pool().dereference(registerValue->type);
        if (!dereferencedType) {
          crash(
            nodeIndex,
            "Unable to dereference non-pointer type '{}'",
            LlvmName(registerValue->type)
          );
        }
        return Reference(StackValue(
          registerValue->name,
          registerValue->type,
          registerValue->scope
        ));
      } else if (auto lValue = pointer.lValue()) {
        OptionalType dereferencedType = Pool().dereference(lValue->type);
        if (!dereferencedType) {
          crash(
            nodeIndex,
            "Unable to dereference non-pointer type '{}'",
            LlvmName(registerValue->type)
          );
        }
        auto registerValue = std::get<RegisterValue>(toRegister(pointer).value);
        return Reference(StackValue(registerValue.name, dereferencedType));
      } else {
        crash(nodeIndex, "Unable to dereference value");
      }
    }
    crash(nodeIndex, "Can't dereference non-pointer type {}", TypeName(type));
  }

  ReturnType dereference(NodeIndex operand) {
    auto value = compile(operand);
    return dereference(value);
  }

  ReturnType reference(NodeIndex operand) {
    auto value = compile(operand);
    if (auto type = value.unboxType()) {
      return Reference(Pool().pointerTo(type));
    } else if (auto lValue = value.lValue()) {
      auto address = Reference(RegisterValue(
        lValue->name,
        Pool().pointerTo(lValue->type),
        lValue->scope
      ));
      log("Making l-value from pointer");
      if (log.canLog()) {
        parser.locationOf(operand).underline(std::cout);
      }
      return address;
    } else {
      crash(operand, "Unable to make reference to non-stack value or type");
    }
  }

  ReturnType unaryNot(NodeIndex operand) {
    auto value = compile(operand);
    if (bool* boolean = value.unbox<bool>()) {
      return Reference(!*boolean);
    }
    auto type = Pool()._bool;
    if (value.getType() != type) {
      crash(
        operand,
        "Unary not operator '!' can only be used on boolean types; Operand "
        "was of type '{}'",
        TypeName(type)
      );
    }
    auto valueName = toRegister(value);
    auto resultName = Reference(environment.makeTemporary(type));
    emitLine("{} = not i1 {}", resultName, valueName);
    return resultName;
  }

  ReturnType sliceType(NodeIndex typeNode) {
    auto value = compile(typeNode);
    auto elementType = value.unboxType();
    if (!elementType) {
      crash(
        typeNode,
        "Element type for array type (slice, fixed-size, etc) must be a "
        "compile-time known type, but was a '{}'",
        TypeName(value.getType())
      );
    }
    return Reference(Pool().sliceOf(elementType));
  }

  // [^]a
  ReturnType multiPointerTo(NodeIndex typeNode) {
    auto value = compile(typeNode);
    auto elementType = value.unboxType();
    if (elementType) {
      return Reference(Pool().multiPointerTo(elementType));
    } else if (auto stackVal = value.unbox<StackValue>()) {
      auto type = stackVal->type;
      return Reference(StackValue(stackVal->name, Pool().multiPointerTo(type)));
      // TODO: take from global?
    } else {
      crash(
        typeNode,
        "Element type for array type (slice, fixed-size, etc) must be a "
        "compile-time known type, or a non-temporary value, but was a '{}'",
        TypeName(value.getType())
      );
    }
  }

  // a[^]
  ReturnType multiPointerFrom(NodeIndex operand) {
    auto value = compile(operand);
    auto type = value.getType();
    if (auto sizedArray = Pool().sizedArray(type)) {
      auto lValue = value.lValue();
      if (!value.lValue()) {
        crash(
          nodeIndex,
          "Unable to make slice from array stored in temporary value"
        );
      }
      auto type = Pool().multiPointerTo(sizedArray->dereferencedType);
      return Reference(RegisterValue(lValue->name, type, lValue->scope));
    } else if (auto elementType = Pool().sliceElementType(type)) {
      auto slicedObj = toRegister(value);
      auto loadedSlice = *slicedObj.unbox<RegisterValue>();
      auto multiPointerType = Pool().multiPointerTo(elementType);
      auto dataPtr = environment.makeTemporary(multiPointerType);
      emitLine(
        "{} = extractvalue {} {}, 0",
        dataPtr,
        LlvmName(multiPointerType),
        loadedSlice
      );
      TODO("Multipointer from slice");
    } else {
      crash(
        nodeIndex,
        "Unable to make slice from value of type '{}'; Can only slice "
        "sized arrays",
        TypeName(type)
      );
    }
  }

  ReturnType unaryMinus(NodeIndex operand) {
    auto value = compile(operand);
    if (auto intLit = value.unbox<IntLiteral>()) {
      return Reference(IntLiteral(-intLit->value));
    } else if (auto floatLit = value.unbox<FloatLiteral>()) {
      return Reference(FloatLiteral(-floatLit->value));
    }
    auto loaded = toRegister(value);
    auto type = loaded.getType();
    auto result = environment.makeTemporary(type);
    if (Pool().isFloat(type)) {
      emitLine("%{} = fsub {} 0.0, {}", result.name, LlvmName(type), loaded);
      return Reference(result);
    }
    if (Pool().isSignedInt(type)) {
      emitLine("%{} = sub {} 0, {}", result.name, LlvmName(type), loaded);
      return Reference(result);
    }
    crash(
      nodeIndex,
      "Unable to create a negative '{}'. Operand must be a float or signed "
      "integer",
      TypeName(type)
    );
  }

  ReturnType bitwiseNot(NodeIndex operand) {
    auto value = compile(operand);
    if (value.isComptime()) {
      crash(nodeIndex, "Can't take a bitwise not of a comptime value");
    }
    auto type = value.getType();
    if (!(Pool().isSignedInt(type) || Pool().isUnsignedInt(type))) {
      crash(
        nodeIndex,
        "Can't perform operation 'bitwise not' on non-integer value of "
        "type '{}'",
        TypeName(type)
      );
    }
    auto loaded = toRegister(value);
    auto result = environment.makeTemporary(type);
    emitLine("%{} = xor {} -1, {}", result.name, LlvmName(type), loaded);
    return Reference(result);
  }

  ReturnType makeSlice(NodeIndex operand) {
    auto value = compile(operand);
    auto type = value.getType();
    if (auto sizedArray = Pool().sizedArray(type)) {
      if (auto stackVal = value.unbox<StackValue>()) {
        auto copy = *stackVal;
        copy.type = sizedArray->dereferencedType;
        value.value = copy;
      } else {
        crash(
          nodeIndex,
          "Unable to make slice from array stored in temporary value"
        );
      }
      auto length = Reference(IntLiteral(sizedArray->length));
      return makeSlice(value, length);
    } else if (auto slice = Pool().sliceElementType(type)) {
      return value;
    } else {
      crash(
        nodeIndex,
        "Unable to make slice from value of type '{}'; Can only slice "
        "sized arrays",
        TypeName(type)
      );
    }
  }

  ReturnType returnExpr(NodeIndex operand) {
    auto returnValue = parser.readOptional(operand.value);
    if (returns.type) {
      crash(nodeIndex, "Return can only be used inside a function");
    }
    auto returnType = returns.type;
    if (returnValue) {
      // parser.locationOf(returnValue).underline(std::cout);
      if (Pool().isVoid(returnType)) {
        crash(
          returnValue,
          "Cannot return value in a function with a 'void' return type"
        );
      }
      auto value = compile(returnValue, returnType);
      value = toRegister(value);
      if (!Pool().isAssignable(value.getType(), returnType)) {
        crash(
          returnValue,
          "Return value of type '{}' needs to be of type '{}'",
          TypeName(value.getType()),
          TypeName(returnType)
        );
      }
      auto registers = returns.registers;
      if (registers.isMemory()) {
        emitLine("store {} {}, ptr %0\nret void", LlvmName(returnType), value);
      } else if (registers.allInt() || !Pool().isAggregate(returnType)) {
        emitLine("ret {} {}", LlvmName(returnType), value);
      } else {
        if (returns.aggregateTypename.empty()) {
          crash(
            returnValue,
            "Expected an aggregate llvm type name in context for return "
            "value, but was left empty"
          );
        }
        auto storage = environment.addTemporary();
        auto transmuted = environment.addTemporary();
        emitLine("%{} = alloca {}", storage, LlvmName(returnType));
        emitLine("store {} {}, ptr %{}", LlvmName(returnType), value, storage);
        emitLine(
          "%{} = load {}, ptr %{}",
          transmuted,
          returns.aggregateTypename,
          storage
        );
        emitLine("ret {} %{}", returns.aggregateTypename, transmuted);
      }
    } else {
      if (!Pool().isVoid(returnType)) {
        crash(
          returnValue,
          "Must return a value in a function with non-void return type"
        );
      }
      emitLine("ret void");
    }

    environment.hasReturned = true;
    return Reference(Never{});
  }

  ReturnType usingExpr(NodeIndex operand) {
    auto value = compile(operand);
    if (auto env = value.unbox<Environment*>()) {
      environment.usings.push_back(*env);
      for (auto import : (*env)->usings) {
        environment.usings.push_back(import);
      }
      log("New symbols");
      environment.debug();
    } else {
      TODO("using for non-environment objects");
    }
    return Reference::Void();
  }

  ReturnType cudaImport(TokenPointer fileName) {
    if (targetType != TargetType::Cpu) {
      crash(
        nodeIndex,
        "Unable to run '@cudaImport' in gpu-targetting file. Did you mean "
        "'import'?"
      );
    }
    auto filePath = inputFilePath.parent_path().append(fileName->lexeme);
    fmt::println("Importing: {}", filePath.string());
    Environment* import = compile(filePath, TargetType::Gpu);
    return Reference(CudaEnv{import});
  }

  ReturnType ifExpr(Encodings::If node) {
    auto resultType = typeChecker.check(nodeIndex);
    auto shouldLoad = !resultType.lValue;
    auto condition = compile(node.condition, Pool()._bool);
    condition = toRegister(condition);

    auto comptime = condition.isComptime();
    std::stringstream ifInstruction;
    u32 ifLabel = environment.addTemporary();
    if (!comptime) {
      fmt::println(ifInstruction, "{}:", ifLabel);
    }
    environment.currentLabel = ifLabel;
    SwitchCase ifCase{
      .result = compile(node.ifClause, ifInstruction),
      .entryBlock = ifLabel,
      .exitLabel = environment.currentLabel,
      .returns = environment.hasReturned,
    };
    if (shouldLoad && ifCase.result.lValue()) {
      auto frame = stackItems;
      frame.outputFile = &ifInstruction;
      auto guard = push(frame);
      ifCase.result = toRegister(ifCase.result);
    }
    environment.hasReturned = false;

    // Else
    bool hasElse = !!node.elseClause;
    std::stringstream elseInstruction;
    u32 elseLabel = hasElse ? environment.addTemporary() : 0;
    if (hasElse) fmt::println(elseInstruction, "{}:", elseLabel);
    environment.currentLabel = elseLabel;
    SwitchCase elseCase = hasElse ? SwitchCase{
        .result = compile(
          node.elseClause, elseInstruction),
        .entryBlock = elseLabel,
        .exitLabel = environment.currentLabel,
        .returns = environment.hasReturned,
      } : SwitchCase();
    environment.hasReturned = ifCase.returns && elseCase.returns;

    auto hasResultValue = hasElse && resultType.type != Pool()._void;

    if (hasResultValue) {
      resultType.type = Pool().isAssignable(resultType.type, expectedType);
    }

    if (comptime) {
      auto conditionValue = condition.unboxBool();
      if (conditionValue) {
        emitLine("br label %{}", ifLabel);
        emitLine(ifInstruction);
        environment.hasReturned = ifCase.returns;
        environment.currentLabel = ifCase.exitLabel;
        return ifCase.result;
      } else {
        emitLine("br label %{}", elseLabel);
        emitLine(elseInstruction);
        environment.hasReturned = elseCase.returns;
        environment.currentLabel = ifCase.exitLabel;
        return elseCase.result;
      }
    }

    u32 endLabel;
    if (hasElse) {
      emitLine("br i1 {}, label %{}, label %{}", condition, ifLabel, elseLabel);
      emitLine(ifInstruction);
      endLabel = environment.addTemporary();
      if (!ifCase.returns) {
        emitLine("br label %{}", endLabel);
      }
      emitLine(elseInstruction);
      if (!elseCase.returns) {
        emitLine("br label %{}", endLabel);
      }
    } else {
      endLabel = environment.addTemporary();
      emitLine("br i1 {}, label %{}, label %{}", condition, ifLabel, endLabel);
      emitLine(ifInstruction);
      if (!ifCase.returns) {
        emitLine("br label %{}", endLabel);
      }
    }
    environment.currentLabel = endLabel;

    if (ifCase.returns && elseCase.returns) {
      environment.hasReturned = true;
      return Reference(Never{});
    }

    emitLine("{}:", endLabel);

    if (hasResultValue) {
      auto type = resultType.type;
      auto lValue = resultType.lValue;
      auto phiResult =
        lValue ? Reference(StackValue(environment.addTemporary(), type))
               : Reference(environment.makeTemporary(type));
      LlvmName typeName(lValue ? Pool().pointerTo(Pool()._void) : type);
      emitLine(
        "{}  = phi {} [{}, %{}], [{}, %{}]",
        phiResult,
        typeName,
        ifCase.result,
        ifCase.exitLabel,
        elseCase.result,
        elseCase.exitLabel
      );
      return phiResult;
    }

    return Reference::Void();
  }

  Function* findMethod(string_view fieldName, TypeIndex type) {
    auto staticValue = environment.getStatic(type, fieldName);
    if (staticValue) {
      log("Testing if {}.{} is a method", TypeName(type), fieldName);
      log("{}", *staticValue);
      if (auto function = staticValue->unboxFunction()) {
        auto paramTypes = Pool().tupleElements(function->type.parameters);
        if (paramTypes.empty()) {
          crash(
            nodeIndex,
            "Function '{}.{}' must have its first argument be of type '^{}' or "
            "'{} to be called as a method, but it takes 0 arguments'",
            TypeName(type),
            fieldName,
            TypeName(type),
            TypeName(type)
          );
        }

        auto selfType = paramTypes[0];
        if (selfType == type) {
          return function;
        }
        if (
          auto ptrType = Pool().dereference(selfType);
          ptrType && type == ptrType
        ) {
          TODO("Calling methods on pointers");
        }

        crash(
          nodeIndex,
          "Function '{}.{}' must have its first argument be of type '^{}' or "
          "'{}' to be called as a method, but was actually of type '{}'",
          TypeName(type),
          fieldName,
          TypeName(type),
          TypeName(type),
          TypeName(type)
        );
      } else {
        crash(
          nodeIndex,
          "{}.{} can't be called as a method because it's a {}, but must be a "
          "function",
          TypeName(type),
          fieldName,
          TypeName(staticValue->getType())
        );
      }
    }
    crash(
      nodeIndex,
      "No field or method '{}' found in struct '{}'",
      fieldName,
      TypeName(type)
    );
  }

  string_view nameOr(string_view altName) {
    if (name.empty()) return altName;
    return name;
  }

  ReturnType structExpr(Encodings::Struct node) {
    // TODO: methods
    u32 llvmName = environment.structIndex();
    std::stringstream typeInstruction;
    auto frame = stackItems;
    frame.outputFile = &typeInstruction;
    auto guard = push(frame);
    auto prettyName = nameOr("Anonymous Struct");
    log("Making struct with name: {}", prettyName);
    auto [typeIndex, structIndex] = Pool().makeStruct(prettyName, llvmName);
    emit("{} = type {{", LlvmName(typeIndex));

    bool hasFields = false;
    for (auto fieldIndex : node.children) {
      auto fieldNode = parser.getNode(fieldIndex);
      auto nodeType = fieldNode.nodeType;
      switch (nodeType) {
      case NodeType::Definition: {
        if (hasFields) typeInstruction << ", ";
        auto definitionNode = parser.getDefinition(fieldIndex);
        auto fieldName = definitionNode.name->lexeme;
        OptionalType type = compile(definitionNode.type).unboxType();
        if (!type) {
          crash(
            fieldIndex,
            "Type for field '{}' must be known at compile time",
            fieldName
          );
        }

        // TODO: default values
        if (!Pool().getStruct(structIndex).defineField(fieldName, type)) {
          auto definitionIndex =
            Pool().getStruct(structIndex).getField(fieldName).index;
          fmt::println(std::cerr, "Originally defined at:");
          auto originalDef =
            parser.getDefinition(node.children[definitionIndex]).name;
          parser.tokenizer.locationOf(originalDef->lexeme);
          crash(definitionNode.name, "Duplicate field '{}'", fieldName);
        }
        fmt::print(typeInstruction, "{}", LlvmName(type));
        hasFields = true;
        break;
      }
      default:
        TODO("TODO: implement default struct fields");
      }
    }
    typeInstruction << "}";
    globalsStack.push(typeInstruction.str());
    return Reference(typeIndex);
  }

  ReturnType dotAccess(Encodings::DotAccessor node) {
    auto type = typeChecker.check(nodeIndex, expectedType).type;
    if (!node.object && !type) {
      log(
        "Index for object: {} vs {}",
        parser.getNode(nodeIndex).left,
        nodeIndex.value
      );
      crash(
        nodeIndex,
        "Prefix '.' operator requires that statement has an expected type"
      );
    }
    Reference object = node.object ? compile(node.object) : Reference(type);

    std::string_view fieldName = node.fieldName->lexeme;

    if (auto type = object.unboxType()) {
      auto sizing = Pool().getSizing(type);
      if (fieldName == "size") {
        return Reference(IntLiteral(sizing.byteSize));
      } else if (fieldName == "alignment") {
        return Reference(IntLiteral(sizing.alignment.byteAlignment()));
      } else if (fieldName == "bitSize") {
        return Reference(IntLiteral(sizing.bitSize));
      }

      if (auto enumDefinition = Pool().getEnum(type)) {
        if (auto value = enumDefinition->get(fieldName)) {
          return Reference(IntLiteral(*value, type));
        }
        crash(
          nodeIndex,
          "Unknown variant '{}' in enum '{}'",
          fieldName,
          TypeName(type)
        );
      }

      if (auto staticVal = environment.getStatic(type, fieldName)) {
        return *staticVal;
      }

      crash(
        nodeIndex,
        "Unknown associated value '{}.{}'",
        TypeName(type),
        fieldName
      );
    }

    if (auto fileEnv = object.unboxEnv()) {
      if (auto value = fileEnv->find(fieldName)) {
        return *value;
      }

      // Failure case
      auto members = fileEnv->defs | std::views::transform([](const auto& x) {
                       return x.first;
                     });
      for (auto [name, value] : fileEnv->defs) {
        log("{}: {}", name, TypeName(value.getType()));
      }
      crash(
        node.fieldName,
        "Unable to find member '{}' in module\nAvailable fields are {}",
        fieldName,
        fmt::join(members, ", ")
      );
    } else if (auto cudaImport = object.unbox<CudaEnv>()) {
      auto fileEnv = cudaImport->env;
      if (auto value = fileEnv->find(fieldName)) {
        if (auto kernel = value->unbox<Kernel>()) {
          return Reference(*kernel);
        }
        crash(
          node.fieldName,
          "Cuda-imported member '{}' can't be accessed because it's not a "
          "kernel",
          fieldName
        );
      }
      auto members =
        fileEnv->defs | std::views::filter([](pair<string_view, Reference> x) {
          return x.second.unbox<Kernel>() != nullptr;
        }) |
        std::views::transform([](const auto& x) { return x.first; });
      for (auto [name, value] : fileEnv->defs) {
        log("{}: {}", name, TypeName(value.getType()));
      }
      crash(
        node.fieldName,
        "Unable to find member '{}' in module\nAvailable fields are {}",
        fieldName,
        fmt::join(members, ", ")
      );
    }

    // TODO: auto dereference pointers
    type = object.getType();
    OptionalType dereferenced = Pool().dereference(type);
    if (dereferenced) {
      object = dereference(object);
      type = dereferenced;
    } else {
      log("Accessing field for type {}", TypeName(type));
    }

    auto boxedField = Pool().getFieldIndex(type, fieldName);

    if (!boxedField.type) {
      auto method = findMethod(fieldName, type);
      if (auto lValue = object.lValue()) {
        return Reference(BoundFunction(*lValue, method));
      } else if (auto lValue = object.unbox<RegisterValue>()) {
        return Reference(BoundFunction(*lValue, method));
      } else {
        TODO("Method calling on literals");
      }
    }

    auto [fieldType, fieldIndex] = boxedField;
    auto fieldPointer = environment.addTemporary();

    if (object.lValue()) {
      auto result = Reference(StackValue(fieldPointer, fieldType));
      emitLine(
        "{} = getelementptr inbounds {}, ptr {}, i32 0, i32 {}",
        result,
        LlvmName(type),
        object,
        fieldIndex
      );
      return result;
    } else if (auto registerValue = object.unbox<RegisterValue>()) {
      auto result = Reference(RegisterValue(fieldPointer, fieldType));
      emitLine(
        "{} = extractvalue {} {}, {}",
        result,
        LlvmName(type),
        object,
        fieldIndex
      );
      return result;
    } else {
      TODO("error for getting struct field");
    }
  }

  ReturnType argList(NodeIndex nodeIndex) {
    auto type = typeChecker.check(nodeIndex, expectedType).type;
    auto argsNode = parser.getArgumentList(nodeIndex);
    return constructStruct(type, argsNode.positional, argsNode.named);
  }

  ReturnType enumExpr(Encodings::Enum node) {
    // TODO: ADT
    TypeIndex rawType;
    if (node.rawType) {
      auto rawValue = compile(node.rawType);
      if (auto type = rawValue.unboxType()) {
        rawType = type;
        if (!Pool().isInt(rawType)) {
          crash(
            node.rawType,
            "Enum raw type '{}' isn't an integer",
            TypeName(rawType)
          );
        }
      } else {
        crash(
          node.rawType,
          "Enum raw type doesn't refer to a compile time-known integer type; "
          "is a value of type '{}'",
          TypeName(rawValue.getType())
        );
      }
    } else {
      auto bitSize = std::bit_width(node.entries.size());
      // TODO: zero-sized enum?
      // TODO: non-Po2-sized enums
      if (bitSize <= 8) {
        rawType = TypePool()._u8;
      } else if (bitSize <= 16) {
        rawType = TypePool()._u16;
      } else if (bitSize <= 32) {
        rawType = TypePool()._u32;
      } else if (bitSize <= 64) {
        rawType = TypePool()._u64;
      } else {
        crash(
          nodeIndex,
          "Unable to create enum with values that can't fit in 64 bits: {} "
          "values",
          node.entries.size()
        );
      }
    }

    auto [typeIndex, enumIndex] =
      Pool().addEnum(rawType, name.empty() ? "Anonymous Enum" : name);
    u32 valueCount = 0;
    Enum& enumDefinition = Pool().getEnum(enumIndex);
    for (auto [nameToken, valueNode] : node.entries) {
      auto name = parser.getToken(nameToken)->lexeme;

      if (parser.readOptional(valueNode.value)) {
        auto entryValue = compile(valueNode);
        if (auto intVal = entryValue.unbox<IntLiteral>()) {
          valueCount = intVal->value;
        } else {
          crash(
            valueNode,
            "Raw value for enum entry '{}' must be a comptime-known int, but "
            "was '{}'",
            name,
            TypeName(entryValue.getType())
          );
        }
      }

      if (!enumDefinition.define(name, valueCount)) {
        crash(
          parser.getToken(nameToken),
          "Duplicate variant '{}' for enum '{}'",
          name,
          TypeName(typeIndex)
        );
      }
      valueCount++;
    }
    return Reference(typeIndex);
  }

  ReturnType multiLineString(NodeIndex nodeIndex) {
    auto node = parser.getNode(nodeIndex, NodeType::MultiLineString);
    u32 startToken = node.left;
    u32 numLines = node.right;
    std::vector<std::string> escapedLines;
    u32 totalLength = 0;
    for (auto i = 0; i < numLines; i++) {
      auto& line = parser.tokens[startToken + i * 2];
      auto [stringValue, length] = escapeSourceString(line.lexeme, &line);
      totalLength += length;
      escapedLines.push_back(std::move(stringValue));
    }
    log("Escaped lines: {}", escapedLines);
    totalLength += numLines - 1;

    auto global = environment.makeGlobal(Pool()._u8);
    std::stringstream instruction;
    instruction << fmt::format(
      "{} = global [{} x i8] c\"{}\" align 1\n",
      global,
      totalLength,
      fmt::join(escapedLines, "\\0A")
    );
    globalsStack.push(instruction.str());
    auto lengthValue = Reference(IntLiteral(totalLength));
    Reference ref(global);
    return makeSlice(ref, lengthValue);
  }

  ReturnType forLoop(NodeIndex nodeIndex) {
    TODO("For loop");
    // auto node = parser.getForLoop(nodeIndex);
    // auto loopId = environment.addTemporary();
    // auto loopHeader = environment.addLabel("for", loopId);
    // auto loopCondition = loopHeader + ".if";
    // auto loopUpdate = loopHeader + ".else";
    // auto loopBody = loopHeader + ".continue";
    // auto endLabel = loopHeader + ".break";

    // auto iterator = interpret(node.iterator, environment, outputFile,
    // context);

    // if (auto range = iterator.unbox<Range>()) {
    //   TODO("Range iterators");
    // } else if (
    //   auto type = iterator.getType();
    //   auto elementType = Pool().sliceElementType(type)
    // ) {
    //   fmt::println(outputFile, "{}:", loopHeader);

    //   auto sliceRegister = toRegister(&iterator, outputFile, environment);
    //   auto slicePointer =
    //     environment.makeTemporary(Pool().multiPointerTo(*elementType));
    //   auto sliceLength = environment.makeTemporary(Pool()._usize);
    //   auto endPointer =
    //     environment.makeTemporary(Pool().multiPointerTo(*elementType));
    //   fmt::println(
    //     outputFile,
    //     "{} = extractElement {} {}, {} 0",
    //     slicePointer,
    //     LlvmName(type),
    //     sliceRegister,
    //     LlvmName(slicePointer.type)
    //   );
    //   fmt::println(
    //     outputFile,
    //     "{} = extractElement {} {}, {} 1",
    //     sliceLength,
    //     LlvmName(type),
    //     sliceRegister,
    //     LlvmName(sliceLength.type)
    //   );
    //   fmt::println(
    //     outputFile,
    //     "{} = getelementptr {}, ptr {}, {} {}",
    //     endPointer,
    //     LlvmName(*elementType),
    //     slicePointer,
    //     LlvmName(sliceLength.type),
    //     sliceLength
    //   );

    //   fmt::println(outputFile, "{}:", loopCondition);
    //   Environment loopEnv(&environment, loopHeader);
    //   // TODO: by ref vs by value
    //   auto iterationVariable = StackValue(node.capture->lexeme,
    //   *elementType); auto nextIterationVar =
    //     environment.makeTemporary(Pool().multiPointerTo(*elementType));
    //   loopEnv.define(node.capture->lexeme, Reference(iterationVariable));

    //   fmt::println(
    //     outputFile,
    //     "{} = phi ptr [{}, %{}], [{}, %{}]",
    //     iterationVariable,
    //     slicePointer,
    //     loopHeader,
    //     nextIterationVar,
    //     loopUpdate
    //   );
    //   auto loopBound =
    //     environment.makeTemporary(Pool().multiPointerTo(*elementType));
    //   fmt::println(
    //     outputFile,
    //     "{} = icmp eq ptr {}, {}",
    //     loopBound,
    //     iterationVariable,
    //     endPointer
    //   );
    //   fmt::println(
    //     outputFile,
    //     "br i1 {}, label %{}, label %{}",
    //     loopBound,
    //     endLabel,
    //     loopBody
    //   );

    //   fmt::println(outputFile, "{}:", loopBody);
    //   // TODO: loop value??
    //   interpret(node.body, loopEnv, outputFile, context);

    //   fmt::println(outputFile, "br %{}\n{}:", loopUpdate, loopUpdate);
    //   fmt::println(
    //     outputFile,
    //     "{} = getelementptr {}, ptr {}, i64 1\nbr %{}\n{}:",
    //     nextIterationVar,
    //     LlvmName(*elementType),
    //     iterationVariable,
    //     loopCondition,
    //     endLabel
    //   );

    //   return Reference::Void();
    // } else {
    //   TODO("Non-range / slice for loops");
    // }

    // Environment loopEnv(environment);

    // // loopEnv.define();

    // // TODO: consider value expression (see
    // // https://ziglang.org/documentation/master/#while)
    // return Reference::Void();
  }

  ReturnType apply(NodeIndex functionNode, NodeIndex argNode) {
    Encodings::ArgumentList argsList{
      .positional = {&argNode, 1},
      .named = {}
    };
    log("Applying!!!");
    return call(functionNode, argsList);
  }

  Reference toRegister(Reference value) {
    if (auto lValue = value.lValue()) {
      auto type = value.getType();
      auto registerIndex = environment.makeTemporary(type);
      auto registerValue = Reference(registerIndex);
      emitLine("{} = load {}, ptr {}", registerValue, LlvmName(type), value);
      return Reference(registerIndex);
    } else {
      return value;
    }
  }

  void guardExclusiveInBounds(Reference& index, RangeBound& baseLength) {
    auto isOutsideBounds = environment.makeTemporary(Pool()._bool);
    auto crashBlockId = environment.addTemporary();
    auto continueBlockId = environment.addTemporary();
    emitLine(
      "{} = icmp ult {} {}, {}",
      isOutsideBounds,
      LlvmName(Pool()._usize),
      baseLength,
      index
    );
    emitLine(
      "br i1 {}, label %{}, label %{}",
      isOutsideBounds,
      crashBlockId,
      continueBlockId
    );
    emitLine("{}:", crashBlockId);
    crashInstruction();
    emitLine("{}:", continueBlockId);
  }

  void guardIndexInBounds(Reference& index, RangeBound& baseLength) {
    Reference usedIndex = index;
    extendToUsize(usedIndex);

    auto isOutsideBounds = environment.makeTemporary(Pool()._bool);
    auto crashBlockId = environment.addTemporary();
    auto continueBlockId = environment.addTemporary();
    emitLine(
      "{} = icmp ule {} {}, {}",
      isOutsideBounds,
      LlvmName(Pool()._usize),
      baseLength,
      usedIndex
    );
    emitLine(
      "br i1 {}, label %{}, label %{}",
      isOutsideBounds,
      crashBlockId,
      continueBlockId
    );
    emitLine("{}:", crashBlockId);
    crashInstruction();
    emitLine("{}:", continueBlockId);
  }

  RegisterValue guardNonnegativeLength(Reference& lower, RangeBound& upper) {
    auto usize = Pool()._usize;
    auto length = environment.makeTemporary(usize);
    emitLine("{} = sub {} {}, {}", length, LlvmName(usize), upper, lower);

    auto lengthIsNegative = environment.makeTemporary(Pool()._bool);
    auto crashBlockId = environment.addTemporary();
    auto continueBlockId = environment.addTemporary();
    emitLine(
      "{} = icmp slt {} {}, 0",
      lengthIsNegative,
      LlvmName(usize),
      lower
    );
    emitLine(
      "br i1 {}, label %{}, label %{}",
      lengthIsNegative,
      crashBlockId,
      continueBlockId
    );
    emitLine("{}:", crashBlockId);
    crashInstruction();
    emitLine("{}:", continueBlockId);

    return length;
  }

  void crashInstruction(std::ostream& output) {
    fmt::println(output, "call void @llvm.trap()\nunreachable");
  }

  template <typename... Args>
  [[noreturn]] void Todo(
    TokenPointer token,
    fmt::format_string<Args...> fmt,
    Args&&... args
  ) {
    auto& out = std::cerr;
    auto location = parser.tokenizer.locationOf(token->lexeme);
    fmt::println(
      out,
      "TODO: file {} at line {}:{}",
      inputFilePath.string(),
      location.line,
      location.column
    );
    location.underline(out);
    fmt::println(out, fmt, std::forward<Args>(args)...);
    dumpStatements();
    abort();
  }

  template <typename... Args>
  [[noreturn]] void Todo(
    NodeIndex node,
    fmt::format_string<Args...> fmt,
    Args&&... args
  ) {
    auto token = parser.getToken(node);
    fmt::println(
      std::cerr,
      "Crashing on substring {} for node {}",
      token->lexeme,
      (u32)parser.getNode(node).nodeType
    );
    Todo(token, fmt, std::forward<Args>(args)...);
  }

  template <typename... Args>
  [[noreturn]] void crash(
    TokenPointer token,
    fmt::format_string<Args...> fmt,
    Args&&... args
  ) {
    auto& out = std::cerr;
    auto location = parser.tokenizer.locationOf(token->lexeme);
    fmt::println(
      out,
      "Compiler error in file {} at line {}:{}",
      inputFilePath.string(),
      location.line,
      location.column
    );
    location.underline(out);
    fmt::println(out, fmt, std::forward<Args>(args)...);
    dumpStatements();
    abort();
  }

  template <typename... Args>
  [[noreturn]] void crash(
    NodeIndex node,
    fmt::format_string<Args...> fmt,
    Args&&... args
  ) {
    crash(parser.getToken(node), fmt, std::forward<Args>(args)...);
  }

  void dumpStatements() {
    if (!(Logger::globalLevels & LogLevel::Parsing)) return;
    Logger logger{LogLevel::Parsing};
    logger("All nodes");
    parser.dumpNodes();

    logger("All statements");
    for (auto x : program) {
      logger("Node type: {}", (int)parser.nodeType(x));
      parser.locationOf(x).underline(std::cout);
      if (parser.nodeType(x) == NodeType::Declaration) {
        auto value = parser.getDeclaration(x).value;
        if (parser.nodeType(value) == NodeType::FunctionLiteral) {
          auto function = parser.getFunctionLiteral(value);
          if (function.body) {
            auto statements = parser.getBlock(function.body).elements;
            for (auto statement : statements) {
              logger("Node type: {}", (int)parser.nodeType(statement));
              parser.locationOf(statement).underline(std::cout);
            }
          }
        }
      }
    }
  }

  static string_view readFile(fs::path filePath) {
    std::ifstream inputFile(filePath);

    if (!inputFile.is_open()) {
      fmt::println(
        std::cerr,
        "Error: could not open the file {}",
        filePath.string()
      );
      abort();
    } else {
      log("Opened file: {}", filePath.string());
    }

    // File contents needs to be kept around after this TL because names are
    // string_views
    std::string& fileContents = *new std::string(
      std::istreambuf_iterator<char>(inputFile),
      std::istreambuf_iterator<char>()
    );

    return fileContents;
  }

  static Environment* compile(fs::path fileName, TargetType targetType) {
    fmt::println("Compiling file: {}", fileName.string());
    using Imports = std::unordered_map<std::string, Environment>;
    static Imports cpuFiles;
    static Imports gpuFiles;

    Imports& compiledFiles =
      targetType == TargetType::Cpu ? cpuFiles : gpuFiles;
    fileName = fs::absolute(fileName);
    fileName = fs::weakly_canonical(fileName);

    if (compiledFiles.contains(fileName)) {
      return &compiledFiles[fileName];
    }

    auto fileContents = readFile(fileName);

    Tokenizer tokenizer(fileContents, fileName);
    Parser parser(tokenizer);
    std::vector<NodeIndex> program = parser.parse();

    std::ofstream& outFile =
      *(targetType == TargetType::Cpu
          ? CompilerContext::inst().blub.outputFileStream
          : CompilerContext::inst().cuda.outputFileStream);
    Compiler translationUnit(parser, program, outFile);
    auto [env, success] =
      compiledFiles.emplace(std::move(fileName), translationUnit.run());
    // TODO: remove
    if (env->second.defs.empty()) {
      throw std::invalid_argument(
        fmt::format(
          "Empty environment from compiled file {}",
          fileName.string()
        )
      );
    }
    return &env->second;
  }

  Environment run() {
    auto outputFile = targetType == TargetType::Cpu
                        ? CompilerContext::inst().blub.outputFileStream
                        : CompilerContext::inst().cuda.outputFileStream;

    log("Program length: {}", program.size());
    for (auto node : program) {
      log("Trying to compile node: {}", node.value);
      compile(node);
      while (!globalsStack.empty()) {
        emitLine("{}", globalsStack.front());
        globalsStack.pop();
      }
    }

    while (!functionStubs.empty()) {
      auto stub = functionStubs.back();
      defineFunction(stub);
      functionStubs.pop_back();
      while (!globalsStack.empty()) {
        *outputFile << globalsStack.front() << "\n";
        globalsStack.pop();
      }
    }

    dumpStatements();

    return fileEnvironment;
  }

  void defineFunction(FunctionStub stub) {
    auto& instruction = *outputFile;
    instruction << "define ";
    if (parser.getToken(stub.definitionNode)->type == TokenType::Kernel) {
      instruction << "ptx_kernel ";
    }

    Function function(stub.functionType, stub.name);

    auto declarationResult = declareParamRegisters(instruction, function);
    fmt::println("Defining function: {}", stub.name);
    environment.nextTemporary = declarationResult.entryLabel + 1;
    auto node = parser.getFunctionLiteral(stub.definitionNode);
    auto parameters = parser.getParameterList(node.parameters);

    // TODO: attributes
    // https://llvm.org/docs/LangRef.html#function-attributes
    instruction << " {\n";

    vector<Identifier> paramNames;
    paramNames.reserve(
      parameters.requiredParameters.size() +
      parameters.optionalParameters.size()
    );
    u32 parameterIndex = 0;
    auto parameterTypes = Pool().tupleElements(stub.functionType.parameters);
    for (NodeIndex paramNode : parameters.requiredParameters) {
      TypeIndex paramType = parameterTypes[parameterIndex];
      auto parameterDefinition = parser.getDefinition(paramNode);
      string_view paramName = parameterDefinition.name->lexeme;

      paramNames.push_back(paramName);
      if (!environment
             .define(paramName, Reference(StackValue(paramName, paramType)))) {
        crash(paramNode, "Duplicate function parameter {}", paramName);
      }
      parameterIndex++;
    }

    for (NodeIndex parameterIndex : parameters.optionalParameters) {
      Todo(parameterIndex, "Named parameters/default values");
    }

    OutContext loadingContext{
      .outputFile = instruction,
      .environment = environment
    };
    loadParameterRegisters(loadingContext, stub.functionType, paramNames);

    FunctionType functionType = stub.functionType;
    auto returnType = functionType.returnType;

    returns = {
      .type = returnType,
      .aggregateTypename = declarationResult.aggregateReturnTypeName,
      .registers = Pool().registerStorage(returnType),
    };

    auto body = parser.getBlock(node.body);
    for (auto statement : body.elements) {
      compile(statement);
    }

    if (!environment.hasReturned) {
      if (returnType == Pool()._void) {
        instruction << "ret void\n";
      } else {
        crash(stub.definitionNode, "Return required for all code paths");
      }
    }

    instruction << "}\n\n";
    return;
  }

  Compiler(Parser& parser, ChildSpan program, std::ostream& outputFile)
      : outputFile(&outputFile), parser(parser),
        typeChecker(*this, environment, parser), program(program) {}

  struct StackItemsGuard {
    StackItems prevFrame;
    Compiler& compiler;
    ~StackItemsGuard() {
      log("Removing frame where name was '{}'", compiler.name);
      compiler.stackItems = prevFrame;
    }
  };

  StackItemsGuard push(StackItems newFrame) {
    StackItemsGuard guard{stackItems, *this};
    stackItems = newFrame;
    return guard;
  }
};

static_assert(AstVisitor<Compiler>, "Compiler must implement AstVisitor");
