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

enum class TargetType { Cpu, Gpu };

struct CompilerContext {
  struct {
    std::ofstream* outputFileStream;
    std::stringstream globalInitialization;
    unordered_set<TypeIndex> emittedTypeDefinitions;
  } blub;
  struct {
    vector<std::string> linkedLibraries;
    vector<std::string> clangArgs;
  } c;
  struct {
    std::ofstream* outputFileStream;
    std::stringstream globalInitialization;
    vector<std::string> linkedFiles;
    optional<RegisterValue> embeddedPtxGlobal;
    unordered_set<std::string> emittedImports;
    unordered_set<TypeIndex> emittedTypeDefinitions;
  } cuda;

  static CompilerContext& inst() {
    static CompilerContext instance;
    return instance;
  }

  static std::stringstream* globalStream(TargetType targetType) {
    auto& outputFile = targetType == TargetType::Cpu
                         ? CompilerContext::inst().blub.globalInitialization
                         : CompilerContext::inst().cuda.globalInitialization;
    return &outputFile;
  }

  static std::ofstream* fileStream(TargetType targetType) {
    auto outputFile = targetType == TargetType::Cpu
                        ? CompilerContext::inst().blub.outputFileStream
                        : CompilerContext::inst().cuda.outputFileStream;
    return outputFile;
  }
};

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
    stackItems.outputFile = &outFile;
    stackItems.expectedType = targetType;
    stackItems.nodeIndex = index;

    typeChecker.check(nodeIndex, targetType);
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

  ReturnType arrayLiteral(ChildSpan elementNodes, NodeIndex _) {
    std::vector<Reference> elements;
    elements.reserve(elementNodes.size());
    if (elementNodes.empty()) {
      crash(nodeIndex, "Empty array literal");
    }
    auto type = typeChecker.check(nodeIndex, expectedType).type;
    TypeIndex elementType;
    if (auto sizedArray = Pool().sizedArray(type)) {
      elementType = sizedArray->dereferencedType;
    } else {
      crash(
        nodeIndex,
        "Expected type for array literal was not sized array, but was {}",
        TypeName(type)
      );
    }

    for (auto element : elementNodes) {
      elements.push_back(compile(element, elementType));
    }

    auto resultArray = Reference(ZeroInit{});
    u32 i = 0;
    for (auto& element : elements) {
      auto loaded = toRegister(element);
      auto newArray = environment.makeTemporary(type);
      emitLine(
        "{} = insertvalue {} {}, {} {}, {}",
        newArray,
        LlvmName(type),
        resultArray,
        LlvmName(elementType),
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
    span<pair<NodeIndex, NodeIndex>> caseNodes,
    NodeIndex elseBody
  ) {
    auto [resultType, lValue] = typeChecker.check(nodeIndex, expectedType);
    // fmt::println("Is we an l value????: {}", lValue);

    auto loadedCondition = toRegister(compile(conditionIndex));
    auto caseType = loadedCondition.getType();

    if (auto enumType = Pool().getEnum(caseType)) {
      // TODO: Exhaustiveness checking
    }

    vector<SwitchCase> cases;
    cases.reserve(caseNodes.size() + (!!elseBody));
    bool hasDefault;
    for (auto [condition, body] : caseNodes) {
      stringstream instruction;
      // Reserve one for loading stack values
      environment.addTemporary();
      u32 block = environment.addTemporary();
      environment.currentLabel = block;
      cases.push_back({
        .condition = compile(condition),
        .result = compile(body, instruction),
        .entryBlock = block,
        .exitLabel = environment.currentLabel,
        .returns = environment.hasReturned,
      });
      environment.hasReturned = false;
      SwitchCase& switchCase = cases.back();
      switchCase.instructions = std::move(instruction);

      if (switchCase.condition.getType() != caseType) {
        crash(
          condition,
          "Expected type for case condition was {}, but was given {}",
          TypeName(caseType),
          TypeName(switchCase.condition.getType())
        );
      }

      // TODO: non-comptime cases
      if (!switchCase.condition.isComptime()) {
        crash(
          condition,
          "Condition for switch case conditions must be comptime known"
        );
      }

      // TODO: default
    }

    if (elseBody) {
      stringstream instruction;
      // Reserve one for loading stack values
      environment.addTemporary();
      u32 block = environment.addTemporary();
      environment.currentLabel = block;
      hasDefault = true;
      cases.push_back({
        .result = compile(elseBody, instruction),
        .entryBlock = block,
        .exitLabel = environment.currentLabel,
        .returns = environment.hasReturned,
        .isDefault = true,
      });
      cases.back().instructions = std::move(instruction);
      environment.hasReturned = false;
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
    auto oldFrame = push(frame);
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

      environment
        .define(definitionName, value, parser.locationOf(node.definition));
      log(
        "Defined {} with type {}",
        definitionName,
        TypeName(environment.find(definitionName)->getType())
      );
      return Reference::Void();
    } else if (assignmentToken == TokenType::Assign) {
      // TODO: pointer stability might be sussy; consider index
      auto assignee = makeDefinition(
        definitionNode.name,
        definitionNode.type,
        node.definition
      );

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
      log("Making value: '{}: {}'", assignee->name, TypeName(assignee->type));
      emitDefinition(*assignee);
      typeChecker.check(node.value, assignedType);
      auto value = toRegister(coerceValue(compile(node.value), assignedType));
      log(
        "Defined {} with type {}",
        definitionName,
        TypeName(environment.find(definitionName)->getType())
      );

      emitLine("store {} {}, ptr {}", LlvmName(assignedType), value, *assignee);
    } else {
      parser.crash(
        nodeIndex,
        "Internal parser error: token for declaration must be '=' or ':'"
      );
    }

    // stackItems = oldFrame;
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
      ensureTypeDefinition(assignee.type);
      globalsStack.push(
        fmt::format(
          "{} = global {} zeroinitializer align {}",
          assignee,
          LlvmName(assignee.type),
          byteAlignment
        )
      );
      break;
    }
    }
  }

  void ensureTypeDefinition(TypeIndex type) {
    auto& emittedTypes =
      targetType == TargetType::Cpu
        ? CompilerContext::inst().blub.emittedTypeDefinitions
        : CompilerContext::inst().cuda.emittedTypeDefinitions;
    if (emittedTypes.contains(type)) {
      return;
    }

    auto* structDef = Pool().getStruct(type);
    if (!structDef) {
      return;
    }

    std::stringstream definition;
    fmt::print(definition, "{} = type {{", LlvmName(type));
    bool hasFields = false;
    for (auto fieldType : structDef->fieldTypes()) {
      ensureTypeDefinition(fieldType);
      if (hasFields) {
        definition << ", ";
      }
      fmt::print(definition, "{}", LlvmName(fieldType));
      hasFields = true;
    }
    definition << "}";
    globalsStack.push(definition.str());
    emittedTypes.insert(type);
  }

  StackValue* makeDefinition(
    TokenPointer name,
    NodeIndex typeNode,
    NodeIndex defNode
  ) {
    TypeIndex type = Pool().infer;
    if (typeNode) {
      if (auto typeIndex = compile(typeNode).unboxType()) {
        type = typeIndex;
      } else {
        crash(typeNode, "Type for identifier '{}' is not a type", name->lexeme);
      }
    }
    bool isGlobal = environment.envType() == EnvType::Global;
    Reference* definition = environment.define(
      name->lexeme,
      Reference(StackValue(
        isGlobal ? environment.nextGlobalIndex() : environment.addTemporary(),
        type,
        isGlobal ? ValueScope::Global : ValueScope::Local
      )),
      parser.locationOf(defNode)
    );
    if (definition) {
      return definition->lValue();
    }

    crash(name, "Definition for identifier '{}' already exists", name->lexeme);
  }

  ReturnType definition(Encodings::Definition node) {
    assert(node.type);
    auto oldName = name;
    name = node.name->lexeme;
    log("Using name: {}", name);
    StackValue* def = makeDefinition(node.name, node.type, nodeIndex);
    emitDefinition(*def);
    doAssignment(*def, Reference(ZeroInit{}));
    name = oldName;
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
      return Reference(FloatLiteral(floatValue, floatType->precision));
    } else if (Pool().floatLiteral == type) {
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
    TypeIndex type = Pool().addOpaque(nameOr("Anonymous Opaque"));
    return Reference(type);
  }

  ReturnType self(TokenPointer token) {
    if (auto type = environment.selfType()) {
      return Reference(type);
    }
    crash(nodeIndex, "No type 'Self' in context");
  }

  ReturnType undefined(TokenPointer token) {
    auto type = typeChecker.check(nodeIndex);
    return Reference(Never{type.type});
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

  void doAssignment(StackValue assignee, Reference value) {
    auto loadedValue = toRegister(coerceValue(value, assignee.type));
    emitLine(
      "store {} {}, ptr {}",
      LlvmName(assignee.type),
      loadedValue,
      assignee
    );
  }

  Reference constructUnionValue(
    TypeIndex type,
    TypeIndex variantType,
    Reference value
  ) {
    auto storage = environment.addTemporary();
    auto alignment = Pool().getSizing(type).alignment.byteAlignment();

    emitLine("%{} = alloca {}, align {}", storage, LlvmName(type), alignment);
    emitLine("store {} zeroinitializer, ptr %{}", LlvmName(type), storage);
    if (!Pool().isVoid(variantType)) {
      auto casted = environment.addTemporary();
      auto valueRef = toRegister(value);
      emitLine("%{} = bitcast ptr %{} to ptr", casted, storage);
      emitLine(
        "store {} {}, ptr %{}, align {}",
        LlvmName(variantType),
        valueRef,
        casted,
        Pool().getSizing(variantType).alignment.byteAlignment()
      );
    }
    auto result = environment.makeTemporary(type);
    emitLine("{} = load {}, ptr %{}", result, LlvmName(type), storage);
    return Reference(result);
  }

  // TODO: fold more type coercions in. should this just be called when compile
  // returns???
  Reference coerceValue(Reference value, TypeIndex targetType) {
    auto valueType = value.getType();
    if (valueType == targetType) {
      return value;
    }

    if (
      auto unionType = Pool().unbox<Union>(targetType);
      unionType && !Pool().unbox<Union>(valueType)
    ) {
      for (auto [variantType, _] : unionType->namedVariants) {
        if (Pool().isAssignable(valueType, variantType)) {
          return constructUnionValue(targetType, variantType, value);
        }
      }
      for (auto variantType : unionType->anonymousVariants) {
        if (Pool().isAssignable(valueType, variantType)) {
          return constructUnionValue(targetType, variantType, value);
        }
      }
    }
    return value;
  }

  StackValue accessFieldPathPointer(
    StackValue object,
    const FieldPath& fieldPath
  ) {
    StackValue current = object;

    for (auto segment : fieldPath.segments) {
      auto fieldPointer = environment.addTemporary();
      auto result = StackValue(fieldPointer, segment.fieldType);

      if (Pool().unbox<Union>(segment.aggregateType)) {
        emitLine("{} = bitcast ptr {} to ptr", result, current);
      } else {
        emitLine(
          "{} = getelementptr inbounds {}, ptr {}, i32 0, i32 {}",
          result,
          LlvmName(segment.aggregateType),
          current,
          segment.index
        );
      }

      current = result;
    }

    return current;
  }

  ReturnType assign(NodeIndex left, NodeIndex right) {
    auto targetType = typeChecker.check(left);
    // if (!targetType.lValue) {
    //   crash(left, "Unable to assign to non l-value");
    // }
    typeChecker.check(right, targetType.type);

    auto value = compile(right, targetType.type);
    auto assignee = compile(left, targetType.type);

    if (auto lValue = assignee.lValue()) {
      doAssignment(*lValue, value);
    } else {
      crash(
        nodeIndex,
        "Internal error; left side of assignment didn't yield StackValue"
      );
    }

    // TODO: consider value
    return Reference::Void();
  }

  ReturnType binopAssign(NodeIndex left, NodeIndex right, TokenType binop) {
    typeChecker.check(nodeIndex);
    auto value = binopVisit(left, right, nodeIndex, binop, parser, *this);
    auto assignee = compile(left);
    if (auto lValue = assignee.lValue()) {
      doAssignment(*lValue, value);
    } else {
      crash(
        nodeIndex,
        "Internal error; left side of assignment didn't yield StackValue"
      );
    }
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

  struct ComparisonOperator {
    string_view instructionName;
    TokenType type;
    struct {
      bool signedUnsignedDistinction = false;
    };
  };

  ReturnType comparison(NodeIndex a, NodeIndex b, ComparisonOperator op) {
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

      char typePrefix;
      string_view opPrefix;
      type = Pool().rawType(type);
      if (Pool().isFloat(type)) {
        typePrefix = 'f';
        opPrefix = "o";
      } else if (Pool().isSignedInt(type)) {
        typePrefix = 'i';
        opPrefix = op.signedUnsignedDistinction ? "s" : "";
      } else if (Pool().isUnsignedInt(type)) {
        typePrefix = 'i';
        opPrefix = op.signedUnsignedDistinction ? "u" : "";
      } else {
        TODO("Non-primitive comparison operations");
      }
      RegisterValue result = environment.makeTemporary(Pool()._bool);
      emitLine(
        "{} = {}cmp {}{} {} {}, {}",
        result,
        typePrefix,
        opPrefix,
        op.instructionName,
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
    return comparison(a, b, {"eq", TokenType::DoubleEqual, false});
  }

  ReturnType notEqual(NodeIndex a, NodeIndex b) {
    return comparison(a, b, {"ne", TokenType::NotEqual, false});
  }

  ReturnType lt(NodeIndex a, NodeIndex b) {
    return comparison(a, b, {"lt", TokenType::Lt, true});
  }

  ReturnType gt(NodeIndex a, NodeIndex b) {
    return comparison(a, b, {"gt", TokenType::Gt, true});
  }

  ReturnType leq(NodeIndex a, NodeIndex b) {
    return comparison(a, b, {"le", TokenType::Leq, true});
  }

  ReturnType geq(NodeIndex a, NodeIndex b) {
    return comparison(a, b, {"ge", TokenType::Geq, true});
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
    T result;
    switch (opType) {
    case TokenType::Plus: {
      result = a + b;
      break;
    }
    case TokenType::Minus: {
      result = a - b;
      break;
    }
    case TokenType::Mult: {
      result = a * b;
      break;
    }
    case TokenType::Div: {
      result = a / b;
      break;
    }
    case TokenType::LeftDiv: {
      result = b / a;
      break;
    }
    case TokenType::Remainder: {
      result = std::remainder(a, b);
      break;
    }
    default: {
      crash(nodeIndex, "Unknown arithmetic operation");
    }
    }
    return result;
  }

  struct ArithmeticOperator {
    string_view instructionName;
    string_view methodName;
    TokenType type;
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
    typeChecker.check(nodeIndex, expectedType);
    auto aType = typeChecker.check(a).type;
    auto bType = typeChecker.check(b).type;
    if (
      auto type = Pool().coerce(aType, bType); type && Pool().isNumber(type)
    ) {
      auto aVal = compile(a);
      auto bVal = compile(b);
      if (type == Pool().intLiteral) {
        auto a = aVal.unbox<IntLiteral>()->value;
        auto b = bVal.unbox<IntLiteral>()->value;
        return Reference(IntLiteral(literalArithmetic(a, b, op.type)));
      } else if (type == Pool().floatLiteral) {
        auto a = aVal.unboxFloat();
        auto b = bVal.unboxFloat();
        return Reference(FloatLiteral(literalArithmetic(a, b, op.type)));
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
        typeChecker.check(b, bType);
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
    static ArithmeticOperator op{"add", "add", TokenType::Plus};
    return arithmeticOperation(a, b, op);
  }

  ReturnType subtract(NodeIndex a, NodeIndex b) {
    static ArithmeticOperator op{"sub", "subtract", TokenType::Minus};
    return arithmeticOperation(a, b, op);
  }

  static ArithmeticOperator multOp;
  ReturnType multiply(NodeIndex a, NodeIndex b) {
    return arithmeticOperation(a, b, multOp);
  }

  ReturnType divide(NodeIndex a, NodeIndex b) {
    static ArithmeticOperator op{"div", "divide", TokenType::Div, true};
    return arithmeticOperation(a, b, op);
  }

  ReturnType leftDivide(NodeIndex a, NodeIndex b) {
    static ArithmeticOperator
      op{"div", "divide", TokenType::LeftDiv, true, true};
    return arithmeticOperation(a, b, op);
  }

  ReturnType remainder(NodeIndex a, NodeIndex b) {
    static ArithmeticOperator
      op{"rem", "remainder", TokenType::Remainder, true};
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
        if (auto stackVal = list.unbox<StackValue>()) {
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
      positionalArgs.push_back(toRegister(*selfArg));
    }
    u32 i = positionalArgs.size();
    for (auto argNode : positionalArguments) {
      auto paramType = parameterTypes[i];
      auto targetType = typeChecker.check(argNode, paramType).type;
      auto argument = coerceValue(compile(argNode, paramType), paramType);
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
      positionalArgs.push_back(toRegister(argument));
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
      auto argument = coerceValue(compile(value), expectedType);
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
    return result;
  }

  Reference constructStruct(
    TypeIndex type,
    ChildSpan positionalArguments,
    Encodings::NamedValues namedArguments = {}
  ) {
    auto structDefinition = Pool().getStruct(type);
    if (!structDefinition) {
      crash(nodeIndex, "Can't construct non-struct type {}", TypeName(type));
    }
    auto storage = environment.addTemporary();
    auto alignment = Pool().getSizing(type).alignment.byteAlignment();
    auto rootPointer = StackValue(storage, type);
    emitLine("%{} = alloca {}, align {}", storage, LlvmName(type), alignment);
    emitLine("store {} zeroinitializer, ptr %{}", LlvmName(type), storage);

    auto fieldTypes = structDefinition->fieldTypes();
    for (u32 i = 0; i < positionalArguments.size(); i++) {
      auto fieldType = fieldTypes[i];
      auto fieldPath = FieldPath{
        .type = fieldType,
        .segments = {{
          .aggregateType = type,
          .fieldType = fieldType,
          .index = i,
        }},
      };
      auto fieldPointer = accessFieldPathPointer(rootPointer, fieldPath);
      auto argument =
        coerceValue(compile(positionalArguments[i], fieldType), fieldType);
      doAssignment(fieldPointer, argument);
    }

    for (auto [name, value] : namedArguments) {
      auto nameToken = parser.getToken(name);
      auto fieldPath = Pool().getFieldPath(type, nameToken->lexeme);
      if (!fieldPath) {
        crash(nameToken, "Unknown named argument '{}'", nameToken->lexeme);
      }
      auto expectedType = typeChecker.check(value, fieldPath.type).type;
      auto argument = coerceValue(compile(value), expectedType);
      auto fieldPointer = accessFieldPathPointer(rootPointer, fieldPath);
      doAssignment(fieldPointer, argument);
    }

    auto result = environment.makeTemporary(type);
    emitLine("{} = load {}, ptr %{}", result, LlvmName(type), storage);
    return Reference(result);
  }

  Reference constructUnion(
    TypeIndex type,
    ChildSpan positionalArguments,
    Encodings::NamedValues namedArguments = {}
  ) {
    auto unionType = Pool().unbox<Union>(type);
    if (!unionType) {
      crash(nodeIndex, "Can't construct non-union type {}", TypeName(type));
    }

    if (!namedArguments.empty()) {
      if (positionalArguments.size() != 0 || namedArguments.size() != 1) {
        crash(
          nodeIndex,
          "Union construction with named fields requires exactly one field"
        );
      }

      auto [name, value] = namedArguments.front();
      auto nameToken = parser.getToken(name);
      for (auto [variantType, variantName] : unionType->namedVariants) {
        if (variantName == nameToken->lexeme) {
          typeChecker.check(value, variantType);
          return constructUnionValue(
            type,
            variantType,
            compile(value, variantType)
          );
        }
      }
      crash(nameToken, "Unknown union field '{}'", nameToken->lexeme);
    }

    if (positionalArguments.size() != 1 || !unionType->namedVariants.empty()) {
      crash(
        nodeIndex,
        "Union construction requires a single positional argument for "
        "anonymous unions"
      );
    }

    auto valueNode = positionalArguments.front();
    auto valueType = typeChecker.check(valueNode).type;
    OptionalType matchedType;
    for (auto variantType : unionType->anonymousVariants) {
      if (Pool().isAssignable(valueType, variantType)) {
        if (matchedType) {
          crash(
            valueNode,
            "Union constructor argument of type '{}' is ambiguous for '{}'",
            TypeName(valueType),
            TypeName(type)
          );
        }
        matchedType = variantType;
      }
    }
    if (!matchedType) {
      crash(
        valueNode,
        "Unable to assign argument of type '{}' to union '{}'",
        TypeName(valueType),
        TypeName(type)
      );
    }
    return constructUnionValue(
      type,
      matchedType,
      compile(valueNode, matchedType)
    );
  }

  Reference constructAggregate(
    TypeIndex type,
    ChildSpan positionalArguments,
    Encodings::NamedValues namedArguments = {}
  ) {
    if (Pool().getStruct(type)) {
      return constructStruct(type, positionalArguments, namedArguments);
    }
    if (Pool().unbox<Union>(type)) {
      return constructUnion(type, positionalArguments, namedArguments);
    }
    crash(nodeIndex, "Can't construct non-aggregate type {}", TypeName(type));
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
      return constructAggregate(type, args.positional, args.named);
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

    auto block = parser.getBlock(blockIndex);
    log("impl for {}", TypeName(targetType));
    auto scope = environment.pushScope();
    environment.scopes.back().self = targetType;
    assert(targetType == environment.selfType());
    log("impl Self = {}", TypeName(environment.selfType()));

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
        llvmName = name;
        // crash(
        //   nodeIndex,
        //   "Can't forward declare anoymnous function; Anonymous functions "
        //   "require a body"
        // );
      } else if (name == "main") {
        llvmName = "main";
      } else {
        llvmName = environment.nextGlobalIndex();
      }
    }

    if (!forwardDeclare) {
      log("Defining function: {}", name);
      log("with self type");

      if (auto type = environment.selfType()) {
        log("{}", TypeName(type));
      } else {
        log("missing self type");
      }
      functionStubs.push_back(
        {.name = llvmName,
         .definitionNode = nodeIndex,
         .functionType = functionType,
         .selfType = environment.selfType()}
      );
      if (
        targetType == TargetType::Gpu &&
        parser.getToken(nodeIndex)->type == TokenType::Kernel
      ) {
        environment.kernelSymbols[name] = registerNameToString(llvmName);
      }
    } else {
      bool isKernel = parser.getToken(nodeIndex)->type == TokenType::Kernel;
      functionType
        .forwardDeclare(llvmName, globalsStack, isKernel ? "ptx_kernel " : "");
    }

    return Reference(Function(functionType, llvmName));
  }

  ReturnType numCast(Encodings::ArgumentList args) {
    TypeIndex outType = typeChecker.check(nodeIndex, expectedType).type;
    auto object = compile(args.positional[0]);

    object = toRegister(object);

    auto inType = typeChecker.check(args.positional[0]).type;
    Reference result(environment.makeTemporary(outType));
    bool isTrunc = inType.value > outType.value;
    std::string_view instructionName = isTrunc ? "trunc" : "ext";
    if (inType == outType) {
      crash(
        nodeIndex,
        "Unnecessary cast from {} to {}",
        TypeName(inType),
        TypeName(outType)
      );
    }
    std::string_view typePrefix;

    if (Pool().isFloat(inType) && Pool().isFloat(outType)) {
      typePrefix = "fp";
    } else if (Pool().isSignedInt(inType)) {
      typePrefix = isTrunc ? "" : "s";
    } else if (Pool().isUnsignedInt(inType)) {
      typePrefix = isTrunc ? "" : "z";
    } else if (Pool().isFloat(inType)) {
      char sign;
      if (Pool().isUnsignedInt(outType)) sign = 'u';
      else if (Pool().isSignedInt(outType)) sign = 's';
      else
        crash(
          nodeIndex,
          "Internal error: unable to cast from {} to {}",
          TypeName(inType),
          TypeName(outType)
        );

      emitLine(
        "{} = fpto{}i {} {} to {}",
        result,
        sign,
        LlvmName(inType),
        object,
        LlvmName(outType)
      );
      return result;
    } else if (Pool().isFloat(outType)) {
      char sign;
      if (Pool().isUnsignedInt(inType)) sign = 'u';
      else if (Pool().isSignedInt(inType)) sign = 's';
      else
        crash(
          nodeIndex,
          "Internal error: unable to cast from {} to {}",
          TypeName(inType),
          TypeName(outType)
        );
      emitLine(
        "{} = {}itofp {} {} to {}",
        result,
        sign,
        LlvmName(inType),
        object,
        LlvmName(outType)
      );
      return result;
    } else {
      crash(
        nodeIndex,
        "Unable to cast from {} to {}",
        TypeName(inType),
        TypeName(outType)
      );
    }
    emitLine(
      "{} = {}{} {} {} to {}",
      result,
      typePrefix,
      instructionName,
      LlvmName(inType),
      object,
      LlvmName(outType)
    );

    return result;
  }

  fs::path concatPath(std::string_view path) {
    return fs::weakly_canonical(inputFilePath.parent_path().append(path));
  }

  fs::path resolveBlubImportPath(std::string_view path) {
    fs::path localPath = inputFilePath.parent_path() / path;
    if (fs::exists(localPath)) {
      return fs::weakly_canonical(localPath);
    }

    fs::path libPath = fs::path(LIB_BLUB_DIR) / path;
    if (fs::exists(libPath)) {
      return fs::weakly_canonical(libPath);
    }

    return fs::weakly_canonical(localPath);
  }

  ReturnType bitCast(Encodings::ArgumentList args) {
    auto targetType = typeChecker.check(nodeIndex, expectedType).type;
    auto inType = typeChecker.check(args.positional[0]).type;
    auto in = toRegister(compile(args.positional[0], inType));
    auto storage = environment.addTemporary();
    auto alignment = std::max(
      Pool().getSizing(inType).alignment.byteAlignment(),
      Pool().getSizing(targetType).alignment.byteAlignment()
    );
    emitLine("%{} = alloca {}, align {}", storage, LlvmName(inType), alignment);

    Reference resultName(environment.makeTemporary(targetType));
    emitLine(
      "{} = load {}, ptr %{}",
      resultName,
      LlvmName(targetType),
      storage
    );

    return resultName;
  }

  ReturnType cImport(Encodings::ArgumentList args) {
    auto argumentNodes = args.positional;
    if (argumentNodes.size() != 2) {
      crash(
        nodeIndex,
        "Builtin '@cImport' must take 2 literal arguments, but was "
        "given {}",
        argumentNodes.size()
      );
    }

    auto includeFile =
      fs::absolute(
        inputFilePath.parent_path() / parser.getToken(argumentNodes[0])->lexeme
      )
        .lexically_normal();
    auto fileName = includeFile.string();
    std::unordered_map<std::string_view, TypeIndex> definedTypes;
    for (auto [name, value] : args.named) {
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
  }

  ReturnType cudaPtx(NodeIndex module) {
    auto importValue = compile(module);
    auto importType = typeChecker.check(module).type;
    auto* importInfo = findCudaImportInfo(importType);
    if (!importInfo) {
      crash(
        module,
        "Builtin '@cudaPtx' requires a value returned by '@cudaImport'"
      );
    }

    auto loadedImport = toRegister(importValue);
    auto result = environment.makeTemporary(Pool().pointerTo(Pool()._u8));
    emitLine(
      "{} = extractvalue {} {}, 0",
      result,
      LlvmName(importType),
      loadedImport
    );
    return Reference(result);
  }

  ReturnType cDefine(Encodings::ArgumentList args) {
    auto& clangArgs = CompilerContext::inst().c.clangArgs;
    for (auto arg : args.positional) {
      clangArgs.push_back(fmt::format("-D{}", parser.getToken(arg)->lexeme));
    }

    for (auto [name, value] : args.named) {
      clangArgs.push_back(
        fmt::format(
          "-D{}={}",
          parser.getToken(name)->lexeme,
          parser.getToken(value)->lexeme
        )
      );
    }
    return Reference::Void();
  }

  ReturnType cInclude(Encodings::ArgumentList args) {
    if (args.positional.size() != 1) {
      parser.crash(
        nodeIndex,
        "Builtin @cInclude must take in 1 string as a positional argument, but "
        "{} were "
        "given",
        args.positional.size()
      );
    }
    if (!args.named.empty()) {
      parser.crash(
        nodeIndex,
        "Builtin @cInclude takes no named arguments; {} were given",
        args.named.size()
      );
    }
    auto fileName = parser.getToken(args.positional[0]);
    if (fileName->type != TokenType::String) {
      parser.crash(
        args.positional[0],
        "Builtin @cInclude must take in 1 string literal"
      );
    }

    CompilerContext::inst().c.clangArgs.push_back("-include");
    CompilerContext::inst().c.clangArgs.push_back(
      concatPath(fileName->lexeme).string()
    );
    return Reference::Void();
  }

  ReturnType cIncludeDir(Encodings::ArgumentList args) {
    if (
      args.positional.size() == 1 &&
      parser.nodeType(args.positional[0]) == NodeType::Literal &&
      args.named.empty()
    ) {
      auto fileName =
        parser.getToken(parser.getNode(args.positional[0]).token)->lexeme;
      CompilerContext::inst().c.clangArgs.push_back(
        "-I" + concatPath(fileName).string()
      );
    } else {
      crash(
        nodeIndex,
        "Builtin '@cIncludeDir' must take only one literal argument"
      );
    }
    return Reference::Void();
  }

  ReturnType link(Encodings::ArgumentList args) {
    if (
      args.positional.size() == 1 &&
      parser.getToken(args.positional[0])->type == TokenType::String &&
      args.named.empty()
    ) {
      auto libName =
        parser.getToken(parser.getNode(args.positional[0]).token)->lexeme;
      CompilerContext::inst().c.linkedLibraries.push_back(
        fmt::format("-l{}", libName)
      );
    } else {
      crash(
        nodeIndex,
        "Builtin '@link' must take one positional string literal argument"
      );
    }
    return Reference::Void();
  }

  ReturnType linkDir(Encodings::ArgumentList args) {
    if (
      args.positional.size() == 1 &&
      parser.getToken(args.positional[0])->type == TokenType::String &&
      args.named.empty()
    ) {
      auto libName = parser.getToken(args.positional[0])->lexeme;
      CompilerContext::inst().c.linkedLibraries.push_back(
        fmt::format("-L{}", libName)
      );
    } else {
      crash(
        nodeIndex,
        "Builtin '@linkDir' must take one positional string literal argument"
      );
    }
    return Reference::Void();
  }

  ReturnType type(Encodings::ArgumentList args) {
    if (args.positional.size() != 1 || !args.named.empty()) {
      crash(
        nodeIndex,
        "Builtin '@type' must take one expression argument, but {} "
        "were provided",
        args.positional.size()
      );
    }
    return Reference(typeChecker.check(args.positional[0]).type);
  }

  ReturnType import(TokenPointer fileName) {
    auto filePath = resolveBlubImportPath(fileName->lexeme);
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

  ReturnType returnExpr(NodeIndex returnValue) {
    if (!returns.type) {
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
      auto value = coerceValue(compile(returnValue, returnType), returnType);
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
    auto filePath = resolveBlubImportPath(fileName->lexeme);
    fmt::println("Importing: {}", filePath.string());
    auto& importInfo = getCudaImportInfo(filePath);

    auto& cudaContext = CompilerContext::inst().cuda;
    cudaContext.embeddedPtxGlobal = importInfo.ptxGlobal;
    auto importKey = filePath.string();
    if (!cudaContext.emittedImports.contains(importKey)) {
      ensureTypeDefinition(importInfo.type);
      for (auto [fieldName, symbolGlobal] : importInfo.symbolGlobals) {
        auto exportedName = importInfo.kernelSymbols[fieldName];
        auto escaped = escapeSourceString(exportedName, fileName).first;
        globalsStack.push(
          fmt::format(
            "@{} = global [{} x i8] c\"{}\\00\" align 1",
            symbolGlobal.name,
            exportedName.size() + 1,
            escaped
          )
        );
      }
      cudaContext.emittedImports.insert(importKey);
    }

    auto result = environment.makeTemporary(importInfo.type);
    emitLine(
      "{} = insertvalue {} undef, ptr @{}, 0",
      result,
      LlvmName(importInfo.type),
      importInfo.ptxGlobal.name
    );
    u32 fieldIndex = 1;
    for (auto [_, symbolGlobal] : importInfo.symbolGlobals) {
      auto next = environment.makeTemporary(importInfo.type);
      emitLine(
        "{} = insertvalue {} {}, ptr @{}, {}",
        next,
        LlvmName(importInfo.type),
        result,
        symbolGlobal.name,
        fieldIndex
      );
      result = next;
      fieldIndex++;
    }
    return Reference(result);
  }

  ReturnType ifExpr(Encodings::If node) {
    auto [type, lValue] = typeChecker.check(nodeIndex);
    auto shouldLoad = !lValue;
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
    if (hasElse && shouldLoad && elseCase.result.lValue()) {
      auto frame = stackItems;
      frame.outputFile = &elseInstruction;
      auto guard = push(frame);
      elseCase.result = toRegister(elseCase.result);
    }
    environment.hasReturned = ifCase.returns && elseCase.returns;

    auto hasResultValue = hasElse && type != Pool()._void;

    if (hasResultValue) {
      type = Pool().isAssignable(type, expectedType);
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
        if (type == Pool().dereference(selfType)) {
          return function;
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
    if (name.empty()) {
      log("Using name {}; Stored name is {}", altName, name);
      return altName;
    }
    name = StringPool::inst().copy(name);
    log("Using name {}", name);
    return name;
  }

  std::unordered_map<u32, TypeIndex> typeCache;

  ReturnType structExpr(Encodings::Struct node) {
    auto it = typeCache.find(nodeIndex.value);
    if (it != typeCache.end()) {
      return Reference(it->second);
    }
    auto isUnion = parser.getToken(nodeIndex)->type == TokenType::Union;
    auto prettyName = nameOr(isUnion ? "Anonymous Union" : "Anonymous Struct");
    log("Making {} with name: {}", isUnion ? "union" : "struct", prettyName);

    if (isUnion) {
      Union unionDef;
      for (auto fieldIndex : node.children) {
        auto fieldNode = parser.getNode(fieldIndex);
        switch (fieldNode.nodeType) {
        case NodeType::Definition: {
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
          auto duplicate = std::ranges::find_if(
            unionDef.namedVariants,
            [&](const auto& variant) { return variant.second == fieldName; }
          );
          if (duplicate != unionDef.namedVariants.end()) {
            crash(definitionNode.name, "Duplicate field '{}'", fieldName);
          }
          unionDef.namedVariants.push_back({type, fieldName});
          break;
        }
        default:
          TODO("TODO: implement default union fields");
        }
      }
      auto typeIndex = Pool().addType(std::move(unionDef));
      typeCache[nodeIndex.value] = typeIndex;
      return Reference(typeIndex);
    }

    // TODO: methods
    u32 llvmName = environment.structIndex();
    std::stringstream typeInstruction;
    auto frame = stackItems;
    frame.outputFile = &typeInstruction;
    auto guard = push(frame);
    auto [typeIndex, structIndex] = Pool().makeStruct(prettyName, llvmName);
    auto envGuard = environment.pushScope();
    environment.scopes.back().self = typeIndex;
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
    auto& emittedTypes =
      targetType == TargetType::Cpu
        ? CompilerContext::inst().blub.emittedTypeDefinitions
        : CompilerContext::inst().cuda.emittedTypeDefinitions;
    emittedTypes.insert(typeIndex);
    typeCache[nodeIndex.value] = typeIndex;
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

    auto fieldPath = Pool().getFieldPath(type, fieldName);

    if (!fieldPath) {
      auto method = findMethod(fieldName, type);

      auto selfType = method->type.parameters.fields().front();
      bool takesPointer = type == Pool().dereference(selfType);
      if (auto lValue = object.lValue()) {
        if (takesPointer) {
          return Reference(BoundFunction(
            RegisterValue(lValue->name, selfType, lValue->scope),
            method
          ));
        }
        return Reference(BoundFunction(*lValue, method));
      } else if (auto registerVal = object.unbox<RegisterValue>()) {
        if (takesPointer) {
          crash(
            nodeIndex,
            "Unable to treat temporary value as reference for first argument "
            "in method call. Expected type '{}'",
            TypeName(selfType)
          );
        }
        return Reference(BoundFunction(*registerVal, method));
      } else {
        TODO("Method calling on literals");
      }
    }

    if (object.lValue()) {
      return Reference(accessFieldPathPointer(*object.lValue(), fieldPath));
    } else if (object.unbox<RegisterValue>()) {
      auto storage = environment.addTemporary();
      auto alignment =
        std::max(1u, Pool().getSizing(type).alignment.byteAlignment());
      emitLine("%{} = alloca {}, align {}", storage, LlvmName(type), alignment);
      emitLine("store {} {}, ptr %{}", LlvmName(type), object, storage);
      auto fieldPointer =
        accessFieldPathPointer(StackValue(storage, type), fieldPath);
      auto loadedValue = environment.makeTemporary(fieldPath.type);
      emitLine(
        "{} = load {}, ptr {}",
        loadedValue,
        LlvmName(fieldPath.type),
        fieldPointer
      );
      return Reference(loadedValue);
    } else {
      TODO("error for getting struct field");
    }
  }

  ReturnType argList(NodeIndex nodeIndex) {
    auto type = typeChecker.check(nodeIndex, expectedType).type;
    auto argsNode = parser.getArgumentList(nodeIndex);
    return constructAggregate(type, argsNode.positional, argsNode.named);
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
      Pool().addEnum(rawType, nameOr("Anonymous Enum"));
    u32 valueCount = 0;
    Enum& enumDefinition = Pool().getEnum(enumIndex);
    for (auto [nameToken, valueNode] : node.entries) {
      auto name = parser.getToken(nameToken)->lexeme;

      if (valueNode) {
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
    globalsStack.push(Environment::dumpEntryNames(enumDefinition).str());
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

  ReturnType forLoop(TokenPointer var, NodeIndex iteratorNode, NodeIndex body) {
    auto node = parser.getForLoop(nodeIndex);
    // auto loopCondition = loopHeader + ".if";
    // auto loopUpdate = loopHeader + ".else";
    // auto loopBody = loopHeader + ".continue";
    // auto endLabel = loopHeader + ".break";

    auto iterator = compile(iteratorNode);

    if (auto range = iterator.unbox<Range>()) {
      TODO("Range iterators");
    } else if (
      auto type = iterator.getType();
      auto elementType = Pool().sliceElementType(type)
    ) {
      auto loopHeader = environment.addTemporary();
      emitLine("br label %{}\n{}:", loopHeader, loopHeader);
      auto sliceRegister = toRegister(iterator);
      auto slicePointer =
        environment.makeTemporary(Pool().multiPointerTo(elementType));
      auto sliceLength = environment.makeTemporary(Pool()._usize);
      auto endPointer =
        environment.makeTemporary(Pool().multiPointerTo(elementType));

      emitLine(
        "{} = extractElement {} {}, {} 0",
        slicePointer,
        LlvmName(type),
        sliceRegister,
        LlvmName(slicePointer.type)
      );
      emitLine(
        "{} = extractElement {} {}, {} 1",
        sliceLength,
        LlvmName(type),
        sliceRegister,
        LlvmName(sliceLength.type)
      );
      emitLine(
        "{} = getelementptr {}, ptr {}, {} {}",
        endPointer,
        LlvmName(elementType),
        slicePointer,
        LlvmName(sliceLength.type),
        sliceLength
      );

      auto loopCondition = environment.addTemporary();
      emitLine("{}:", loopCondition);
      // TODO: by ref vs by value
      auto iterationName = node.capture->lexeme;
      auto iterationVariable =
        StackValue(environment.addTemporary(), elementType);
      auto nextIterationVar =
        environment.makeTemporary(Pool().multiPointerTo(elementType));
      auto defGuard = environment.pushScope();
      if (!environment.define(
            iterationName,
            Reference(iterationVariable),
            parser.locationOf(node.iterator)
          )) {

        auto original = environment.definitionLocation(iterationName);
        fmt::println(std::cerr, "{} originally defined at:", iterationName);
        original->underline(std::cerr);
        crash(
          node.capture,
          "Parameter name {} shadows a higher scope",
          iterationName
        );
      }

      auto loopBound =
        environment.makeTemporary(Pool().multiPointerTo(elementType));

      auto loopBody = environment.addTemporary();

      // TODO: loop value??
      stringstream body;
      compile(node.body, body);

      auto loopUpdate = environment.addTemporary();
      auto endLabel = environment.addTemporary();

      emitLine(
        "{} = phi ptr [{}, %{}], [{}, %{}]",
        iterationVariable,
        slicePointer,
        loopHeader,
        nextIterationVar,
        loopUpdate
      );
      emitLine(
        "{} = icmp eq ptr {}, {}",
        loopBound,
        iterationVariable,
        endPointer
      );
      emitLine("br i1 {}, label %{}, label %{}", loopBound, endLabel, loopBody);
      emitLine("{}:", loopBody);
      emitLine(body);

      emitLine("br %{}\n{}:", loopUpdate, loopUpdate);
      emitLine(
        "{} = getelementptr {}, ptr {}, i64 1\nbr %{}\n{}:",
        nextIterationVar,
        LlvmName(elementType),
        iterationVariable,
        loopCondition,
        endLabel
      );

      return Reference::Void();
    } else {
      TODO("Non-range / slice for loops");
    }

    // TODO: consider value expression (see
    // https://ziglang.org/documentation/master/#while)
    return Reference::Void();
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

    Compiler translationUnit(parser, program, targetType);
    translationUnit.inputFilePath = fileName;
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
    auto finalFile = targetType == TargetType::Cpu
                       ? CompilerContext::inst().blub.outputFileStream
                       : CompilerContext::inst().cuda.outputFileStream;

    log("Program length: {}", program.size());
    for (auto node : program) {
      log("Trying to compile node: {}", node.value);
      compile(node);
      while (!globalsStack.empty()) {
        *finalFile << globalsStack.front() << "\n";
        globalsStack.pop();
      }
    }

    while (!functionStubs.empty()) {
      auto stub = functionStubs.back();
      codegenFunction(stub);
      functionStubs.pop_back();
      while (!globalsStack.empty()) {
        *finalFile << globalsStack.front() << "\n";
        globalsStack.pop();
      }
    }

    dumpStatements();

    return std::move(environment);
  }

  void codegenKernel(FunctionStub stub) {
    environment.hasReturned = false;
    auto envGuard = environment.pushScope();
    environment.scopes.back().envType = EnvType::Function;
    environment.scopes.back().self = stub.selfType;
    log("Codegening kernel {} with self type:", stub.name);
    if (stub.selfType) {
      log("{}", TypeName(stub.selfType));
    } else {
      log("Missing self type");
    }
    std::stringstream instruction;
    auto stackGuard = push(
      {.outputFile = &instruction,
       .expectedType = Pool().infer,
       .nodeIndex = stub.definitionNode,
       .name = {}}
    );
    // auto& instruction = *outputFile;
    emit("define ptx_kernel void @\"{}\"(", stub.name);

    // auto declarationResult = declareParamRegisters(instruction, function);
    // environment.nextTemporary = declarationResult.entryLabel + 1;
    auto parameterTypes = Pool().tupleElements(stub.functionType.parameters);
    bool hasMultiple = false;
    for (auto type : parameterTypes) {
      if (hasMultiple) instruction << ", ";
      if (Pool().dereference(type)) {
        instruction << "addrspace(1) ptr";
      } else {
        emit("{}", LlvmName(type));
      }
      hasMultiple = true;
    }
    instruction << ") {\n";
    auto node = parser.getFunctionLiteral(stub.definitionNode);
    auto parameters = parser.getParameterList(node.parameters);
    u32 parameterIndex = 0;
    for (NodeIndex paramNode : parameters.requiredParameters) {
      TypeIndex paramType = parameterTypes[parameterIndex];
      auto parameterDefinition = parser.getDefinition(paramNode);
      string_view paramName = parameterDefinition.name->lexeme;
      if (!environment.define(
            paramName,
            Reference(RegisterValue(parameterIndex, paramType)),
            parser.locationOf(paramNode)
          )) {
        auto original = environment.definitionLocation(paramName);
        fmt::println(std::cerr, "{} originally defined at:", paramName);
        original->underline(std::cerr);
        crash(paramNode, "Parameter name {} shadows a higher scope", paramName);
      }
      parameterIndex++;
    }

    for (NodeIndex parameterIndex : parameters.optionalParameters) {
      Todo(parameterIndex, "Named parameters/default values for cuda kernels");
    }

    // Load parameter registers
    FunctionType functionType = stub.functionType;
    auto returnType = functionType.returnType;

    returns = {
      .type = returnType,
      .aggregateTypename = "",
      .registers = Pool().registerStorage(returnType),
    };
    environment.scopes.back().returnType = returnType;

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
    environment.scopes.back().envType = EnvType::Global;
    globalsStack.push(instruction.str());
  }

  void codegenFunction(FunctionStub stub) {
    if (parser.getToken(stub.definitionNode)->type == TokenType::Kernel) {
      codegenKernel(stub);
      return;
    }
    environment.hasReturned = false;
    auto envGuard = environment.pushScope();
    environment.scopes.back().envType = EnvType::Function;
    environment.scopes.back().self = stub.selfType;
    log("Codegening function {} with self type:", stub.name);
    if (stub.selfType) {
      log("{}", TypeName(stub.selfType));
    } else {
      log("Missing self type");
    }
    std::stringstream instruction;
    auto stackGuard = push(
      {.outputFile = &instruction,
       .expectedType = Pool().infer,
       .nodeIndex = stub.definitionNode,
       .name = {}}
    );
    // auto& instruction = *outputFile;
    instruction << "define ";

    Function function(stub.functionType, stub.name);

    auto declarationResult = declareParamRegisters(instruction, function);
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

      if (
        std::find(paramNames.begin(), paramNames.end(), paramName) !=
        paramNames.end()
      ) {
        crash(paramNode, "Duplicate function parameter {}", paramName);
      }
      paramNames.push_back(paramName);
      if (!environment.define(
            paramName,
            Reference(StackValue(paramName, paramType)),
            parser.locationOf(paramNode)
          )) {
        auto original = environment.definitionLocation(paramName);
        fmt::println(std::cerr, "{} originally defined at:", paramName);
        original->underline(std::cerr);
        crash(paramNode, "Parameter name {} shadows a higher scope", paramName);
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
    environment.scopes.back().returnType = returnType;

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
    environment.scopes.back().envType = EnvType::Global;
    globalsStack.push(instruction.str());
  }

  Compiler(Parser& parser, ChildSpan program, TargetType targetType)
      : outputFile(CompilerContext::globalStream(targetType)), parser(parser),
        typeChecker(*this, environment, parser), program(program),
        targetType(targetType) {}

  struct StackItemsGuard {
    StackItems prevFrame;
    Compiler& compiler;
    ~StackItemsGuard() {
      compiler.stackItems = prevFrame;
    }
  };

  StackItemsGuard push(StackItems newFrame) {
    StackItemsGuard guard{stackItems, *this};
    stackItems = newFrame;
    return guard;
  }

  Reference dispatchKernel(
    InstancedKernel kernel,
    ChildSpan positionalArgs,
    Encodings::NamedValues namedArguments
  ) {
    TODO("Dispatch kernel");
  }

  Reference nullPointer() {
    return Reference(NullPointer{});
  }

  Reference builtinName(NodeIndex expr) {
    auto type = typeChecker.check(expr);
    auto value = toRegister(compile(expr));
    if (auto enumDef = Pool().getEnum(type.type)) {
      auto ptr = environment.addTemporary();
      emitLine(
        "%{} = getelementptr {}, ptr @{}, {} {}",
        ptr,
        SliceName,
        enumDef->namesArrayGlobal,
        LlvmName(type.type),
        value
      );
      auto result = environment.makeTemporary(Pool().sliceOf(Pool()._u8));
      emitLine("{} = load {}, ptr %{}", result, SliceName, ptr);
      return Reference(result);
    } else {
      crash(
        expr,
        "Expected value with an enum type as argument to builtin '@name'"
      );
    }
  }
};

static_assert(AstVisitor<Compiler>, "Compiler must implement AstVisitor");

inline auto& cudaImportInfoCache() {
  static std::unordered_map<std::string, CudaImportInfo> cache;
  return cache;
}

inline CudaImportInfo& getCudaImportInfo(const fs::path& rawFilePath) {
  auto& cache = cudaImportInfoCache();

  auto filePath = fs::weakly_canonical(fs::absolute(rawFilePath));
  auto key = filePath.string();
  if (auto found = cache.find(key); found != cache.end()) {
    return found->second;
  }

  auto* env = Compiler::compile(filePath, TargetType::Gpu);
  auto& importInfo = cache[key];

  u32 moduleIndex = Environment::structIndex();
  auto typeName =
    StringPool::inst().copy(fmt::format("CudaImport.{}", moduleIndex));
  auto [typeIndex, structIndex] = Pool().makeStruct(typeName, moduleIndex);
  auto& structDef = Pool().getStruct(structIndex);
  auto ptrType = Pool().pointerTo(Pool()._u8);

  importInfo.ptxFieldName = "__anon_cuda_import_ptx";
  structDef.defineField(importInfo.ptxFieldName, ptrType);
  importInfo.ptxGlobal =
    RegisterValue("cuda_embedded_ptx", ptrType, ValueScope::Global);

  for (auto [kernelName, symbolName] : env->kernelSymbols) {
    auto fieldName = StringPool::inst().copy(kernelName);
    structDef.defineField(fieldName, ptrType);
    importInfo.kernelSymbols[fieldName] = symbolName;
    importInfo.symbolGlobals[fieldName] = RegisterValue(
      StringPool::inst().copy(
        fmt::format("cuda_symbol_{}_{}", moduleIndex, fieldName)
      ),
      ptrType,
      ValueScope::Global
    );
  }
  importInfo.type = typeIndex;
  return importInfo;
}

inline CudaImportInfo* findCudaImportInfo(TypeIndex type) {
  for (auto& [_, info] : cudaImportInfoCache()) {
    if (info.type == type) {
      return &info;
    }
  }
  return nullptr;
}
