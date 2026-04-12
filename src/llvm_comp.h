#pragma once
#include "abi.h"
#include "cimport.h"
#include "common.h"
#include "compilercontext.h"
#include "fmt/base.h"
#include "parser.h"
#include "registers.h"
#include "tokenizer.h"
#include "types.h"
#include "value.h"
#include <bit>
#include <cctype>
#include <cmath>
#include <filesystem>
#include <fmt/args.h>
#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <fmt/ranges.h>
#include <iostream>
#include <optional>
#include <ostream>
#include <queue>
#include <ranges>
#include <span>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

namespace fs = std::filesystem;

struct StatementContext {
  optional<string_view> name;

  OptionalType expectedType;
  OptionalType selfType;
  struct {
    OptionalType type;
    string_view aggregateTypename;
    RegisterAssignment registers;
  } returns;
  // Used bc llvm return types with floats are sussy
};

struct CompilerContext {
  struct {
    std::ofstream* outputFileStream;
    std::stringstream globalInitialization;
  } blub;
  struct {
    std::vector<std::string> linkedLibraries;
    std::vector<std::string> clangArgs;
  } c;
  struct {
    std::ofstream* outputFileStream;
    std::stringstream globalInitialization;
  } cuda;

  static CompilerContext& inst() {
    static CompilerContext instance;
    return instance;
  }
};

enum class TargetType { Cpu, Gpu };

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

class TranslationUnit {
public:
  std::ofstream& outputFileStream;
  fs::path inputFilePath;
  Environment fileEnvironment;
  Parser& parser;
  std::span<NodeIndex> program;
  std::queue<std::string> globalsStack;
  Logger log;
  TargetType targetType;
  std::vector<FunctionStub> functionStubs;

  TranslationUnit(
    Parser& parser,
    std::span<NodeIndex> program,
    fs::path inputPath,
    std::ofstream& outputFileStream,
    TargetType targetType = TargetType::Cpu
  )
      : parser(parser), fileEnvironment(), program(program),
        log(LogLevel::Compile), inputFilePath(inputPath),
        outputFileStream(outputFileStream), targetType(targetType) {}

  Reference toRegister(
    Reference* value,
    std::ostream& outputFile,
    Environment& environment
  ) {
    if (value->isLiteral()) {
      return *value;
    } else if (auto lValue = value->lValue()) {
      auto type = value->getType();
      auto registerIndex = environment.makeTemporary(type);
      auto registerValue = Reference(registerIndex);
      fmt::println(
        outputFile,
        "{} = load {}, ptr {}",
        registerValue,
        LlvmName(type),
        *value
      );
      return Reference(registerIndex);
    } else if (auto recursive = std::get_if<Reference*>(&value->value)) {
      return toRegister(*recursive, outputFile, environment);
    } else {
      return *value;
      // auto message = fmt::format("Error converting value of type '{}' to Llvm
      // register", TypeName(value->getType()));
      // TODO(message);
    }
  }

  Reference toByValPointer(
    Reference value,
    std::ostream& outputFile,
    Environment& environment
  ) {
    auto loadedValue = std::get<RegisterValue>(
      toRegister(&value, outputFile, environment).value
    );
    auto stackPointer = Reference(environment.makeTemporary(loadedValue.type));

    auto sizing = Pool().getSizing(loadedValue.type);
    auto llvmType = LlvmName(loadedValue.type);
    fmt::println(
      outputFile,
      "{} = alloca {}, align {}",
      stackPointer,
      llvmType,
      sizing.alignment.byteAlignment()
    );
    fmt::println(
      outputFile,
      "store {} {}, ptr {}",
      llvmType,
      loadedValue,
      stackPointer
    );

    return stackPointer;
  }

  StackValue dereference(
    Reference* value,
    std::ostream& outputFile,
    Environment& environment,
    NodeIndex node
  ) {
    if (auto registerValue = value->unbox<RegisterValue>()) {
      OptionalType dereferencedType = Pool().dereference(registerValue->type);
      if (!dereferencedType.has_value()) {
        crash(
          node,
          "Unable to dereference non-pointer type '{}'",
          LlvmName(registerValue->type)
        );
      }
      return StackValue(
        registerValue->name,
        registerValue->type,
        registerValue->scope
      );
    } else if (auto lValue = value->lValue()) {
      OptionalType dereferencedType = Pool().dereference(lValue->type);
      if (!dereferencedType.has_value()) {
        crash(
          node,
          "Unable to dereference non-pointer type '{}'",
          LlvmName(registerValue->type)
        );
      }
      auto registerValue = std::get<RegisterValue>(
        toRegister(value, outputFile, environment).value
      );
      return StackValue(registerValue.name, *dereferencedType);
    } else {
      TODO("Dereferencing non-stack values?");
    }
  }

  fs::path concatPath(std::string_view path) {
    return fs::weakly_canonical(inputFilePath.parent_path().append(path));
  }

  Reference interpret(
    NodeIndex nodeIndex,
    Environment& environment,
    std::ostream& outputFile,
    StatementContext& context
  ) {
    if (environment.hasReturned) {
      crash(nodeIndex, "Statement found after all code paths have returned");
    }
    auto encoded = parser.getNode(nodeIndex);
    auto prefix = environment.prefix;
    switch (encoded.nodeType) {
    case NodeType::Block: {
      auto node = parser.getBlock(nodeIndex);
      auto token = parser.getToken(nodeIndex);
      switch (token->type) {
      case TokenType::LeftCurlyBrace: {
        Environment blockEnv(environment);
        blockEnv.nextTemporary = environment.nextTemporary;
        for (auto child : node.elements) {
          interpret(child, blockEnv, outputFile, context);
        }
        if (blockEnv.hasReturned) {
          environment.hasReturned = true;
        }
        environment.nextTemporary = blockEnv.nextTemporary;
        return Reference::Void();
      }
      case TokenType::LeftSquareBracket: {
        std::vector<Reference> elements;
        auto type = Pool().infer;
        bool canUseExpected = false;
        if (context.expectedType) {
          auto expected = context.expectedType.value();
          if (auto sizedArray = Pool().sizedArray(expected)) {
            type = sizedArray->dereferencedType;
            canUseExpected = true;
          }
        }

        StatementContext elementContext;
        if (!type.isInfer()) {
          elementContext.expectedType = type;
        }
        for (auto element : node.elements) {
          elements.push_back(
            interpret(element, environment, outputFile, elementContext)
          );
        }
        if (elements.empty()) {
          crash(nodeIndex, "Empty array literal");
        }

        u32 i = 0;
        for (auto element : elements) {
          if (auto elementType = Pool().coerce(element.getType(), type)) {
            type = *elementType;
          } else {
            crash(
              node.elements[i],
              "Unable to create array of both type {} and {}",
              TypeName(type),
              TypeName(element.getType())
            );
          }
          i++;
        }
        if (
          auto forceLiteralsToActualTypes =
            Pool().isAssignable(type, Pool().infer)
        ) {
          type = forceLiteralsToActualTypes.value();
        } else {
          crash(
            nodeIndex,
            "Unable to construct array of type {}",
            TypeName(type)
          );
        }
        auto resultType = canUseExpected
                            ? context.expectedType.value()
                            : Pool().sizedArrayOf(type, elements.size());
        auto resultArray = Reference(ZeroInit{});
        i = 0;
        for (auto element : elements) {
          auto loaded = toRegister(&element, outputFile, environment);
          if (auto floatType = Pool().getFloat(type)) {
            if (auto intLiteral = loaded.unbox<IntLiteral>()) {
              loaded.value =
                FloatLiteral(intLiteral->value, floatType->precision);
            }
          }
          auto newArray = environment.makeTemporary(resultType);
          fmt::println(
            outputFile,
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
      case TokenType::LeftParen: {
        crash(nodeIndex, "TODO: Tuples");
        // TODO: type inference
        std::vector<Reference> elements;
        for (auto element : node.elements) {
          elements.push_back(
            interpret(element, environment, outputFile, context)
          );
        }
        std::vector<TypeIndex> elementTypes;
        for (auto element : elements) {
          auto elementType = element.unboxType();
          if (!elementType) TODO("Value tuples");
          elementTypes.push_back(*elementType);
        }
        auto [typeIndex, _] = Pool().tupleOf(std::move(elementTypes));
        return Reference(typeIndex);
      }
      case TokenType::When: {
        StatementContext conditionContext(context);
        conditionContext.expectedType = std::nullopt;
        auto condition = interpret(
          node.elements[0],
          environment,
          outputFile,
          conditionContext
        );

        // Always odd number in encoded children, so floor division is fine
        auto caseNodes = std::bit_cast<span<pair<NodeIndex, NodeIndex>>>(
          node.elements.subspan(1, node.elements.size() / 2)
        );
        auto loadedCondition = toRegister(&condition, outputFile, environment);
        auto caseType = condition.getType();

        // TODO: Exhaustiveness checking
        if (auto enumType = Pool().getEnum(caseType)) {
        }

        vector<SwitchCase> cases;
        cases.reserve(caseNodes.size());
        StatementContext caseConditionContext{.expectedType = caseType};
        StatementContext caseBodyContext(context);
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
              .result =
                interpret(body, environment, instruction, caseBodyContext),
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
            .condition = interpret(
              condition,
              environment,
              outputFile,
              caseConditionContext
            ),
            .result =
              interpret(body, environment, instruction, caseBodyContext),
            .entryBlock = block,
            .exitBlock = environment.basicBlock,
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

        // Reserve one for loading stack values
        environment.addTemporary();
        auto endBlock = environment.addTemporary();
        auto defaultBlock = hasDefault ? cases.back().entryBlock : endBlock;

        // TODO: mix named and unnamed values
        // auto lValueResult =

        fmt::print(
          outputFile,
          "switch {} {}, label %{} [",
          LlvmName(caseType),
          loadedCondition,
          defaultBlock
        );
        for (auto& caseBlock : cases) {
          if (caseBlock.isDefault) break;
          fmt::print(
            outputFile,
            " {} {}, label %{}",
            LlvmName(caseType),
            caseBlock.condition,
            caseBlock.entryBlock
          );
        }
        outputFile << "]\n";

        bool allStackValues = true;
        bool hasResults = false;
        optional<TypeIndex> resultType = Pool().infer;
        for (auto& caseBlock : cases) {
          fmt::print(
            outputFile,
            "{}:\n{}",
            caseBlock.entryBlock,
            caseBlock.instructions.str()
          );
          if (!caseBlock.returns) {
            fmt::println(outputFile, "br label %{}", endBlock);
          }
          auto blockType = caseBlock.result.getType();
          if (resultType) {
            resultType = Pool().coerce(*resultType, blockType);
          }
          allStackValues =
            allStackValues && (caseBlock.returns || caseBlock.result.lValue());
          hasResults =
            hasResults || !(caseBlock.returns || Pool().isVoid(blockType));
        }

        fmt::println(outputFile, "{}:", endBlock);

        if (hasResults && resultType && !Pool().isVoid(*resultType)) {
          auto type = *resultType;
          auto resultRegister = environment.addTemporary();
          if (allStackValues) {
            fmt::println(outputFile, "%{} = phi ptr ", resultRegister);
          } else {
            fmt::println(
              outputFile,
              "%{} = phi {} ",
              resultRegister,
              LlvmName(type)
            );
          }

          bool hasMultiple = false;
          for (auto& caseBlock : cases) {
            if (caseBlock.returns) {
              continue;
            }
            if (hasMultiple) {
              outputFile << ", ";
            }
            hasMultiple = true;
            fmt::print(
              outputFile,
              "[{}, %{}]",
              caseBlock.result,
              caseBlock.exitBlock
            );
          }
          outputFile << '\n';

          return allStackValues
                   ? Reference(StackValue(resultRegister, type))
                   : Reference(RegisterValue(resultRegister, type));
        }

        return Reference::Void();
      }
      default:
        TODO("Default for block nodes");
      }
    }
    case NodeType::Declaration: {
      auto node = parser.getDeclaration(nodeIndex);
      // NOTE: don't support using non-identifiers
      bool compileTime =
        parser.getToken(encoded.token)->type == TokenType::Colon;

      auto definition = parser.getDefinition(node.definition);
      auto definitionName = definition.name->lexeme;
      if (environment.isDefined(definitionName)) {
        crash(nodeIndex, "Attempt to redefine name '{}'", definitionName);
      }

      if (compileTime) {
        if (definition.type.has_value()) {
          TODO("Explicitly-typed comptime constants");
        }
        auto valueContext = StatementContext(context);
        valueContext.name = definitionName;
        auto value =
          interpret(node.value, environment, outputFile, valueContext);

        if (!value.isComptime()) {
          crash(
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
        return Reference(value);
      } else {
        Reference definition =
          interpret(node.definition, environment, outputFile, context);

        if (
          StackValue* assignee = std::get_if<StackValue>(
            &std::get<Reference*>(definition.value)->value
          )
        ) {
          TypeIndex expectedType = assignee->type;
          StatementContext valueContext{
            .name = std::get<Identifier>(assignee->name),
            .expectedType = assignee->type
          };
          log(
            "Making value: '{}: {}'",
            assignee->name,
            TypeName(assignee->type)
          );
          auto value =
            interpret(node.value, environment, outputFile, valueContext);
          auto assignedType = value.isAssignableTo(expectedType);
          if (!assignedType) {
            crash(
              nodeIndex,
              "Unable to assign value of type '{}' to to variable '{}' of type "
              "'{}'",
              TypeName(value.getType()),
              assignee->name,
              TypeName(assignee->type)
            );
          }
          assignee->type = *assignedType;
          auto byteAlignment =
            Pool().getSizing(*assignedType).alignment.byteAlignment();
          switch (assignee->scope) {
          case ValueScope::Local: {
            fmt::println(
              outputFile,
              "{} = alloca {}, align {}",
              definition,
              LlvmName(*assignedType),
              byteAlignment
            );
            break;
          }
          case ValueScope::Global: {
            globalsStack.push(
              fmt::format(
                "{} = global {} undef align {}",
                definition,
                LlvmName(*assignedType),
                byteAlignment
              )
            );
            break;
          }
          }

          auto loaded = toRegister(&value, outputFile, environment);
          fmt::println(
            outputFile,
            "store {} {}, ptr {}",
            LlvmName(*assignedType),
            loaded,
            definition
          );
        } else {
          crash(
            nodeIndex,
            "Internal compiler error: definition didn't result in stack or "
            "global value"
          );
        }
      }

      // TODO: support assignment as expression???
      return Reference::Void();
    }
    case NodeType::Definition: {
      auto node = parser.getDefinition(nodeIndex);
      auto name = node.name->lexeme;
      TypeIndex type = Pool().infer;
      if (node.type.has_value()) {
        StatementContext ctx(context);
        ctx.expectedType = std::nullopt;
        if (
          auto typeIndex =
            interpret(node.type.value(), environment, outputFile, ctx)
              .unboxType()
        ) {
          type = *typeIndex;
        } else {
          crash(
            nodeIndex,
            "Type for identifier '{}' is not a type",
            node.name->lexeme
          );
        }
      }
      std::optional<Reference*> definition = environment.define(
        name,
        Reference(StackValue(
          node.name->lexeme,
          type,
          environment.envType == EnvType::Global ? ValueScope::Global
                                                 : ValueScope::Local
        ))
      );
      if (definition) {
        return Reference(*definition);
      }

      crash(
        nodeIndex,
        "Definition for identifier '{}' already exists",
        node.name->lexeme
      );
    }
    case NodeType::Literal: {
      auto node = parser.getLiteral(nodeIndex);
      auto token = node.token;
      auto u8Type = Pool()._u8;
      if (token->type == TokenType::Char) {
        return Reference(IntLiteral(token->lexeme[0], u8Type));
      }
      if (token->type == TokenType::String) {
        auto global = environment.makeGlobal(u8Type);
        auto [stringValue, length] = escapeSourceString(token->lexeme, token);
        // TODO: use string types instead of C strings
        std::stringstream instruction;
        instruction << fmt::format(
          "{} = global [{} x i8] c\"{}\" align 1\n",
          global,
          length,
          stringValue
        );
        globalsStack.push(instruction.str());
        auto lengthValue = Reference(IntLiteral(length));
        auto ref = Reference(global);
        return makeSlice(ref, lengthValue, outputFile, environment);
      }
      if (token->type == TokenType::NullTerminatedString) {
        auto [stringValue, length] = escapeSourceString(token->lexeme, token);
        auto global = environment.makeGlobal(Pool()._u8);
        // auto global = environment.makeGlobal(Pool().sizedArrayOf(Pool()._u8,
        // length));
        static std::string nullByte = "\\00";
        std::stringstream instruction;
        instruction << fmt::format(
          "{} = global [{} x i8] c\"{}{}\" align 1\n",
          global,
          length + 1,
          stringValue,
          nullByte
        );
        globalsStack.push(instruction.str());
        return Reference(RegisterValue(
          global.name,
          Pool().pointerTo(global.type),
          global.scope
        ));
      }
      if (token->type == TokenType::Decimal) {
        float floatValue = std::stof(token->lexeme.data());
        return Reference(FloatLiteral(floatValue));
      }
      if (token->type == TokenType::Integer) {
        int64_t intVal = std::stoi(token->lexeme.data());
        return Reference(IntLiteral(intVal));
      }
      if (token->type == TokenType::HexInt) {
        int64_t intVal = std::stoi(token->lexeme.data(), 0, 16);
        return Reference(IntLiteral(intVal));
      }
      if (token->type == TokenType::True) {
        return Reference(true);
      }
      if (token->type == TokenType::False) {
        return Reference(false);
      }
      if (token->type == TokenType::Identifier) {
        auto name = node.token->lexeme;
        if (auto value = environment.find(name)) {
          return Reference(*value);
        }
        environment.debug();
        crash(nodeIndex, "Identifier \"{}\" not defined", name);
      }
      if (token->type == TokenType::Opaque) {
        TypeIndex type = Pool().addOpaque(
          std::string(context.name.value_or("Anonymous Opaque"))
        );
        return Reference(type);
      }
      if (token->type == TokenType::Self) {
        if (context.selfType) {
          return Reference(context.selfType.value());
        }
        crash(nodeIndex, "No type 'Self' in context");
      }
      if (token->type == TokenType::Undef) {
        return Reference(Never{});
      }

      static std::unordered_map<TokenType, std::string_view> cudaBuiltins{
        {TokenType::CudaThreadIdxX, "@llvm.nvvm.read.ptx.sreg.tid.x"   },
        {TokenType::CudaThreadIdxY, "@llvm.nvvm.read.ptx.sreg.tid.y"   },
        {TokenType::CudaThreadIdxZ, "@llvm.nvvm.read.ptx.sreg.tid.z"   },
        {TokenType::CudaBlockIdxX,  "@llvm.nvvm.read.ptx.sreg.ctaid.x" },
        {TokenType::CudaBlockIdxY,  "@llvm.nvvm.read.ptx.sreg.ctaid.y" },
        {TokenType::CudaBlockIdxZ,  "@llvm.nvvm.read.ptx.sreg.ctaid.z" },
        {TokenType::CudaBlockDimX,  "@llvm.nvvm.read.ptx.sreg.ntid.x"  },
        {TokenType::CudaBlockDimY,  "@llvm.nvvm.read.ptx.sreg.ntid.y"  },
        {TokenType::CudaBlockDimZ,  "@llvm.nvvm.read.ptx.sreg.ntid.z"  },
        {TokenType::CudaGridDimX,   "@llvm.nvvm.read.ptx.sreg.nctaid.x"},
        {TokenType::CudaGridDimY,   "@llvm.nvvm.read.ptx.sreg.nctaid.y"},
        {TokenType::CudaGridDimZ,   "@llvm.nvvm.read.ptx.sreg.nctaid.z"},
      };

      if (cudaBuiltins.contains(token->type)) {
        if (targetType != TargetType::Gpu) {
          crash(
            nodeIndex,
            "Unable to use cuda builtin '@{}' in non-gpu target",
            token->lexeme
          );
        }
        auto result = environment.makeTemporary(Pool()._u32);
        fmt::println(
          outputFile,
          "{} = call i32 {}",
          result,
          cudaBuiltins[token->type]
        );
        return Reference(result);
      }
      crash(nodeIndex, "Unable to create literal from value");
    }
    case NodeType::Assignment: {
      auto node = parser.getNode(nodeIndex);
      NodeIndex assigneeNode(node.left), valueNode{node.right};
      TokenPointer token = parser.getToken(node.token);
      if (token->isArithmeticOperation()) {
        auto rValue = parser.addNode(
          Encodings::BinaryOp{
            .left = assigneeNode,
            .right = valueNode,
            .operation = token
          }
        );
        fmt::println("Operator: {}", token->lexeme);
        auto node = parser.addAssignment(assigneeNode, rValue, &token[1]);
        fmt::println("Equals: {}", token[1].lexeme);
        return interpret(node, environment, outputFile, context);
      } else if (token->type != TokenType::Assign) {
        crash(token, "Unknown compound assignment operator");
      }

      auto assignee = interpret(assigneeNode, environment, outputFile, context);
      StatementContext valueContext(context);
      valueContext.expectedType = assignee.getType();
      auto value = interpret(valueNode, environment, outputFile, valueContext);

      Reference lValue;
      TypeIndex leftType;
      if (auto stackValue = assignee.lValue()) {
        lValue.value = stackValue.value();
        leftType = stackValue.value().type;
      } else {
        crash(nodeIndex, "Can't assign to literal");
      }
      OptionalType valueType = value.isAssignableTo(leftType);
      if (!valueType) {
        crash(
          nodeIndex,
          "Can't assign value of type {} to symbol of type {}",
          TypeName(value.getType()),
          TypeName(leftType)
        );
      }
      if (Pool().isFloat(*valueType)) {
        if (auto literal = value.unbox<IntLiteral>()) {
          value.value = FloatLiteral(literal->value);
        }
      }
      auto loaded = toRegister(&value, outputFile, environment);
      fmt::println(
        outputFile,
        "store {} {}, ptr {}",
        LlvmName(*valueType),
        loaded,
        assignee
      );

      // TODO: consider value
      return Reference::Void();
    }
    case NodeType::BinaryOp: {
      auto node = parser.getBinaryOp(nodeIndex);
      auto opType = node.operation->type;
      auto leftVal = interpret(node.left, environment, outputFile, context);

      if (node.operation->isArithmeticOperation()) {
        return arithmeticOperation(
          opType,
          node,
          leftVal,
          nodeIndex,
          environment,
          outputFile
        );
      }

      auto leftType = leftVal.getType();

      switch (opType) {
      case TokenType::While: {
        auto loopId = environment.addTemporary();
        auto loopHeader = environment.addLabel("while", loopId);
        auto loopBody = loopHeader + ".continue";
        auto endLabel = loopHeader + ".break";
        fmt::println(outputFile, "br label %{}\n{}:", loopHeader, loopHeader);
        auto condition = interpret(node.left, environment, outputFile, context);
        auto conditionLiteral = toRegister(&condition, outputFile, environment);
        fmt::println(
          outputFile,
          "br i1 {}, label %{}, label %{}\n{}:",
          conditionLiteral,
          loopBody,
          endLabel,
          loopBody
        );
        if (condition.getType() != Pool()._bool) {
          crash(
            nodeIndex,
            "Condition for while loop must be of type 'bool', "
            "but was of type '{}'",
            TypeName(condition.getType())
          );
        }

        interpret(node.right, environment, outputFile, context);

        fmt::println(outputFile, "br label %{}\n{}:", loopHeader, endLabel);
        // TODO: consider value expression (see
        // https://ziglang.org/documentation/master/#while)
        return Reference::Void();
      }

      case TokenType::LeftSquareBracket: {
        // [left]right
        // ^
        if (parser.nodeTokenPrecedes(nodeIndex, node.left)) {
          // TODO: comptime evaluation
          auto size = interpret(node.left, environment, outputFile, context);
          // auto sizeNode = parser.getNode(node.left);
          // auto sizeToken = parser.getToken(sizeNode.token);
          auto sizeLiteral = size.unbox<IntLiteral>();
          if (!sizeLiteral) {
            crash(node.left, "Array size must be a compile-time known integer");
          }
          if (sizeLiteral->value <= 0) {
            crash(
              node.left,
              "Array size must be a positive int, but was {}",
              sizeLiteral->value
            );
          }

          auto elementType =
            interpret(node.right, environment, outputFile, context);
          if (auto type = elementType.unboxType()) {
            return Reference(
              Pool().sizedArrayOf(*type, (u32)sizeLiteral->value)
            );
          }

          crash(
            node.right,
            "Array element type must be a compile-time known type, but was of "
            "type '{}'",
            TypeName(elementType.getType())
          );
        }

        // left[right]
        //     ^

        // Generic instantiation
        if (auto boxedGeneric = std::get_if<GenericValue>(&leftVal.value)) {
          auto arguments = parser.getArgumentList(node.right);
          auto expectedArgLength = boxedGeneric->parameterNames.size();
          {
            auto actualArgLength = arguments.requiredArgs.size();
            if (expectedArgLength != actualArgLength)
              crash(
                node.right,
                "Expected {} generic arguments, but {} were provided",
                expectedArgLength,
                actualArgLength
              );
          }
          Environment genericEnvironment(
            &boxedGeneric->definitionEnvironment,
            std::string(""),
            true
          );
          std::vector<TypeIndex> argTypes(expectedArgLength);

          // TODO: optional inputs
          for (auto i = 0; i < boxedGeneric->parameterNames.size(); i++) {
            auto argNode = arguments.requiredArgs[i];
            auto argValue =
              interpret(argNode, environment, outputFile, context);

            auto paramName = boxedGeneric->parameterNames[i];
            if (auto typeIndex = argValue.unboxType()) {
              genericEnvironment.define(paramName, argValue);
              argTypes.push_back(*typeIndex);
            } else {
              TODO("Non-type generic parameters");
            }
          }

          for (auto param : arguments.optionalArgs) {
            Todo(parser.getToken(param.token), "Named generic parameters");
          }

          auto [tupleType, tupleIndex] =
            TypePool().tupleOf(std::move(argTypes));

          if (boxedGeneric->cache.contains(tupleIndex)) {
            return Reference(boxedGeneric->cache[tupleIndex]);
          } else {
            auto tupleName = fmt::format("{}", TypeName(tupleType));
            std::string genericName =
              fmt::format("{}{}", boxedGeneric->name, tupleName);
            genericName[genericEnvironment.prefix.size() - 1] = ']';
            genericName[genericEnvironment.prefix.size() - tupleName.size()] =
              '[';

            StatementContext genericContext = {
              .name = genericName,
              .expectedType = context.expectedType
            };
            // Match source syntax when debugging if possible
            genericEnvironment.prefix =
              boxedGeneric->definitionEnvironment.prefix + genericName;
            boxedGeneric->translationUnit.interpret(
              boxedGeneric->astNode,
              genericEnvironment,
              outputFile,
              genericContext
            );
            auto genericValue = interpret(
              boxedGeneric->astNode,
              genericEnvironment,
              outputFile,
              genericContext
            );
            auto cached = new Reference(genericValue);
            boxedGeneric->cache[tupleIndex] = cached;
            return Reference(cached);
          }
        }

        // Bracket access
        auto rightVal = interpret(node.right, environment, outputFile, context);
        auto rightType = rightVal.getType();

        // TODO: pointers to slice?
        if (auto dereffedType = Pool().unboxReference(leftType)) {
          auto ptr = toRegister(&leftVal, outputFile, environment);
          // auto loaded = StackValue(environment.addTemporary(),
          // *dereffedType);
          auto loaded =
            StackValue(std::get<RegisterValue>(ptr.value).name, *dereffedType);
          // fmt::println(outputFile, "{} = load ptr, ptr {}", loaded, ptr);
          leftVal.value = loaded;
          leftType = *dereffedType;
        }
        if (auto sliceElement = Pool().sliceElementType(leftType)) {
          TypeIndex elementType = *sliceElement;
          auto leftLiteral = toRegister(&leftVal, outputFile, environment);
          auto [dataPointer, length] = getSliceElements(
            std::get<RegisterValue>(leftLiteral.value),
            environment,
            outputFile
          );
          auto lengthBound = RangeBound(length);
          auto dataPointerRef = Reference(dataPointer);

          if (Pool().isInt(rightType)) {
            auto index = toRegister(&rightVal, outputFile, environment);
            auto result = StackValue(environment.addTemporary(), elementType);

            if (Pool().isSignedInt(rightType)) {
              guardLowerBound(index, environment, outputFile);
            }
            guardIndexInBounds(index, lengthBound, environment, outputFile);
            fmt::println(
              outputFile,
              "{} = getelementptr {}, ptr {}, {} {}",
              result,
              LlvmName(elementType),
              leftLiteral,
              LlvmName(rightType),
              index
            );
            return Reference(result);
          } else if (auto range = rightVal.unbox<Range>()) {
            return sliceRange(
              *range,
              node.right,
              outputFile,
              environment,
              length,
              dataPointerRef
            );
          }
          crash(
            node.right,
            "Index must be an integer or range, but was of type '{}'",
            TypeName(rightType)
          );
        } else if (auto elementType = Pool().multiPointerElement(leftType)) {
          auto dataPointer = toRegister(&leftVal, outputFile, environment);
          if (Pool().isInt(rightType)) {
            auto rightLiteral = toRegister(&rightVal, outputFile, environment);
            auto result =
              Reference(StackValue(environment.addTemporary(), *elementType));
            fmt::println(
              outputFile,
              "{} = getelementptr ptr, ptr {}, {} {}",
              result,
              dataPointer,
              LlvmName(rightType),
              rightLiteral
            );
            return result;
          } else if (auto range = rightVal.unbox<Range>()) {
            return sliceRange(
              *range,
              node.right,
              outputFile,
              environment,
              std::nullopt,
              dataPointer
            );
          }
          crash(
            node.right,
            "Index must be an integer or range, but was of type '{}'",
            TypeName(rightType)
          );
        } else if (auto sizedArray = Pool().sizedArray(leftType)) {
          auto length = IntLiteral(sizedArray->length);
          auto elementType = sizedArray->dereferencedType;
          if (!leftVal.lValue()) {
            TODO("Indexing sized array not in stack value");
          }

          if (Pool().isInt(rightType)) {
            auto index = toRegister(&rightVal, outputFile, environment);
            if (Pool().isSignedInt(rightType)) {
              guardLowerBound(index, environment, outputFile);
            }
            auto lengthRef = RangeBound(length);
            guardIndexInBounds(index, lengthRef, environment, outputFile);
            auto result = StackValue(environment.addTemporary(), elementType);
            fmt::println(
              outputFile,
              "{} = getelementptr {}, ptr {}, {} {}",
              result,
              LlvmName(elementType),
              leftVal,
              LlvmName(rightType),
              index
            );
            return Reference(result);
          } else if (auto range = rightVal.unbox<Range>()) {
            Reference dataPointer;
            if (auto stackVal = leftVal.unbox<StackValue>()) {
              dataPointer.value = StackValue(stackVal->name, elementType);
            } else {
              crash(nodeIndex, "Compiler state failure when slicing array");
            }
            return sliceRange(
              *range,
              node.right,
              outputFile,
              environment,
              length,
              dataPointer
            );
          }
          crash(
            node.right,
            "Index must be an integer or range, but was of type '{}'",
            TypeName(rightType)
          );
        } else crashBinOp(node.operation, &leftVal, &rightVal);
        break;
      }
      case TokenType::LeftParen: {
        auto function = interpret(node.left, environment, outputFile, context);
        auto argsNode = parser.getArgumentList(node.right);

        if (auto func = function.unboxFunction()) {
          return callFunction(
            func,
            argsNode,
            nodeIndex,
            environment,
            outputFile,
            nullptr
          );
        } else if (auto bound = function.unbox<BoundFunction>()) {
          auto ref = Reference(bound->self);
          return callFunction(
            &bound->method,
            argsNode,
            nodeIndex,
            environment,
            outputFile,
            &ref
          );
        } else if (auto type = function.unboxType()) {
          if (auto structDefinition = Pool().getStruct(*type)) {
            return constructStruct(
              nodeIndex,
              *type,
              argsNode,
              environment,
              outputFile
            );
          } else {
            crash(
              nodeIndex,
              "Can't construct non-struct type {}",
              TypeName(*type)
            );
          }
        } else {
          crash(
            nodeIndex,
            "Unable to call value of type '{}' as a function",
            TypeName(function.getType())
          );
        }
      }
      case TokenType::ExclusiveRange: {
        auto lowerBoundIndex = parser.readOptional(node.left.value);
        auto upperBoundIndex = parser.readOptional(node.right.value);
        if (!(lowerBoundIndex || upperBoundIndex)) {
          crash(
            nodeIndex,
            "At least one of upper and lower bound on a range must be set"
          );
        }
        RangeBound lowerBound = IntLiteral(0);
        TypeIndex lowerType = Pool().intLiteral;
        if (lowerBoundIndex) {
          auto result = interpret(
            lowerBoundIndex.value(),
            environment,
            outputFile,
            context
          );
          lowerType = result.getType();
          if (!Pool().isInt(lowerType)) {
            crash(
              node.left,
              "Provided range bounds must be of an integer type, but were {}",
              TypeName(lowerType)
            );
          }
          lowerBound =
            toRegister(&result, outputFile, environment).rangeBound();
        }
        std::optional<RangeBound> upperBound = std::nullopt;
        if (upperBoundIndex) {
          auto result = interpret(
            upperBoundIndex.value(),
            environment,
            outputFile,
            context
          );
          auto upperType = result.getType();
          if (!Pool().isInt(upperType)) {
            crash(
              node.left,
              "Provided range bounds must be of an integer type, but were {}",
              TypeName(upperType)
            );
          }
          auto rangeType = Pool().coerce(lowerType, upperType);
          if (!rangeType) {
            crash(
              node.right,
              "Range bounds must be of the same type, but were {} and {}",
              TypeName(lowerType),
              TypeName(upperType)
            );
          }
          upperBound =
            toRegister(&result, outputFile, environment).rangeBound();
        }
        return Reference(Range(lowerBound, upperBound));
      }
      case TokenType::LogicOr: {
        auto orId = environment.addTemporary();
        auto labelStart = environment.addLabel("or", orId);
        auto labelFalse = labelStart + ".false";
        auto labelEnd = labelStart + ".true";

        fmt::println(outputFile, "br label %{}\n{}:", labelStart, labelStart);
        auto leftVal = interpret(node.left, environment, outputFile, context);
        auto leftRegister = toRegister(&leftVal, outputFile, environment);

        if (auto type = leftVal.getType(); type != Pool()._bool) {
          crash(
            node.left,
            "Operands to logical 'or' need to be of type bool. Left operand "
            "was of type '{}'",
            TypeName(type)
          );
        }

        if (leftVal.isComptime()) {
          // TODO
        }

        fmt::println(
          outputFile,
          "br i1 {}, label %{}, label %{}\n{}:",
          leftRegister,
          labelEnd,
          labelFalse,
          labelFalse
        );

        auto rightVal = interpret(node.right, environment, outputFile, context);
        auto rightRegister = toRegister(&rightVal, outputFile, environment);

        if (auto type = rightVal.getType(); type != Pool()._bool) {
          crash(
            node.right,
            "Operands to logical 'or' need to be of type bool. Right operand "
            "was of type '{}'",
            TypeName(type)
          );
        }

        fmt::println(outputFile, "br label %{}\n{}:", labelEnd, labelEnd);
        auto result = environment.makeTemporary(Pool()._bool);
        fmt::println(
          outputFile,
          "{} = phi i1 [{}, %{}], [{}, %{}]",
          result,
          leftRegister,
          labelStart,
          rightRegister,
          labelFalse
        );
        return Reference(result);
      }
      case TokenType::LogicAnd: {
        auto orId = environment.addTemporary();
        auto labelStart = environment.addLabel("and", orId);
        auto labelTrue = labelStart + ".true";
        auto labelFalse = labelStart + ".false";

        fmt::println(outputFile, "br label %{}\n{}:", labelStart, labelStart);
        auto leftVal = interpret(node.left, environment, outputFile, context);
        auto leftRegister = toRegister(&leftVal, outputFile, environment);

        if (auto type = leftVal.getType(); type != Pool()._bool) {
          crash(
            node.left,
            "Operands to logical 'or' need to be of type bool. Left operand "
            "was of type '{}'",
            TypeName(type)
          );
        }

        if (leftVal.isComptime()) {
          // TODO
        }

        fmt::println(
          outputFile,
          "br i1 {}, label %{}, label %{}\n{}:",
          leftRegister,
          labelTrue,
          labelTrue,
          labelFalse
        );

        auto rightVal = interpret(node.right, environment, outputFile, context);
        auto rightRegister = toRegister(&rightVal, outputFile, environment);

        if (auto type = rightVal.getType(); type != Pool()._bool) {
          crash(
            node.right,
            "Operands to logical 'or' need to be of type bool. Right operand "
            "was of type '{}'",
            TypeName(type)
          );
        }

        fmt::println(outputFile, "br label %{}\n{}:", labelFalse, labelFalse);
        auto result = environment.makeTemporary(Pool()._bool);
        fmt::println(
          outputFile,
          "{} = phi i1 [{}, {}], [{}, {}]",
          result,
          leftRegister,
          labelStart,
          rightRegister,
          labelTrue
        );
        return Reference(result);
      }
      case TokenType::BUILTIN_Align: {
        auto boxedAlignment =
          interpret(node.left, environment, outputFile, context);
        auto boxedType =
          interpret(node.right, environment, outputFile, context);

        u64 byteAlign;
        if (auto intLit = boxedAlignment.unbox<IntLiteral>()) {
          byteAlign = intLit->value;
          if ((byteAlign & (byteAlign - 1)) != 0) {
            crash(
              node.left,
              "Alignment value must be a power-of-two, but was {}",
              byteAlign
            );
          }
        } else {
          crash(
            node.left,
            "Alignment argument needs to be a compile-time known integer"
          );
        }

        TypeIndex baseTypeIndex;
        if (auto typeIndex = boxedType.unboxType()) {
          baseTypeIndex = *typeIndex;
        } else {
          crash(
            node.right,
            "Type argument for @align must be a compile-time known type"
          );
        }
        auto typeIndex = Pool().alignType(
          baseTypeIndex,
          Log2Alignment::fromByteSize(byteAlign)
        );
        return Reference(typeIndex);
      }
      case TokenType::Impl: {
        TypeIndex targetType;
        auto boxedType = interpret(node.left, environment, outputFile, context);
        if (auto type = boxedType.unboxType()) {
          targetType = *type;
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
        auto block = parser.getBlock(node.right);
        auto envPrefix = fmt::format("{}", TypeName(targetType));
        Environment implEnv(&environment, envPrefix);
        implEnv.envType = EnvType::Global;
        StatementContext implContext{.selfType = targetType};
        for (auto index : block.elements) {
          auto declaration = parser.getDeclaration(index);
          auto nameToken = parser.getDefinition(declaration.definition).name;
          auto name = nameToken->lexeme;
          implContext.name = name;
          auto value =
            interpret(declaration.value, implEnv, outputFile, implContext);
          log("impl {}.{} = {}", TypeName(targetType), name, value);
          if (implEnv.defs.contains(name))
            crash(nameToken, "Duplicate member in impl block '{}'", name);
          if (
            parser.getToken(index)->type != TokenType::Colon ||
            !value.isComptime()
          )
            crash(index, "TODO: non-comptime values");
          implEnv.define(name, value);
        }

        // TODO: figure out if we can do a move here
        environment.impls.witnesses[targetType] = std::move(implEnv.defs);
        if (log.logLevel & log.globalLevels) {
          for (auto& [name, value] : environment.impls.witnesses[targetType]) {
            log("impl {}.{} = {}", TypeName(targetType), name, value);
          }
        }

        return Reference(targetType);
      }
      default:
        crash(nodeIndex, "Unknown binary operation {}", node.operation->lexeme);
      }
    }
    case NodeType::FunctionLiteral: {
      auto node = parser.getFunctionLiteral(nodeIndex);
      TypeIndex returnType = Pool()._void;
      if (node.returnType.has_value()) {
        StatementContext context;
        auto boxedReturnType =
          interpret(node.returnType.value(), environment, outputFile, context)
            .unboxType();
        if (!boxedReturnType) {
          crash(
            nodeIndex,
            "Return type of function must be a compile-time known type"
          );
        }
        returnType = *boxedReturnType;
      }

      std::string_view llvmName;
      bool forwardDeclare = !node.body.has_value();
      auto parameters = parser.getParameterList(node.parameters);
      std::vector<TypeIndex> parameterTypes;
      for (NodeIndex parameterIndex : parameters.requiredParameters) {
        auto parameterDefinition = parser.getDefinition(parameterIndex);
        if (!parameterDefinition.type.has_value()) {
          crash(parameterIndex, "Parameters must have a type");
        }

        auto parameterType =
          interpret(*parameterDefinition.type, environment, outputFile, context)
            .unboxType();
        if (!parameterType) {
          crash(
            parameterIndex,
            "Parameter type must be a compile time-known type"
          );
        }

        parameterTypes.push_back(*parameterType);
      }
      auto [_, tupleType] = Pool().tupleOf(std::move(parameterTypes));
      auto functionType = FunctionType(tupleType, returnType);

      // auto token = parser.getTokenIndex()
      auto token = parser.getToken(nodeIndex);
      token++;
      if (token->type == TokenType::String) {
        llvmName = token->lexeme;
      } else if (context.name.has_value()) {
        prefix = context.name.value();
        llvmName = StringPool::inst().copy(
          // TODO: figure out anonymous function naming here
          forwardDeclare ? prefix : environment.addConstant(prefix)
        );
      } else {
        if (forwardDeclare) {
          crash(
            nodeIndex,
            "Can't forward declare anoymnous function; Anonymous functions "
            "require a body"
          );
        }
        llvmName = StringPool::inst().copy(
          fmt::format("{}{}", environment.prefix, environment.nextGlobalIndex())
        );
      }

      if (!forwardDeclare) {
        functionStubs.push_back(
          {.name = llvmName,
           .definitionNode = nodeIndex,
           .functionType = functionType}
        );
      } else {
        functionType.forwardDeclare(llvmName, globalsStack);
      }

      return Reference(Function(functionType, llvmName));
    }
    case NodeType::Unary: {
      auto node = parser.getUnary(nodeIndex);

      switch (node.operation) {
      case UnaryOps::CompilerBuiltin:
        if (node.operation == UnaryOps::CompilerBuiltin) {
          auto builtinToken = parser.getToken(parser.getNode(nodeIndex).token);
          auto argList = parser.getArgumentList(node.operand);
          auto argumentNodes = argList.requiredArgs;
          auto namedArgs = argList.optionalArgs;
          switch (builtinToken->type) {
          case TokenType::BUILTIN_NumCast: {
            auto arguments =
              argumentNodes |
              std::views::transform(
                [this, &environment, &outputFile, &context](const NodeIndex x) {
                  return interpret(x, environment, outputFile, context);
                }
              );
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
                targetType = *type;
              } else {
                crash(
                  argumentNodes[1],
                  "Second arguments for builtin extend needs to be a "
                  "compile-time known type"
                );
              }
            } else if (context.expectedType) {
              targetType = context.expectedType.value();
            } else {
              crash(
                nodeIndex,
                "Builtin @numCast must either take a second argument for the "
                "target type, or have an inferrable target"
              );
            }

            auto objectLiteral = toRegister(&object, outputFile, environment);

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
              Pool().isUnsignedInt(objectType) &&
              Pool().isUnsignedInt(targetType)
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
            fmt::println(
              outputFile,
              "{} = {}{} {} {} to {}",
              resultName,
              typePrefix,
              instructionName,
              LlvmName(objectType),
              objectLiteral,
              LlvmName(targetType)
            );

            return resultName;
            break;
          }
          case TokenType::BUILITN_BitCast: {
            auto arguments =
              argumentNodes |
              std::views::transform(
                [this, &environment, &outputFile, &context](const NodeIndex x) {
                  return interpret(x, environment, outputFile, context);
                }
              );
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
                targetType = *type;
              } else {
                crash(
                  argumentNodes[1],
                  "Second arguments for builtin extend needs to be a "
                  "compile-time known type"
                );
              }
            } else if (context.expectedType) {
              targetType = context.expectedType.value();
            } else {
              crash(
                nodeIndex,
                "Builtin @numCast must either take a second argument for the "
                "target type, or have an inferrable target"
              );
            }

            auto objectLiteral = toRegister(&object, outputFile, environment);

            auto objectType = arguments[0].getType();

            Reference resultName(environment.makeTemporary(targetType));
            fmt::println(
              outputFile,
              "{} = bitcast {} {} to {}",
              resultName,
              LlvmName(objectType),
              objectLiteral,
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
            StatementContext typeContext;
            for (auto [name, value] : namedArgs) {
              auto valueType =
                interpret(value, environment, outputFile, typeContext);
              if (auto type = valueType.unboxType()) {
                definedTypes[parser.getToken(name)->lexeme] = *type;
              } else {
                TODO("Error for passing non-type into types");
              }
            }
            CompilerContext::inst().c.clangArgs.push_back("-include");
            CompilerContext::inst().c.clangArgs.push_back(std::move(fileName));

            auto prefix =
              std::string(parser.getToken(argumentNodes[1])->lexeme);
            return Reference(cBindings(
              std::move(includeFile),
              prefix,
              globalsStack,
              definedTypes
            ));
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
              arg = fmt::format(
                "{}={}",
                arg,
                parser.getToken(argumentNodes[1])->lexeme
              );
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
              crash(
                nodeIndex,
                "Builtin '@cInclude' must take one literal argument"
              );
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
              crash(
                nodeIndex,
                "Builtin '@link' must take one literal argument"
              );
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
              crash(
                nodeIndex,
                "Builtin '@linkDir' must take one literal argument"
              );
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
            auto arg =
              interpret(argumentNodes[0], environment, outputFile, context);
            return Reference(arg.getType());
          }
          default: {
            crash(nodeIndex, "Malformed builtin '@{}'", builtinToken->lexeme);
          }
          }
        }
      case UnaryOps::Import: {
        auto fileName = parser.getToken(TokenIndex{node.operand.value})->lexeme;
        auto filePath = inputFilePath.parent_path().append(fileName);
        fmt::println("Importing: {}", filePath.string());
        Environment* import = compile(filePath, outputFileStream, targetType);
        if (!import->impls.witnesses.empty()) {
          environment.importedImpls.push_back(&import->impls);
        }
        return Reference(import);
      }
      case UnaryOps::Dereference: {
        auto value = interpret(node.operand, environment, outputFile, context);
        auto type = value.getType();
        if (Pool().isPointer(type) || Pool().multiPointerElement(type)) {
          return Reference(
            dereference(&value, outputFile, environment, nodeIndex)
          );
        }
        crash(
          node.operand,
          "Can't dereference non-pointer type {}",
          TypeName(type)
        );
      }
      case UnaryOps::Reference: {
        auto value = interpret(node.operand, environment, outputFile, context);
        if (auto type = value.unboxType()) {
          return Reference(Pool().pointerTo(*type));
        } else if (auto lValue = value.lValue()) {
          auto address = Reference(RegisterValue(
            lValue->name,
            Pool().pointerTo(lValue->type),
            lValue->scope
          ));
          parser.locationOf(node.operand).underline(std::cout);
          return address;
        } else {
          crash(
            node.operand,
            "Unable to make reference to non-stack value or type"
          );
        }
      }
      case UnaryOps::Not: {
        auto value = interpret(node.operand, environment, outputFile, context);
        if (auto boolean = value.unboxBool()) {
          return Reference(!boolean);
        }
        auto type = Pool()._bool;
        if (value.getType() != type) {
          crash(
            node.operand,
            "Unary not operator '!' can only be used on boolean types; Operand "
            "was of type '{}'",
            TypeName(type)
          );
        }
        auto valueName = toRegister(&value, outputFile, environment);
        auto resultName = Reference(environment.makeTemporary(type));
        fmt::println(outputFile, "{} = not i1 {}", resultName, valueName);
        return Reference(resultName);
      }
      case UnaryOps::SliceType: {
        auto value = interpret(node.operand, environment, outputFile, context);
        auto elementType = value.unboxType();
        if (!elementType.has_value()) {
          crash(
            node.operand,
            "Element type for array type (slice, fixed-size, etc) must be a "
            "compile-time known type, but was a '{}'",
            TypeName(value.getType())
          );
        }
        return Reference(Pool().sliceOf(*elementType));
      }
      case UnaryOps::MultiPointerTo: {
        auto value = interpret(node.operand, environment, outputFile, context);
        auto elementType = value.unboxType();
        if (elementType.has_value()) {
          return Reference(Pool().multiPointerTo(*elementType));
        } else if (auto stackVal = value.unbox<StackValue>()) {
          auto type = stackVal->type;
          return Reference(
            StackValue(stackVal->name, Pool().multiPointerTo(type))
          );
          // TODO: take from global?
        } else {
          crash(
            node.operand,
            "Element type for array type (slice, fixed-size, etc) must be a "
            "compile-time known type, or a non-temporary value, but was a '{}'",
            TypeName(value.getType())
          );
        }
      }
      case UnaryOps::MultiPointerFrom: {
        auto value = interpret(node.operand, environment, outputFile, context);
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
          auto slicedObj = toRegister(&value, outputFile, environment);
          auto loadedSlice = *slicedObj.unbox<RegisterValue>();
          auto multiPointerType = Pool().multiPointerTo(*elementType);
          auto dataPtr = environment.makeTemporary(multiPointerType);
          fmt::println(
            outputFile,
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
      case UnaryOps::Minus: {
        auto value = interpret(node.operand, environment, outputFile, context);
        if (auto intLit = value.unbox<IntLiteral>()) {
          return Reference(IntLiteral(-intLit->value));
        } else if (auto floatLit = value.unbox<FloatLiteral>()) {
          return Reference(FloatLiteral(-floatLit->value));
        }
        auto loaded = toRegister(&value, outputFile, environment);
        auto type = loaded.getType();
        auto result = environment.makeTemporary(type);
        if (Pool().isFloat(type)) {
          fmt::println(
            outputFile,
            "%{} = fsub {} 0.0, {}",
            result.name,
            LlvmName(type),
            loaded
          );
          return Reference(result);
        }
        if (Pool().isSignedInt(type)) {
          fmt::println(
            outputFile,
            "%{} = sub {} 0, {}",
            result.name,
            LlvmName(type),
            loaded
          );
          return Reference(result);
        }
        crash(
          nodeIndex,
          "Unable to create a negative '{}'. Operand must be a float or signed "
          "integer",
          TypeName(type)
        );
      }
      case UnaryOps::BitNot: {
        auto value = interpret(node.operand, environment, outputFile, context);
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
        auto loaded = toRegister(&value, outputFile, environment);
        auto result = environment.makeTemporary(type);
        fmt::println(
          outputFile,
          "%{} = xor {} -1, {}",
          result.name,
          LlvmName(type),
          loaded
        );
        return Reference(result);
      }
      case UnaryOps::MakeSlice: {
        auto value = interpret(node.operand, environment, outputFile, context);
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
          return makeSlice(value, length, outputFile, environment);
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
      case UnaryOps::Return: {
        auto returnValue = parser.readOptional(node.operand.value);
        if (!context.returns.type) {
          crash(nodeIndex, "Return can only be used inside a function");
        }
        auto returnType = context.returns.type.value();
        if (returnValue) {
          parser.locationOf(returnValue.value()).underline(std::cout);
          if (Pool().isVoid(returnType)) {
            crash(
              *returnValue,
              "Cannot return value in a function with a 'void' return type"
            );
          }
          StatementContext returnContext{.expectedType = returnType};
          auto value =
            interpret(*returnValue, environment, outputFile, returnContext);
          auto loadedValue = toRegister(&value, outputFile, environment);
          if (!Pool().isAssignable(value.getType(), returnType)) {
            crash(
              *returnValue,
              "Return value of type '{}' needs to be of type '{}'",
              TypeName(value.getType()),
              TypeName(returnType)
            );
          }
          auto registers = context.returns.registers;
          if (registers.isMemory()) {
            fmt::println(
              outputFile,
              "store {} {}, ptr %0\nret void",
              LlvmName(returnType),
              loadedValue
            );
          } else if (registers.allInt() || !Pool().isAggregate(returnType)) {
            fmt::println(
              outputFile,
              "ret {} {}",
              LlvmName(returnType),
              loadedValue
            );
          } else {
            if (context.returns.aggregateTypename.empty()) {
              crash(
                *returnValue,
                "Expected an aggregate llvm type name in context for return "
                "value, but was left empty"
              );
            }
            auto storage = environment.addTemporary();
            auto transmuted = environment.addTemporary();
            fmt::println(
              outputFile,
              "%{} = alloca {}",
              storage,
              LlvmName(returnType)
            );
            fmt::println(
              outputFile,
              "store {} {}, ptr %{}",
              LlvmName(returnType),
              loadedValue,
              storage
            );
            fmt::println(
              outputFile,
              "%{} = load {}, ptr %{}",
              transmuted,
              context.returns.aggregateTypename,
              storage
            );
            fmt::println(
              outputFile,
              "ret {} %{}",
              context.returns.aggregateTypename,
              transmuted
            );
          }
        } else {
          if (!Pool().isVoid(returnType)) {
            crash(
              *returnValue,
              "Must return a value in a function with non-void return type"
            );
          }
          fmt::println(outputFile, "ret void");
        }

        environment.hasReturned = true;
        return Reference(Never{});
      }
      case UnaryOps::Using: {
        auto value = interpret(node.operand, environment, outputFile, context);
        if (auto env = value.unboxEnv()) {
          environment.usings.push_back(*env);
          for (auto import : (*env)->usings) {
            environment.usings.push_back(import);
          }
          fmt::println("New symbols");
          environment.debug();
        } else {
          TODO("using for non-environment objects");
        }
        return Reference::Void();
      }
      }
      break;
    }
    case NodeType::If: {
      auto node = parser.getIf(nodeIndex);
      StatementContext conditionContext(context);
      auto expectedConditionType = Pool()._bool;
      conditionContext.expectedType = expectedConditionType;
      auto condition =
        interpret(node.condition, environment, outputFile, conditionContext);
      auto conditionType = condition.getType();
      if (conditionType != expectedConditionType) {
        crash(
          node.condition,
          "Condition of an 'if' statement needs to be of type 'bool'"
        );
      }
      auto loadedCondition = toRegister(&condition, outputFile, environment);

      auto comptime = condition.isComptime();
      std::stringstream ifInstruction;
      u32 ifLabel = environment.addTemporary();
      if (!comptime) {
        fmt::println(ifInstruction, "{}:", ifLabel);
      }
      environment.basicBlock = ifLabel;
      SwitchCase ifCase{
        .result = interpret(node.value, environment, ifInstruction, context),
        .entryBlock = ifLabel,
        .exitBlock = environment.basicBlock,
        .returns = environment.hasReturned,
      };
      u32 loadedIf = environment.addTemporary();
      environment.hasReturned = false;
      std::optional<TypeIndex> resultType = ifCase.result.getType();

      // Else
      auto hasElse = node.elseValue.has_value();
      std::stringstream elseInstruction;
      auto elseContext(context);
      if (!context.expectedType) {
        elseContext.expectedType = resultType;
      }
      u32 elseLabel = hasElse ? environment.addTemporary() : 0;
      if (hasElse) fmt::println(elseInstruction, "{}:", elseLabel);
      environment.basicBlock = elseLabel;
      SwitchCase elseCase = hasElse ? SwitchCase{
        .result = interpret(
          *node.elseValue,
          environment,
          elseInstruction,
          elseContext
        ),
        .entryBlock = elseLabel,
        .exitBlock = environment.basicBlock,
        .returns = environment.hasReturned,
      } : SwitchCase();
      environment.hasReturned = ifCase.returns && elseCase.returns;

      if (hasElse && resultType) {
        auto elseType = elseCase.result.getType();
        log("Result type: {}", TypeName(*resultType));
        resultType = Pool().coerce(*resultType, elseType);
        log("Result type 2: {}", TypeName(*resultType));
      }

      if (resultType && context.expectedType) {
        resultType = Pool().isAssignable(*resultType, *context.expectedType);
      }

      if (comptime) {
        auto conditionValue = condition.unboxBool().value();
        if (conditionValue) {
          outputFile << ifInstruction.str();
          environment.hasReturned = ifCase.returns;
          return ifCase.result;
        } else {
          outputFile << elseInstruction.str();
          environment.hasReturned = elseCase.returns;
          return elseCase.result;
        }
      }

      u32 endLabel;
      if (hasElse) {
        fmt::println(
          outputFile,
          "br i1 {}, label %{}, label %{}",
          loadedCondition,
          ifLabel,
          elseLabel
        );
        bool lValueIf = ifCase.result.lValue().has_value();
        bool lValueElse = elseCase.result.lValue().has_value();
        bool useRValue = !(lValueIf && lValueElse) && resultType.has_value();
        outputFile << ifInstruction.str();
        if (useRValue && lValueIf) {
          fmt::println(
            outputFile,
            "%{} = load {}, ptr {}",
            loadedIf,
            LlvmName(*resultType),
            ifCase.result
          );
          ifCase.result.value = RegisterValue(loadedIf, *resultType);
        }
        if (useRValue && lValueElse) {
          u32 loadedElse = environment.addTemporary();
          fmt::println(
            elseInstruction,
            "%{} = load {}, ptr {}",
            loadedElse,
            LlvmName(*resultType),
            elseCase.result
          );
          elseCase.result.value = RegisterValue(loadedElse, *resultType);
        }
        endLabel = environment.addTemporary();
        if (!ifCase.returns) {
          fmt::println(outputFile, "br label %{}", endLabel);
        }
        outputFile << elseInstruction.str();
        if (!elseCase.returns) {
          fmt::println(outputFile, "br label %{}", endLabel);
        }
      } else {
        endLabel = environment.addTemporary();
        fmt::println(
          outputFile,
          "br i1 {}, label %{}, label %{}",
          loadedCondition,
          ifLabel,
          endLabel
        );
        outputFile << ifInstruction.str();
        if (!ifCase.returns) {
          fmt::println(outputFile, "br label %{}", endLabel);
        }
      }
      environment.basicBlock = endLabel;

      if (ifCase.returns && elseCase.returns) {
        environment.hasReturned = true;
        return Reference(Never{});
      }

      fmt::println(outputFile, "{}:", endLabel);

      if (resultType && resultType != Pool()._void) {
        auto type = resultType.value();
        bool lValue = ifCase.result.lValue().has_value() &&
                      elseCase.result.lValue().has_value();
        auto phiResult =
          lValue ? Reference(StackValue(environment.addTemporary(), type))
                 : Reference(environment.makeTemporary(type));
        LlvmName typeName(lValue ? Pool().pointerTo(Pool()._void) : type);
        if (auto floatType = Pool().unbox<Float>(type)) {
          fmt::println(
            outputFile,
            "{} = phi {} [{}, %{}], [{}, %{}]",
            phiResult,
            typeName,
            ifCase.result.coerceFloat(floatType->precision),
            ifCase.exitBlock,
            elseCase.result.coerceFloat(floatType->precision),
            elseCase.exitBlock
          );
        } else {
          fmt::println(
            outputFile,
            "{}  = phi {} [{}, %{}], [{}, %{}]",
            phiResult,
            typeName,
            ifCase.result,
            ifCase.exitBlock,
            elseCase.result,
            elseCase.exitBlock
          );
        }
        return phiResult;
      }

      return Reference::Void();
    }
    case NodeType::Struct: {
      auto node = parser.getStruct(nodeIndex);
      // TODO: methods
      auto named = context.name.has_value();
      std::string llvmName = named ? environment.addGlobal(context.name.value())
                                   : environment.addGlobal();
      std::stringstream typeInstruction;
      typeInstruction << llvmName << " = type {";
      std::string typeName =
        named ? std::string(context.name.value()) : "Anonymous type";
      auto [typeIndex, structIndex] =
        Pool().makeStruct(std::move(typeName), std::move(llvmName));
      if (named) {
        environment.define(context.name.value(), Reference(typeIndex));
      }

      bool hasFields = false;
      for (auto fieldIndex : node.children) {
        auto fieldNode = parser.getNode(fieldIndex);
        auto nodeType = fieldNode.nodeType;
        switch (nodeType) {
        case NodeType::Definition: {
          if (hasFields) typeInstruction << ", ";
          auto definitionNode = parser.getDefinition(fieldIndex);
          auto fieldName = definitionNode.name->lexeme;
          OptionalType type = interpret(
                                definitionNode.type.value(),
                                environment,
                                outputFile,
                                context
          )
                                .unboxType();
          if (!type) {
            crash(
              nodeIndex,
              "Type for field '{}' must be known at compile time",
              fieldName
            );
          }

          // TODO: default values
          Pool().getStruct(structIndex).defineField(fieldName, *type);
          fmt::print(typeInstruction, "{}", LlvmName(*type));
          hasFields = true;
          break;
        }
        default:
          TODO("TODO: implement struct fields");
        }
      }
      typeInstruction << "}";
      globalsStack.push(typeInstruction.str());
      if (node.implBlock) {
        Todo(*node.implBlock, "Remove 'impl' block inside structs");
      }
      return Reference(typeIndex);
    }
    case NodeType::DotAccess: {
      return dotAccess(nodeIndex, outputFile, environment, context);
    }
    case NodeType::ArgumentList: {
      if (!context.expectedType) {
        crash(nodeIndex, "Can't evaluate value tuple without inferred type");
      }
      auto type = context.expectedType.value();
      auto structDefinition = Pool().getStruct(type);
      if (!structDefinition.has_value()) {
        crash(nodeIndex, "Can't construct non-struct type {}", TypeName(type));
      }
      auto argsNode = parser.getArgumentList(nodeIndex);
      return constructStruct(
        nodeIndex,
        type,
        argsNode,
        environment,
        outputFile
      );
    }
    case NodeType::ParameterList: {
      crash(nodeIndex, "Input list nodes shouldn't be directly interpreted");
    }
    case NodeType::Enum: {
      auto node = parser.getEnumDefinition(nodeIndex);
      // TODO: ADT
      TypeIndex rawType;
      if (node.rawType) {
        auto rawValue =
          interpret(*node.rawType, environment, outputFile, context);
        if (auto type = rawValue.unboxType()) {
          rawType = *type;
          if (!Pool().isInt(rawType)) {
            crash(
              *node.rawType,
              "Enum raw type '{}' isn't an integer",
              TypeName(rawType)
            );
          }
        } else {
          crash(
            *node.rawType,
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

      auto [typeIndex, enumIndex] = Pool().addEnum(
        rawType,
        context.name.has_value() ? std::string(context.name.value())
                                 : "Anonymous Enum"
      );
      u32 valueCount = 0;
      Enum& enumDefinition = Pool().getEnum(enumIndex);
      for (auto [nameToken, valueNode] : node.entries) {
        auto name = parser.getToken(nameToken)->lexeme;

        if (parser.readOptional(valueNode.value)) {
          auto entryValue =
            interpret(valueNode, environment, outputFile, context);
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
    case NodeType::MultiLineString: {
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
      return makeSlice(ref, lengthValue, outputFile, environment);
    }
    case NodeType::ForLoop: {
      auto node = parser.getForLoop(nodeIndex);
      auto loopId = environment.addTemporary();
      auto loopHeader = environment.addLabel("for", loopId);
      auto loopCondition = loopHeader + ".if";
      auto loopUpdate = loopHeader + ".else";
      auto loopBody = loopHeader + ".continue";
      auto endLabel = loopHeader + ".break";

      auto iterator =
        interpret(node.iterator, environment, outputFile, context);

      if (auto range = iterator.unbox<Range>()) {
        TODO("Range iterators");
      } else if (
        auto type = iterator.getType();
        auto elementType = Pool().sliceElementType(type)
      ) {
        fmt::println(outputFile, "{}:", loopHeader);

        auto sliceRegister = toRegister(&iterator, outputFile, environment);
        auto slicePointer =
          environment.makeTemporary(Pool().multiPointerTo(*elementType));
        auto sliceLength = environment.makeTemporary(Pool()._usize);
        auto endPointer =
          environment.makeTemporary(Pool().multiPointerTo(*elementType));
        fmt::println(
          outputFile,
          "{} = extractElement {} {}, {} 0",
          slicePointer,
          LlvmName(type),
          sliceRegister,
          LlvmName(slicePointer.type)
        );
        fmt::println(
          outputFile,
          "{} = extractElement {} {}, {} 1",
          sliceLength,
          LlvmName(type),
          sliceRegister,
          LlvmName(sliceLength.type)
        );
        fmt::println(
          outputFile,
          "{} = getelementptr {}, ptr {}, {} {}",
          endPointer,
          LlvmName(*elementType),
          slicePointer,
          LlvmName(sliceLength.type),
          sliceLength
        );

        fmt::println(outputFile, "{}:", loopCondition);
        Environment loopEnv(&environment, loopHeader);
        // TODO: by ref vs by value
        auto iterationVariable = StackValue(node.capture->lexeme, *elementType);
        auto nextIterationVar =
          environment.makeTemporary(Pool().multiPointerTo(*elementType));
        loopEnv.define(node.capture->lexeme, Reference(iterationVariable));

        fmt::println(
          outputFile,
          "{} = phi ptr [{}, %{}], [{}, %{}]",
          iterationVariable,
          slicePointer,
          loopHeader,
          nextIterationVar,
          loopUpdate
        );
        auto loopBound =
          environment.makeTemporary(Pool().multiPointerTo(*elementType));
        fmt::println(
          outputFile,
          "{} = icmp eq ptr {}, {}",
          loopBound,
          iterationVariable,
          endPointer
        );
        fmt::println(
          outputFile,
          "br i1 {}, label %{}, label %{}",
          loopBound,
          endLabel,
          loopBody
        );

        fmt::println(outputFile, "{}:", loopBody);
        // TODO: loop value??
        interpret(node.body, loopEnv, outputFile, context);

        fmt::println(outputFile, "br %{}\n{}:", loopUpdate, loopUpdate);
        fmt::println(
          outputFile,
          "{} = getelementptr {}, ptr {}, i64 1\nbr %{}\n{}:",
          nextIterationVar,
          LlvmName(*elementType),
          iterationVariable,
          loopCondition,
          endLabel
        );

        return Reference::Void();
      } else {
        TODO("Non-range / slice for loops");
      }

      Environment loopEnv(environment);

      // loopEnv.define();

      // TODO: consider value expression (see
      // https://ziglang.org/documentation/master/#while)
      return Reference::Void();
    }
    case NodeType::Apply: {
      auto node = parser.getNode(nodeIndex);
      auto object = interpret({node.left}, environment, outputFile, context);
      auto objectType = object.getType();
      Parser::ArgumentList argsNode{
        .requiredArgs = span((NodeIndex*)&node.right, 1)
      };
      fmt::println("Applying!!!");
      parser.locationOf(nodeIndex).underline(std::cout);
      if (auto function = object.unboxFunction()) {
        return callFunction(
          function,
          argsNode,
          nodeIndex,
          environment,
          outputFile
        );
      } else if (auto type = object.unboxType()) {
        if (auto structDefinition = Pool().getStruct(*type)) {
          return constructStruct(
            nodeIndex,
            *type,
            argsNode,
            environment,
            outputFile
          );
        } else {
          crash(
            nodeIndex,
            "Can't construct non-struct type {}",
            TypeName(*type)
          );
        }
      } else if (
        auto methodTest = environment.getStatic(objectType, "multiply")
      ) {
        // TODO: calling multiply method in arithmeticOperation
        auto lValue = object.lValue();
        if (!lValue) TODO("Calling methods on non-lvalues");
        auto method =
          findMethod(nodeIndex, "multiply", objectType, environment);
        return callFunction(
          method,
          argsNode,
          nodeIndex,
          environment,
          outputFile,
          &object
        );
      } else {
        fmt::println("Applying");

        Encodings::BinaryOp binOp{
          .left = {node.left},
          .right = {node.right},
          .operation = parser.toPointer(node.token)
        };
        return arithmeticOperation(
          TokenType::Mult,
          binOp,
          object,
          nodeIndex,
          environment,
          outputFile
        );
      }
    }
    }
    crash(nodeIndex, "Unknown node type");
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
        case 'x': {
          char hex1 = str[++i];
          char hex2 = str[++i];
          if (!Tokenization::Tokenizer::isHex(hex1)) {
            crash(
              token,
              "Invalid hex escape '\\x{}{}': {} is not a hexidecimal digit "
              "0-9,A-F",
              hex1,
              hex2,
              hex1
            );
          }
          if (!Tokenization::Tokenizer::isHex(hex2)) {
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
        byteLength++;
      }
    }
    return {escaped, byteLength};
  }

  void crashBinOp(TokenPointer token, Reference* leftVal, Reference* rightVal) {
    crash(
      token,
      "Unable to perform binary operation '{}' on types '{}' and '{}'",
      token->lexeme,
      TypeName(leftVal->getType()),
      TypeName(rightVal->getType())
    );
  }

  Environment run() {
    StatementContext context;
    auto outputFile = targetType == TargetType::Cpu
                        ? CompilerContext::inst().blub.outputFileStream
                        : CompilerContext::inst().cuda.outputFileStream;
    auto& globalStream = targetType == TargetType::Cpu
                           ? CompilerContext::inst().blub.globalInitialization
                           : CompilerContext::inst().cuda.globalInitialization;
    for (auto node : program) {
      interpret(node, fileEnvironment, globalStream, context);
      while (!globalsStack.empty()) {
        *outputFile << globalsStack.front() << "\n";
        globalsStack.pop();
      }
    }

    while (!functionStubs.empty()) {
      auto stub = functionStubs.back();
      defineFunction(stub, &fileEnvironment);
      functionStubs.pop_back();
      while (!globalsStack.empty()) {
        *outputFile << globalsStack.front() << "\n";
        globalsStack.pop();
      }
    }

    dumpStatements();

    return fileEnvironment;
  }

  static std::string& readFile(fs::path filePath) {
    std::ifstream inputFile(filePath);

    if (!inputFile.is_open()) {
      fmt::println(
        std::cerr,
        "Error: could not open the file {}",
        filePath.string()
      );
      abort();
    }

    // File contents needs to be kept around after this TL because names are
    // string_views
    std::string& fileContents = *new std::string(
      std::istreambuf_iterator<char>(inputFile),
      std::istreambuf_iterator<char>()
    );

    return fileContents;
  }

  static Environment* cudaImport(fs::path fileName) {
    return compile(
      fileName,
      *CompilerContext::inst().cuda.outputFileStream,
      TargetType::Gpu
    );
  }

  static Environment* compile(
    fs::path fileName,
    std::ofstream& outFile,
    TargetType targetType
  ) {
    using Imports = std::unordered_map<std::string, Environment>;
    static Imports cpuFiles;
    static Imports gpuFiles;
    if (targetType == TargetType::Cpu) {
      CompilerContext::inst().blub.outputFileStream = &outFile;
    } else {
      CompilerContext::inst().cuda.outputFileStream = &outFile;
    }

    Imports& compiledFiles =
      targetType == TargetType::Cpu ? cpuFiles : gpuFiles;
    fileName = fs::absolute(fileName);
    fileName = fs::weakly_canonical(fileName);

    if (compiledFiles.contains(fileName)) {
      return &compiledFiles[fileName];
    }

    std::string& fileContents = readFile(fileName);

    Tokenizer tokenizer(fileContents, fileName);
    Parser parser(tokenizer);
    std::vector<NodeIndex> program = parser.parse();

    TranslationUnit interpreter(parser, program, fileName, outFile, targetType);
    auto [env, success] =
      compiledFiles.emplace(std::move(fileName), interpreter.run());
    return &env->second;
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
    crash(token, fmt, std::forward<Args>(args)...);
  }

  Reference makeSlice(
    Reference& dataPointer,
    Reference& length,
    std::ostream& output,
    Environment& env
  ) {
    auto lengthLoaded = toRegister(&length, output, env);
    auto dataType = dataPointer.getType();
    auto type = Pool().sliceOf(dataType);
    auto intermediateResult = env.makeTemporary(type);
    fmt::println(
      output,
      "{} = insertvalue {} undef, ptr {}, 0",
      intermediateResult,
      LlvmName(type),
      dataPointer
    );
    auto result = env.makeTemporary(Pool().sliceOf(dataType));
    fmt::println(
      output,
      "{} = insertvalue {} {}, {} {}, 1",
      result,
      LlvmName(type),
      intermediateResult,
      LlvmName(Pool()._usize),
      lengthLoaded
    );
    return Reference(result);
  }

  Reference sliceRange(
    Range& range,
    NodeIndex rangeNode,
    std::ostream& outputFile,
    Environment& environment,
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
      // guardLowerBound(lower, environment, outputFile);
    }

    extendToUsize(lower, environment, outputFile);
    extendToUsize(upper, environment, outputFile);

    if (baseLength && range.hasUpper()) {
      guardExclusiveInBounds(
        upper,
        baseLength.value(),
        environment,
        outputFile
      );
    }

    auto lengthRef = Reference(
      guardNonnegativeLength(lower, upperBound, environment, outputFile)
    );
    auto newStartPoint = environment.makeTemporary(dataPointer.getType());
    fmt::println(
      outputFile,
      "{} = getelementptr {}, ptr {}, {} {}",
      newStartPoint,
      LlvmName(newStartPoint.type),
      dataPointer,
      LlvmName(usize),
      lower
    );
    auto startPointer = Reference(newStartPoint);
    return makeSlice(startPointer, lengthRef, outputFile, environment);
  }

  void extendToUsize(
    Reference& index,
    Environment& environment,
    std::ostream& outputFile
  ) {
    auto usize = Pool()._usize;
    if (
      auto type = index.getType(); Pool().isUnsignedInt(type) && type != usize
    ) {
      auto extended = environment.makeTemporary(Pool()._usize);
      fmt::println(
        outputFile,
        "{} = zext {} {} to {}",
        extended,
        LlvmName(type),
        index,
        LlvmName(usize)
      );
      index.value = extended;
    }
  }

  void guardLowerBound(
    Reference& index,
    Environment& environment,
    std::ostream& outputFile
  ) {
    auto isNegative = environment.makeTemporary(Pool()._bool);
    auto crashBlockId = environment.addTemporary();
    auto continueBlockId = environment.addTemporary();
    fmt::println(
      outputFile,
      "{} = icmp slt {} {}, 0",
      isNegative,
      LlvmName(index.getType()),
      index
    );
    fmt::println(
      outputFile,
      "br i1 {}, label %{}, label %{}",
      isNegative,
      crashBlockId,
      continueBlockId
    );
    fmt::println(outputFile, "{}:", crashBlockId);
    crashInstruction(outputFile);
    fmt::println(outputFile, "{}:", continueBlockId);
  }

  void guardExclusiveInBounds(
    Reference& index,
    RangeBound& baseLength,
    Environment& environment,
    std::ostream& outputFile
  ) {
    auto isOutsideBounds = environment.makeTemporary(Pool()._bool);
    auto crashBlockId = environment.addTemporary();
    auto continueBlockId = environment.addTemporary();
    fmt::println(
      outputFile,
      "{} = icmp ult {} {}, {}",
      isOutsideBounds,
      LlvmName(Pool()._usize),
      baseLength,
      index
    );
    fmt::println(
      outputFile,
      "br i1 {}, label %{}, label %{}",
      isOutsideBounds,
      crashBlockId,
      continueBlockId
    );
    fmt::println(outputFile, "{}:", crashBlockId);
    crashInstruction(outputFile);
    fmt::println(outputFile, "{}:", continueBlockId);
  }

  void guardIndexInBounds(
    Reference& index,
    RangeBound& baseLength,
    Environment& environment,
    std::ostream& outputFile
  ) {
    Reference usedIndex = index;
    extendToUsize(usedIndex, environment, outputFile);

    auto isOutsideBounds = environment.makeTemporary(Pool()._bool);
    auto crashBlockId = environment.addTemporary();
    auto continueBlockId = environment.addTemporary();
    fmt::println(
      outputFile,
      "{} = icmp ule {} {}, {}",
      isOutsideBounds,
      LlvmName(Pool()._usize),
      baseLength,
      usedIndex
    );
    fmt::println(
      outputFile,
      "br i1 {}, label %{}, label %{}",
      isOutsideBounds,
      crashBlockId,
      continueBlockId
    );
    fmt::println(outputFile, "{}:", crashBlockId);
    crashInstruction(outputFile);
    fmt::println(outputFile, "{}:", continueBlockId);
  }

  RegisterValue guardNonnegativeLength(
    Reference& lower,
    RangeBound& upper,
    Environment& environment,
    std::ostream& outputFile
  ) {
    auto usize = Pool()._usize;
    auto length = environment.makeTemporary(usize);
    fmt::println(
      outputFile,
      "{} = sub {} {}, {}",
      length,
      LlvmName(usize),
      upper,
      lower
    );

    auto lengthIsNegative = environment.makeTemporary(Pool()._bool);
    auto crashBlockId = environment.addTemporary();
    auto continueBlockId = environment.addTemporary();
    fmt::println(
      outputFile,
      "{} = icmp slt {} {}, 0",
      lengthIsNegative,
      LlvmName(usize),
      lower
    );
    fmt::println(
      outputFile,
      "br i1 {}, label %{}, label %{}",
      lengthIsNegative,
      crashBlockId,
      continueBlockId
    );
    fmt::println(outputFile, "{}:", crashBlockId);
    crashInstruction(outputFile);
    fmt::println(outputFile, "{}:", continueBlockId);

    return length;
  }

  pair<RegisterValue, RegisterValue> getSliceElements(
    RegisterValue slice,
    Environment& environment,
    std::ostream& outputFile
  ) {
    auto elementType = Pool().sliceElementType(slice.type);
    assert(elementType);
    auto dataPointer = environment.makeTemporary(*elementType);
    auto length = environment.makeTemporary(Pool()._usize);
    fmt::println(
      outputFile,
      "{} = extractvalue {} {}, 0",
      dataPointer,
      TypeName(slice.type),
      slice
    );
    fmt::println(
      outputFile,
      "{} = extractvalue {} {}, 1",
      length,
      TypeName(slice.type),
      slice
    );
    return {dataPointer, length};
  }

  void crashInstruction(std::ostream& output) {
    fmt::println(output, "call void @llvm.trap()\nunreachable");
  }

  Reference constructStruct(
    NodeIndex nodeIndex,
    TypeIndex type,
    Parser::ArgumentList& argsNode,
    Environment& environment,
    std::ostream& outputFile
  ) {
    // TODO: unions: named and unnamed
    auto structDefinition = Pool().getStruct(type);
    if (!structDefinition.has_value()) {
      crash(nodeIndex, "Can't construct non-struct type {}", TypeName(type));
    }
    auto structLlvmName = LlvmName(type);
    auto structName = TypeName(type);

    Struct& definition = **structDefinition;
    auto fieldTypes = TypeSpan(definition.fieldTypes);

    auto [arguments, namedArguments] = getArguments(
      fieldTypes,
      argsNode,
      environment,
      outputFile,
      (*structDefinition)->fields,
      nullptr
    );
    std::vector<bool> setArguments(fieldTypes.size(), false);

    Reference structVal = Reference(ZeroInit{});
    // TODO: default values that aren't zero initialized; using 0 for now to
    // avoid initializing all fields in C structs
    for (u32 i = 0; i < arguments.size(); i++) {
      auto fieldValue = toRegister(&arguments[i], outputFile, environment);
      auto fieldTypeLlvmName = LlvmName((*structDefinition)->fieldTypes[i]);
      setArguments[i] = true;
      Reference prevStruct = structVal;
      structVal.value = environment.makeTemporary(type);
      Reference ref(structVal);
      fmt::println(
        outputFile,
        "{} = insertvalue {} {}, {} {}, {}",
        ref,
        structLlvmName,
        prevStruct,
        fieldTypeLlvmName,
        fieldValue,
        i
      );
    }

    for (auto arg : argsNode.optionalArgs) {
      auto argToken = parser.toPointer(arg.token);
      auto name = argToken->lexeme;
      assert(namedArguments.contains(name));
      auto value = toRegister(&namedArguments[name], outputFile, environment);
      auto field = definition.getField(name);
      if (!field) {
        crash(
          argToken,
          "Unknown field '{}' for type '{}'",
          name,
          TypeName(type)
        );
      } else {
        fmt::println(
          "Assigning value of type {} to field of type {}",
          TypeName(value.getType()),
          TypeName(field->type)
        );
      }
      auto i = field->index;
      auto targetType = Pool().isAssignable(value.getType(), field->type);
      if (!targetType) {
        fmt::println("Struct {}:", structName);
        for (auto [name, type] : structDefinition.value()->fields) {
          fmt::println("\t{}:\t{}", name, TypeName(type.type));
        }
        crash(
          argToken,
          "Unable to assign argument of type '{}' to parameter of type '{}'",
          TypeName(value.getType()),
          TypeName(field->type)
        );
      }
      if (Pool().isFloat(*targetType)) {
        if (auto intLit = value.getInt()) {
          value.value = FloatLiteral(*intLit);
        }
      }
      if (setArguments[i]) {
        auto location = parser.locationOf(argsNode.requiredArgs[i]);
        fmt::println(std::cerr, "Field '{}' originally assigned here", name);
        location.underline(std::cerr);
        crash(argToken, "Duplicate assignment for field '{}'", name);
      }
      setArguments[i] = field->index;
      Reference prevStruct = structVal;
      structVal.value = environment.makeTemporary(type);
      fmt::println(
        outputFile,
        "{} = insertvalue {} {}, {} {}, {}",
        structVal,
        structLlvmName,
        prevStruct,
        LlvmName(field->type),
        value,
        field->index
      );
    }
    return Reference(structVal);
  }

  pair<std::vector<Reference>, std::unordered_map<Identifier, Reference>>
  getArguments(
    TypeSpan paramTypes,
    Parser::ArgumentList& argsNode,
    Environment& environment,
    std::ostream& outputFile,
    FieldMap& fieldNames,
    Reference* firstArg
  ) {
    vector<Reference> arguments;
    if (firstArg) arguments.push_back(*firstArg);
    u32 i = arguments.size();
    for (auto arg : argsNode.requiredArgs) {
      auto paramType = paramTypes[i];
      StatementContext context{.expectedType = paramType};
      auto argument = interpret(arg, environment, outputFile, context);
      auto targetType = argument.isAssignableTo(paramType);
      if (!targetType) {
        crash(
          arg,
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
      Reference loadedArg = toRegister(&argument, outputFile, environment);
      if (auto floatType = Pool().getFloat(paramType)) {
        arguments.push_back(loadedArg.coerceFloat(floatType->precision));
      } else {
        arguments.push_back(loadedArg);
      }
      i++;
    }

    std::unordered_map<Identifier, Reference> namedArgs;
    for (auto [name, value] : argsNode.optionalArgs) {
      auto nameToken = parser.getToken(name);
      if (!fieldNames.contains(nameToken->lexeme)) {
        crash(nameToken, "Unknown named argument '{}'", nameToken->lexeme);
      }

      auto field = fieldNames[nameToken->lexeme];
      if (field.index < argsNode.requiredArgs.size()) {
        auto original = parser.locationOf(argsNode.requiredArgs[field.index]);
        fmt::println(
          "Orginally assigned at: {}:{}",
          original.line,
          original.column
        );
        original.underline(std::cerr);
        crash(nameToken, "Duplicate named argument '{}'", nameToken->lexeme);
      }

      StatementContext context{.expectedType = field.type};
      auto argument = interpret(value, environment, outputFile, context);
      auto targetType = argument.isAssignableTo(field.type);
      if (!targetType) {
        crash(
          value,
          fmt::runtime(
            "Unable to assign argument of type '{}' to parameter of type '{}'"
          ),
          TypeName(argument.getType()),
          TypeName(field.type)
        );
      }
      Reference loadedArg = toRegister(&argument, outputFile, environment);
      bool nonDuplicate = false;
      if (auto floatType = Pool().getFloat(field.type)) {
        auto result = namedArgs.emplace(
          nameToken->lexeme,
          loadedArg.coerceFloat(floatType->precision)
        );
        nonDuplicate = result.second;
      } else {
        auto result = namedArgs.emplace(nameToken->lexeme, loadedArg);
        nonDuplicate = result.second;
      }
      if (!nonDuplicate) {
        crash(nameToken, "Duplicate named argument");
      }
    }
    return std::make_pair(arguments, namedArgs);
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
            auto statements = parser.getBlock(function.body.value()).elements;
            for (auto statement : statements) {
              logger("Node type: {}", (int)parser.nodeType(statement));
              parser.locationOf(statement).underline(std::cout);
            }
          }
        }
      }
    }
  }

  void defineFunction(FunctionStub stub, Environment* environment) {
    Environment functionEnvironment = Environment(environment, stub.name);

    auto& instruction = outputFileStream;
    instruction << "define ";

    Function function(stub.functionType, stub.name);

    auto declarationResult = declareParamRegisters(instruction, function);
    functionEnvironment.nextTemporary = declarationResult.entryLabel + 1;
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
      if (!functionEnvironment
             .define(paramName, Reference(StackValue(paramName, paramType)))
             .has_value()) {
        crash(paramNode, "Duplicate function parameter {}", paramName);
      }
      parameterIndex++;
    }

    for (NodeIndex parameterIndex : parameters.optionalParameters) {
      Todo(parameterIndex, "Named parameters/default values");
    }

    OutContext loadingContext{
      .outputFile = instruction,
      .environment = functionEnvironment
    };
    loadParameterRegisters(loadingContext, stub.functionType, paramNames);

    FunctionType functionType = stub.functionType;
    auto returnType = functionType.returnType;

    StatementContext functionContext;
    functionContext.returns = {
      .type = returnType,
      .aggregateTypename = declarationResult.aggregateReturnTypeName,
      .registers = Pool().registerStorage(returnType),
    };

    auto body = parser.getBlock(node.body.value());
    for (auto statement : body.elements) {
      interpret(statement, functionEnvironment, instruction, functionContext);
    }

    if (!functionEnvironment.hasReturned) {
      if (returnType == Pool()._void) {
        instruction << "ret void\n";
      } else {
        crash(stub.definitionNode, "Return required for all code paths");
      }
    }

    instruction << "}\n\n";
    return;
  }

  Reference dotAccess(
    NodeIndex nodeIndex,
    std::ostream& outputFile,
    Environment& environment,
    StatementContext& context
  ) {
    auto node = parser.getDotAccess(nodeIndex);
    if (!node.object && !context.expectedType) {
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
    Reference object =
      node.object
        ? interpret(node.object.value(), environment, outputFile, context)
        : Reference(context.expectedType.value());

    std::string_view fieldName = node.fieldName->lexeme;

    if (auto type = object.unboxType()) {
      {
        auto sizing = Pool().getSizing(*type);
        if (fieldName == "size") {
          return Reference(IntLiteral(sizing.byteSize));
        } else if (fieldName == "alignment") {
          return Reference(IntLiteral(sizing.alignment.byteAlignment()));
        } else if (fieldName == "bitSize") {
          return Reference(IntLiteral(sizing.bitSize));
        }
      }

      if (auto enumDefinition = Pool().getEnum(*type)) {
        if (auto value = enumDefinition->get(fieldName)) {
          return Reference(IntLiteral(*value, *type));
        }
        crash(
          nodeIndex,
          "Unknown variant '{}' in enum '{}'",
          fieldName,
          TypeName(*type)
        );
      }

      if (auto staticVal = environment.getStatic(*type, fieldName)) {
        return Reference(staticVal);
      }

      crash(
        nodeIndex,
        "Unknown associated value '{}.{}'",
        TypeName(*type),
        fieldName
      );
    }

    if (auto import = object.unboxEnv()) {
      auto fileEnv = *import;
      if (auto value = fileEnv->find(fieldName)) {
        return Reference(*value);
      }
      // auto members = fileEnv->defs | std::views::transform([](const auto& x)
      // { return TypeName(x.second.getType()); });
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
    }

    // TODO: auto dereference pointers
    auto type = object.getType();
    OptionalType dereferenced = Pool().dereference(type);
    if (dereferenced.has_value()) {
      StackValue dereffed =
        dereference(&object, outputFile, environment, node.object.value());
      object.value = dereffed;
      type = *dereferenced;
    } else {
      log("Accessing field for type {}", TypeName(type));
    }

    auto boxedField = Pool().getFieldIndex(type, fieldName);

    if (!boxedField.has_value()) {
      auto lValue = object.lValue();
      if (!lValue) TODO("Calling methods on non-lvalues");
      auto method = findMethod(nodeIndex, fieldName, type, environment);
      return Reference(BoundFunction(*lValue, *method));
    }

    auto [fieldType, fieldIndex] = boxedField->first;
    type = boxedField->second;
    auto fieldPointer = environment.addTemporary();

    if (object.lValue()) {
      auto result = Reference(StackValue(fieldPointer, fieldType));
      fmt::println(
        outputFile,
        "{} = getelementptr inbounds {}, ptr {}, i32 0, i32 {}",
        result,
        LlvmName(type),
        object,
        fieldIndex
      );
      return result;
    } else if (auto registerValue = object.unbox<RegisterValue>()) {
      auto result = Reference(RegisterValue(fieldPointer, fieldType));
      fmt::println(
        outputFile,
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

  Reference arithmeticOperation(
    TokenType opType,
    Encodings::BinaryOp& node,
    Reference& leftVal,
    NodeIndex nodeIndex,
    Environment& environment,
    std::ostream& outputFile
  ) {

    StatementContext context{.expectedType = leftVal.getType()};
    auto rightVal = interpret(node.right, environment, outputFile, context);
    auto coercedType = Reference::coerceType(&leftVal, &rightVal);
    if (!coercedType.has_value()) {
      crash(
        nodeIndex,
        "Unable to perform binary operation on incompatible types"
      );
    }
    auto [operandType, coercedLeft, coeredRight] = coercedType.value();
    auto leftLiteral = toRegister(&coercedLeft, outputFile, environment);
    auto rightLiteral = toRegister(&coeredRight, outputFile, environment);
    auto resultType = operandType;
    std::string binaryOperator;
    switch (opType) {
    case TokenType::Plus: {
      if (
        auto left = leftLiteral.unbox<IntLiteral>(),
        right = rightLiteral.unbox<IntLiteral>();
        left && right
      ) {
        return Reference(IntLiteral(left->value + right->value));
      }
      if (
        auto left = leftLiteral.unbox<FloatLiteral>(),
        right = rightLiteral.unbox<FloatLiteral>();
        left && right
      ) {
        return Reference(FloatLiteral(left->value + right->value));
      }
      if (Pool().isInt(operandType)) binaryOperator = "add";
      else if (Pool().isFloat(operandType)) binaryOperator = "fadd";
      else crashBinOp(node.operation, &leftVal, &rightVal);
      break;
    }
    case TokenType::Minus: {
      if (
        auto left = leftLiteral.unbox<IntLiteral>(),
        right = rightLiteral.unbox<IntLiteral>();
        left && right
      ) {
        return Reference(IntLiteral(left->value - right->value));
      }
      if (
        auto left = leftLiteral.unbox<FloatLiteral>(),
        right = rightLiteral.unbox<FloatLiteral>();
        left && right
      ) {
        return Reference(FloatLiteral(left->value - right->value));
      }
      if (Pool().isInt(operandType)) binaryOperator = "sub";
      else if (Pool().isFloat(operandType)) binaryOperator = "fsub";
      else crashBinOp(node.operation, &leftVal, &rightVal);
      break;
    }
    case TokenType::Div: {
      if (
        auto left = leftLiteral.unbox<IntLiteral>(),
        right = rightLiteral.unbox<IntLiteral>();
        left && right
      ) {
        return Reference(IntLiteral(left->value / right->value));
      }
      if (
        auto left = leftLiteral.unbox<FloatLiteral>(),
        right = rightLiteral.unbox<FloatLiteral>();
        left && right
      ) {
        return Reference(FloatLiteral(left->value / right->value));
      }
      if (Pool().isSignedInt(operandType)) binaryOperator = "sdiv";
      else if (Pool().isInt(operandType)) binaryOperator = "udiv";
      else if (Pool().isFloat(operandType)) binaryOperator = "fdiv";
      else crashBinOp(node.operation, &leftVal, &rightVal);
      break;
    }
    case TokenType::Mult: {
      fmt::println("Multiplying a:");
      parser.locationOf(node.left).underline(std::cout);
      fmt::println("Multiplying b:");
      parser.locationOf(node.right).underline(std::cout);

      if (
        auto left = leftLiteral.unbox<IntLiteral>(),
        right = rightLiteral.unbox<IntLiteral>();
        left && right
      ) {
        fmt::println("Two int literals");
        return Reference(IntLiteral(left->value * right->value));
      }
      if (
        auto left = leftLiteral.unbox<FloatLiteral>(),
        right = rightLiteral.unbox<FloatLiteral>();
        left && right
      ) {
        fmt::println("Two float literals");
        return Reference(FloatLiteral(left->value * right->value));
      }
      fmt::println("Two ints or floats");
      if (Pool().isInt(operandType)) binaryOperator = "mul";
      else if (Pool().isFloat(operandType)) binaryOperator = "fmul";
      else crashBinOp(node.operation, &leftVal, &rightVal);
      break;
    }
    case TokenType::Remainder: {
      if (
        auto left = leftLiteral.unbox<IntLiteral>(),
        right = rightLiteral.unbox<IntLiteral>();
        left && right
      ) {
        return Reference(IntLiteral(left->value % right->value));
      }
      if (
        auto left = leftLiteral.unbox<FloatLiteral>(),
        right = rightLiteral.unbox<FloatLiteral>();
        left && right
      ) {
        auto result = std::remainder(left->value, right->value);
        return Reference(FloatLiteral(result));
      }
      if (Pool().isSignedInt(operandType)) binaryOperator = "srem";
      else if (Pool().isUnsignedInt(operandType)) binaryOperator = "urem";
      else if (Pool().isFloat(operandType)) binaryOperator = "frem";
      else crashBinOp(node.operation, &leftVal, &rightVal);
      break;
    }
    case TokenType::Lt: {
      if (
        auto left = leftLiteral.unbox<IntLiteral>(),
        right = rightLiteral.unbox<IntLiteral>();
        left && right
      ) {
        return Reference(left->value < right->value);
      }
      if (
        auto left = leftLiteral.unbox<FloatLiteral>(),
        right = rightLiteral.unbox<FloatLiteral>();
        left && right
      ) {
        return Reference(left->value < right->value);
      }
      resultType = Pool()._bool;
      if (Pool().isSignedInt(operandType)) binaryOperator = "icmp slt";
      else if (Pool().isInt(operandType)) binaryOperator = "icmp ult";
      else if (Pool().isFloat(operandType)) binaryOperator = "fcmp uolt";
      else crashBinOp(node.operation, &leftVal, &rightVal);
      break;
    }
    case TokenType::Gt: {
      if (
        auto left = leftLiteral.unbox<IntLiteral>(),
        right = rightLiteral.unbox<IntLiteral>();
        left && right
      ) {
        return Reference(left->value > right->value);
      }
      if (
        auto left = leftLiteral.unbox<FloatLiteral>(),
        right = rightLiteral.unbox<FloatLiteral>();
        left && right
      ) {
        return Reference(left->value > right->value);
      }
      resultType = Pool()._bool;
      if (Pool().isSignedInt(operandType)) binaryOperator = "icmp sgt";
      else if (Pool().isInt(operandType)) binaryOperator = "icmp ugt";
      else if (Pool().isFloat(operandType)) binaryOperator = "fcmp uogt";
      else crashBinOp(node.operation, &leftVal, &rightVal);
      break;
    }
    case TokenType::Leq: {
      if (
        auto left = leftLiteral.unbox<IntLiteral>(),
        right = rightLiteral.unbox<IntLiteral>();
        left && right
      ) {
        return Reference(left->value <= right->value);
      }
      if (
        auto left = leftLiteral.unbox<FloatLiteral>(),
        right = rightLiteral.unbox<FloatLiteral>();
        left && right
      ) {
        return Reference(left->value <= right->value);
      }
      resultType = Pool()._bool;
      if (Pool().isSignedInt(operandType)) binaryOperator = "icmp sle";
      else if (Pool().isInt(operandType)) binaryOperator = "icmp ule";
      else if (Pool().isFloat(operandType)) binaryOperator = "fcmp uole";
      else crashBinOp(node.operation, &leftVal, &rightVal);
      break;
    }
    case TokenType::Geq: {
      if (
        auto left = leftLiteral.unbox<IntLiteral>(),
        right = rightLiteral.unbox<IntLiteral>();
        left && right
      ) {
        return Reference(left->value >= right->value);
      }
      if (
        auto left = leftLiteral.unbox<FloatLiteral>(),
        right = rightLiteral.unbox<FloatLiteral>();
        left && right
      ) {
        return Reference(left->value >= right->value);
      }
      resultType = Pool()._bool;
      if (Pool().isSignedInt(operandType)) binaryOperator = "icmp sge";
      else if (Pool().isInt(operandType)) binaryOperator = "icmp uge";
      else if (Pool().isFloat(operandType)) binaryOperator = "fcmp uoge";
      else crashBinOp(node.operation, &leftVal, &rightVal);
      break;
    }
    case TokenType::DoubleEqual: {
      if (
        auto left = leftLiteral.unbox<IntLiteral>(),
        right = rightLiteral.unbox<IntLiteral>();
        left && right
      ) {
        return Reference(left->value == right->value);
      }
      if (
        auto left = leftLiteral.unbox<FloatLiteral>(),
        right = rightLiteral.unbox<FloatLiteral>();
        left && right
      ) {
        return Reference(left->value == right->value);
      }
      resultType = Pool()._bool;
      if (Pool().isInt(operandType) || Pool().isPointer(operandType))
        binaryOperator = "icmp eq";
      else if (Pool().isFloat(operandType)) binaryOperator = "fcmp ueq";
      else if (auto enumDef = Pool().getEnum(operandType)) {
        auto rawType = enumDef->rawType;
        if (Pool().isInt(rawType)) {
          binaryOperator = "icmp eq";
        } else if (Pool().isFloat(rawType)) {
          binaryOperator = "fcmp ueq";
        } else {
          crashBinOp(node.operation, &leftVal, &rightVal);
        }
      } else crashBinOp(node.operation, &leftVal, &rightVal);
      break;
    }
    case TokenType::NotEqual: {
      if (
        auto left = leftLiteral.unbox<IntLiteral>(),
        right = rightLiteral.unbox<IntLiteral>();
        left && right
      ) {
        return Reference(left->value != right->value);
      }
      if (
        auto left = leftLiteral.unbox<FloatLiteral>(),
        right = rightLiteral.unbox<FloatLiteral>();
        left && right
      ) {
        return Reference(left->value != right->value);
      }
      resultType = Pool()._bool;
      if (Pool().isInt(operandType) || Pool().isPointer(operandType))
        binaryOperator = "icmp ne";
      else if (Pool().isFloat(operandType)) binaryOperator = "fcmp une";
      else if (auto enumDef = Pool().getEnum(operandType)) {
        auto rawType = enumDef->rawType;
        if (Pool().isInt(rawType)) {
          binaryOperator = "icmp ne";
        } else if (Pool().isFloat(rawType)) {
          binaryOperator = "fcmp une";
        } else {
          crashBinOp(node.operation, &leftVal, &rightVal);
        }
      } else crashBinOp(node.operation, &leftVal, &rightVal);
      break;
    }
    default:
      crash(
        nodeIndex,
        "Unknown binary operation: {}",
        parser.getToken(nodeIndex)->lexeme
      );
    }
    auto resultName = Reference(environment.makeTemporary(resultType));
    fmt::println(
      outputFile,
      "{} = {} {} {}, {}",
      resultName,
      binaryOperator,
      LlvmName(operandType),
      leftLiteral,
      rightLiteral
    );
    return resultName;
  }

  Reference callFunction(
    Function* func,
    Parser::ArgumentList& argsNode,
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

    // TODO: named arguments
    static FieldMap namedArgs;

    auto [arguments, namedArguments] = getArguments(
      parameterTypes,
      argsNode,
      environment,
      outputFile,
      namedArgs,
      selfArg
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

  Function* findMethod(
    NodeIndex nodeIndex,
    string_view fieldName,
    TypeIndex type,
    Environment& environment
  ) {
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
          ptrType && type == *ptrType
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
};
