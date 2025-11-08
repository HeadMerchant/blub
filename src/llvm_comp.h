#pragma once
#include "cimport.h"
#include "common.h"
#include "environment.h"
#include "fmt/base.h"
#include "interpreter/value.h"
#include "logging.h"
#include "parser.h"
#include "tokenizer.h"
#include "types.h"
#include <bit>
#include <cassert>
#include <charconv>
#include <filesystem>
#include <fmt/args.h>
#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ostream.h>
#include <fmt/ranges.h>
#include <ostream>
#include <queue>
#include <ranges>
#include <span>
#include <sstream>
#include <string>
#include <string_view>

namespace fs = std::filesystem;

struct CompilerContext {
  struct {
  } blub;
  struct {
    std::vector<std::string_view> defines;
    std::vector<std::string_view> includes;
    std::vector<std::string_view> includeDirs;
    std::vector<std::string_view> linkLibraries;
  } c;
  struct {
  } cuda;

  static CompilerContext& inst() {
    static CompilerContext instance;
    return instance;
  }
};

struct StatementContext {
  std::optional<std::string_view> name;

  Types::OptionalType expectedType;
  Types::OptionalType returnType;
};

class TranslationUnit {
public:
  std::ofstream& outputFileStream;
  fs::path inputFilePath;
  ASTNode* main;
  Environment fileEnvironment;
  Parser& parser;
  std::span<NodeIndex> program;
  std::ostream& log;
  std::queue<std::string> globalsStack;

  TranslationUnit(Parser& parser, std::span<NodeIndex> program, fs::path inputPath, std::ofstream& outputFileStream)
      : parser(parser), fileEnvironment(), program(program), log(logger(LogLevel::DEBUG)), inputFilePath(inputPath), outputFileStream(outputFileStream) {}

  Reference toRegister(Reference* value, std::ostream& outputFile, Environment& environment) {
    if (value->isLiteral()) {
      return *value;
    } else if (auto stackValue = std::get_if<StackValue>(&value->value)) {
      auto registerIndex = environment.makeTemporary(stackValue->type);
      auto registerValue = Reference(registerIndex);
      // registerValue.outputLLVMName(outputFile, &environment);
      // fmt::print(" = load {}, ptr ",
      // Types::Pool().getLLVMType(stackValue->type));
      fmt::println(outputFile, "{} = load {}, ptr {}", registerValue, LlvmName(stackValue->type), *value);
      // value->outputLLVMName(outputFile, &environment);
      // outputFile << '\n';
      return Reference(registerIndex);
    } else if (auto recursive = std::get_if<Reference*>(&value->value)) {
      return toRegister(*recursive, outputFile, environment);
    } else {
      TODO("Error converting to literal");
    }
  }

  Reference toByValPointer(Reference value, std::ostream& outputFile, Environment& environment, NodeIndex node) {
    auto loadedValue = std::get<RegisterValue>(toRegister(&value, outputFile, environment).value);
    auto stackPointer = Reference(environment.makeTemporary(loadedValue.type));
    auto loadedReference = Reference(stackPointer);
    // auto typeName = Types::Pool().getLLVMType(loadedValue.type);

    // loadedReference.outputLLVMName(outputFile, &environment);
    auto sizing = Types::Pool().getSizing(loadedValue.type);
    fmt::println(outputFile, "{} = alloca {}, align {}", stackPointer, LlvmName(loadedValue.type), sizing.alignment.byteAlignment());
    fmt::println(outputFile, "store {} {}, ptr {}", LlvmName(loadedValue.type), loadedReference, LlvmName(loadedValue.type), sizing.alignment.byteAlignment());
    // fmt::println(outputFile, " = alloca {}, align {}", typeName,
    // sizing.alignment.byteAlignment()); fmt::println("store {} ", typeName);
    // Reference(loadedValue).outputLLVMName(outputFile, &environment);
    // outputFile << ", ptr ";
    // loadedReference.outputLLVMName(outputFile, &environment);
    // outputFile << '\n';

    return stackPointer;
  }

  StackValue dereference(Reference* value, std::ostream& outputFile, Environment& environment, NodeIndex node) {
    if (auto registerValue = std::get_if<RegisterValue>(&value->value)) {
      Types::OptionalType dereferencedType = Types::Pool().dereference(registerValue->type);
      if (!dereferencedType.has_value()) {
        crash(node, "Unable to dereference non-pointer type '{}'", LlvmName(registerValue->type));
      }
      return StackValue(registerValue->name, registerValue->type);
    } else if (auto stackValue = std::get_if<StackValue>(&value->value)) {
      Types::OptionalType dereferencedType = Types::Pool().dereference(registerValue->type);
      if (!dereferencedType.has_value()) {
        crash(node, "Unable to dereference non-pointer type '{}'", LlvmName(registerValue->type));
      }
      auto registerValue = std::get<RegisterValue>(toRegister(value, outputFile, environment).value);
      return StackValue(registerValue.name, *dereferencedType);
    } else {
      TODO("Dereferencing non-stack values?");
    }
  }

  Reference interpret(NodeIndex nodeIndex, Environment& environment, std::ostream& outputFile, StatementContext& context) {
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
        for (auto child : node.elements) {
          interpret(child, blockEnv, outputFile, context);
        }
        if (blockEnv.hasReturned) {
          environment.hasReturned = true;
        }
        return Reference::Void();
      }
      case TokenType::LeftSquareBracket: {
        TODO("Array literals");
      }
      case TokenType::LeftParen: {
        TODO("Tuples");
        // TODO: type inference
        std::vector<Reference> elements;
        for (auto element : node.elements) {
          elements.push_back(interpret(element, environment, outputFile, context));
        }
        bool isType = false;
        std::vector<TypeIndex> elementTypes;
        for (auto element : elements) {
          auto elementType = element.unboxType();
          if (!elementType) TODO("Value tuples");
          elementTypes.push_back(*elementType);
        }
        std::stringstream instruction;
        auto [typeIndex, _] = Types::Pool().tupleOf(elementTypes, instruction);
        globalsStack.push(instruction.str());
        return Reference(typeIndex);
      }
      default:
        TODO("Default for block nodes");
      }
    }
    case NodeType::Declaration: {
      auto node = parser.getDeclaration(nodeIndex);
      // NOTE: don't support using non-identifiers
      bool compileTime = parser.getToken(encoded.token)->type == TokenType::Colon;

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
        auto value = interpret(node.value, environment, outputFile, valueContext);

        if (!value.isComptime()) {
          crash(
            nodeIndex,
            "Unable to make comptime constant out of "
            "non-comptime value assigned to '{}'",
            definitionName);
        }

        // Allow double define because we checked earlier; functions are bound to environment for recursion
        // TODO: stubbing for mutual recursion
        environment.define(definitionName, value);
      } else {
        Reference definition = interpret(node.definition, environment, outputFile, context);

        StackValue* stackValue = std::get_if<StackValue>(&std::get<Reference*>(definition.value)->value);
        TypeIndex expectedType = stackValue->type;
        auto value = interpret(node.value, environment, outputFile, context);
        auto assignedType = value.isAssignableTo(expectedType);
        if (!assignedType) {
          crash(
            nodeIndex, "Unable to assign value of type '{}' to to variable '{}' of type '{}'", TypeName(value.getType()), stackValue->name, TypeName(stackValue->type));
        }
        stackValue->type = *assignedType;
        fmt::println(outputFile, "{} = alloca {}, align {}", definition, LlvmName(*assignedType), Types::Pool().getSizing(*assignedType).alignment.byteAlignment());

        auto loaded = toRegister(&value, outputFile, environment);
        fmt::println(outputFile, "store {} {}, ptr {}", LlvmName(*assignedType), loaded, definition);
      }

      // TODO: support assignment as expression???
      return Reference::Void();
    }
    case NodeType::Definition: {
      auto node = parser.getDefinition(nodeIndex);
      auto name = node.name->lexeme;
      TypeIndex type = Types::Pool().infer;
      if (node.type.has_value()) {
        StatementContext ctx(context);
        ctx.expectedType = std::nullopt;
        ctx.name = node.name->lexeme;
        if (auto typeIndex = interpret(node.type.value(), environment, outputFile, ctx).unboxType()) {
          type = *typeIndex;
        } else {
          crash(nodeIndex, "Type for identifier '{}' is not a type", node.name->lexeme);
        }
      }
      std::optional<Reference*> definition = environment.define(name, Reference(StackValue(node.name->lexeme, Types::Pool().infer)));
      if (definition) {
        return Reference(*definition);
      }

      crash(nodeIndex, "Definition for indentifier '{}' already exists", node.name->lexeme);
    }
    case NodeType::Literal: {
      auto node = parser.getLiteral(nodeIndex);
      auto token = node.token;
      if (token->type == TokenType::String) {
        TODO("Length-based strings");
        auto u8 = Types::Pool().u8;
        auto global = environment.makeGlobal(Types::Pool().multiPointerTo(u8));
        auto [stringValue, length] = escapeSourceString(token->lexeme, token);
        // TODO: use string types instead of C strings
        std::stringstream instruction;
        instruction << fmt::format("{} = global [{} x i8] c\"{}\" align 1\n", global, length, stringValue);
        globalsStack.push(instruction.str());
        auto sliceType = Types::Pool().sliceOf(Types::Pool().u8);
        RegisterValue val = environment.makeTemporary(sliceType);
        auto ref0 = Reference(val);
        auto ref1 = Reference(environment.makeTemporary(sliceType));
        auto ref2 = Reference(environment.makeTemporary(sliceType));
        fmt::println(outputFile, "{} = {} zeroinitializer", ref0, LlvmName(sliceType));
        fmt::println(outputFile, "{} = insertvalue {} {}, ptr {}, 0", ref1, LlvmName(sliceType), ref0, ref0);
        fmt::println(outputFile, "{} = insertvalue {} {}, {} {}, 1", ref2, LlvmName(sliceType), ref1, LlvmName(Types::Pool().usize), length);
        return ref2;
      }
      if (token->type == TokenType::NullTerminatedString) {
        auto global = environment.makeGlobal(Types::Pool().multiPointerTo(Types::Pool().u8));
        auto [stringValue, length] = escapeSourceString(token->lexeme, token);
        static std::string nullByte = "\\00";
        std::stringstream instruction;
        instruction << fmt::format("{} = global [{} x i8] c\"{}{}\" align 1\n", global, length + 1, stringValue, nullByte);
        globalsStack.push(instruction.str());
        return global;
        // Reference* ref =
        // Reference::literal(Types::Pool().multiPointerTo(Types::Pool().u8),
        // std::move(name)); return ref;
      }
      if (token->type == TokenType::Decimal) {
        float floatValue = std::stof(token->lexeme.data());
        return Reference(FloatLiteral(floatValue));
      }
      if (token->type == TokenType::Integer) {
        int64_t intVal = std::stoi(token->lexeme.data());
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
        std::optional<Reference*> value = environment.find(name);
        if (!value.has_value()) {
          crash(nodeIndex, "Identifier \"{}\" not defined", name);
        }
        return Reference(*value);
      }
      if (token->type == TokenType::Opaque) {
        TypeIndex type = Types::Pool().addOpaque(std::string(context.name.value_or("Anonymous Opaque")));
        return Reference(type);
      }
      crash(nodeIndex, "Unable to create literal from value");
    }
    case NodeType::BinaryOp: {
      auto node = parser.getBinaryOp(nodeIndex);
      auto opType = node.operation->type;
      auto leftVal = interpret(node.left, environment, outputFile, context);

      if (node.operation->isArithmeticOperation()) {
        auto rightVal = interpret(node.right, environment, outputFile, context);
        if (leftVal.isComptime() && rightVal.isComptime()) {
          TODO("comptime binary operator evaluation");
        }
        auto coercedType = Reference::coerceType(&leftVal, &rightVal);
        if (!coercedType.has_value()) {
          crash(nodeIndex, "Unable to perform binary operation on incompatible types");
        }
        auto [operandType, coercedLeft, coeredRight] = coercedType.value();
        auto leftLiteral = toRegister(&coercedLeft, outputFile, environment);
        auto rightLiteral = toRegister(&coeredRight, outputFile, environment);
        auto resultType = operandType;
        std::string binaryOperator;
        switch (opType) {
        case TokenType::Plus: {
          if (Types::Pool().isInt(operandType)) binaryOperator = "add";
          else if (Types::Pool().isFloat(operandType)) binaryOperator = "fadd";
          else crashBinOp(node.operation, &leftVal, &rightVal);
          break;
        }
        case TokenType::Minus: {
          if (Types::Pool().isInt(operandType)) binaryOperator = "sub";
          else if (Types::Pool().isFloat(operandType)) binaryOperator = "fsub";
          else crashBinOp(node.operation, &leftVal, &rightVal);
          break;
        }
        case TokenType::Div: {
          if (Types::Pool().isSignedInt(operandType)) binaryOperator = "sdiv";
          else if (Types::Pool().isInt(operandType)) binaryOperator = "udiv";
          else if (Types::Pool().isFloat(operandType)) binaryOperator = "fdiv";
          else crashBinOp(node.operation, &leftVal, &rightVal);
          break;
        }
        case TokenType::Mult: {
          if (Types::Pool().isInt(operandType)) binaryOperator = "mul";
          else if (Types::Pool().isFloat(operandType)) binaryOperator = "fmul";
          else crashBinOp(node.operation, &leftVal, &rightVal);
          break;
        }
        case TokenType::Lt: {
          resultType = Types::Pool()._bool;
          if (Types::Pool().isSignedInt(operandType)) binaryOperator = "icmp slt";
          else if (Types::Pool().isInt(operandType)) binaryOperator = "icmp ult";
          else if (Types::Pool().isFloat(operandType)) binaryOperator = "fcmp olt";
          else crashBinOp(node.operation, &leftVal, &rightVal);
          break;
        }
        case TokenType::Gt: {
          resultType = Types::Pool()._bool;
          if (Types::Pool().isSignedInt(operandType)) binaryOperator = "icmp sgt";
          else if (Types::Pool().isInt(operandType)) binaryOperator = "icmp ugt";
          else if (Types::Pool().isFloat(operandType)) binaryOperator = "fcmp ogt";
          else crashBinOp(node.operation, &leftVal, &rightVal);
          break;
        }
        case TokenType::Leq: {
          resultType = Types::Pool()._bool;
          if (Types::Pool().isSignedInt(operandType)) binaryOperator = "icmp sle";
          else if (Types::Pool().isInt(operandType)) binaryOperator = "icmp ule";
          else if (Types::Pool().isFloat(operandType)) binaryOperator = "fcmp ole";
          else crashBinOp(node.operation, &leftVal, &rightVal);
          break;
        }
        case TokenType::Geq: {
          resultType = Types::Pool()._bool;
          if (Types::Pool().isSignedInt(operandType)) binaryOperator = "icmp sge";
          else if (Types::Pool().isInt(operandType)) binaryOperator = "icmp uge";
          else if (Types::Pool().isFloat(operandType)) binaryOperator = "fcmp oge";
          else crashBinOp(node.operation, &leftVal, &rightVal);
          break;
        }
        case TokenType::DoubleEqual: {
          resultType = Types::Pool()._bool;
          if (Types::Pool().isInt(operandType) || Types::Pool().isPointer(operandType)) binaryOperator = "icmp eq";
          else if (Types::Pool().isFloat(operandType)) binaryOperator = "fcmp eq";
          else if (auto enumDef = Types::Pool().getEnum(operandType)) {
            auto rawType = (*enumDef)->rawType;
            if (Types::Pool().isInt(rawType)) {
              binaryOperator = "icmp eq";
            } else if (Types::Pool().isFloat(rawType)) {
              binaryOperator = "fcmp eq";
            } else {
              crashBinOp(node.operation, &leftVal, &rightVal);
            }
          } else crashBinOp(node.operation, &leftVal, &rightVal);
          break;
        }
        case TokenType::NotEqual: {
          resultType = Types::Pool()._bool;
          if (Types::Pool().isInt(operandType) || Types::Pool().isPointer(operandType)) binaryOperator = "icmp ne";
          else if (Types::Pool().isFloat(operandType)) binaryOperator = "fcmp ne";
          else if (auto enumDef = Types::Pool().getEnum(operandType)) {
            auto rawType = (*enumDef)->rawType;
            if (Types::Pool().isInt(rawType)) {
              binaryOperator = "icmp ne";
            } else if (Types::Pool().isFloat(rawType)) {
              binaryOperator = "fcmp ne";
            } else {
              crashBinOp(node.operation, &leftVal, &rightVal);
            }
          } else crashBinOp(node.operation, &leftVal, &rightVal);
          break;
        }
        default:
          crash(nodeIndex, "Unknown binary operation: {}", parser.getToken(nodeIndex)->lexeme);
        }
        auto resultName = Reference(environment.makeTemporary(resultType));
        fmt::println(outputFile, "{} = {} {} {}, {}", resultName, binaryOperator, LlvmName(operandType), leftLiteral, rightLiteral);
        return resultName;
      }

      auto leftType = leftVal.getType();

      switch (opType) {
      case TokenType::While: {
        auto rightVal = interpret(node.right, environment, outputFile, context);
        auto rightType = rightVal.getType();
        auto loopId = environment.addTemporary();
        auto loopHeader = environment.addLabel("while", loopId);
        auto loopBody = loopHeader + ".continue";
        auto endLabel = loopHeader + ".break";
        auto condition = interpret(node.left, environment, outputFile, context);
        if (condition.getType() != Types::Pool()._bool) {
          crash(
            nodeIndex,
            "Condition for while loop must be of type 'bool', "
            "but was of type '{}'",
            TypeName(condition.getType()));
        }

        fmt::println(outputFile, "{}", loopHeader);
        auto conditionLiteral = toRegister(&condition, outputFile, environment);

        fmt::println(outputFile, "br i1 {}, label %{}, label %{}\n{}:", conditionLiteral, loopBody, endLabel, loopBody);

        interpret(node.right, environment, outputFile, context);

        fmt::println(outputFile, "br label %{}\n{}:", loopHeader, endLabel);
        // TODO: consider value expression (see
        // https://ziglang.org/documentation/master/#while)
        return Reference::Void();
      }
      case TokenType::Assign: {
        auto value = interpret(node.right, environment, outputFile, context);
        auto assignee = interpret(node.left, environment, outputFile, context);

        auto stackValue = assignee.lValue();
        if (!stackValue) {
          crash(nodeIndex, "Can't assign to literal");
        }
        Types::OptionalType valueType = value.isAssignableTo(stackValue->type);
        if (!valueType) {
          crash(nodeIndex, "Can't assign value of type {} to symbol of type {}", TypeName(value.getType()), TypeName(stackValue->type));
        }
        if (Types::Pool().isFloat(*valueType)) {
          if (auto literal = value.unbox<IntLiteral>()) {
            value.value = FloatLiteral((*literal)->value);
          }
        }
        auto loaded = toRegister(&value, outputFile, environment);
        fmt::println(outputFile, "store {} {}, ptr {}", LlvmName(*valueType), loaded, assignee);

        // TODO: consider value
        return Reference::Void();
      }
      // TODO: slicing operations
      case TokenType::LeftSquareBracket: {
        // [left]right
        // ^
        if (parser.nodeTokenPrecedes(nodeIndex, node.left)) {
          // TODO: comptime evaluation
          auto sizeNode = parser.getNode(node.left);
          auto sizeToken = parser.getToken(sizeNode.token);
          if (sizeNode.nodeType != NodeType::Literal || sizeToken->type != TokenType::Integer) {
            crash(node.left, "Array size must be a non-negative int");
          }
          i32 arrayLength;
          std::from_chars(sizeToken->lexeme.data(), sizeToken->lexeme.data() + sizeToken->lexeme.length(), arrayLength);

          auto elementType = interpret(node.right, environment, outputFile, context);
          if (auto type = elementType.unboxType()) {
            TODO("Sized arrays");
            // return Reference(Types::Pool().sizedArrayOf(*elementType,
            // arrayLength));
          }

          crash(node.right, "Array element type must be a compile-time known type, but was of type '{}'", TypeName(elementType.getType()));
        }

        auto parameterNode = parser.getInputList(node.right);

        // left[right]
        if (auto boxedGeneric = std::get_if<Generic>(&leftVal.value)) {
          auto expectedArgLength = boxedGeneric->parameterNames.size();
          {
            auto actualArgLength = parameterNode.requiredInputs.size();
            if (expectedArgLength != actualArgLength) crash(node.right, "Expected {} generic arguments, but {} were provided", expectedArgLength, actualArgLength);
          }
          Environment genericEnvironment(&boxedGeneric->definitionEnvironment, std::string(""), true);
          std::vector<TypeIndex> argTypes(expectedArgLength);

          // TODO: optional inputs
          for (auto i = 0; i < boxedGeneric->parameterNames.size(); i++) {
            auto argNode = parameterNode.requiredInputs[i];
            auto argValue = interpret(argNode, environment, outputFile, context);

            auto paramName = boxedGeneric->parameterNames[i];
            if (auto typeIndex = argValue.unboxType()) {
              genericEnvironment.define(paramName, argValue);
              argTypes.push_back(*typeIndex);
            } else {
              TODO("Non-type generic parameters");
            }
          }

          for (auto param : parameterNode.optionalInputs) {
            TODO("Named generic parameters");
          }

          auto [tupleType, tupleIndex] = Types::TypePool().tupleOf(argTypes, outputFile);

          if (boxedGeneric->cache.contains(tupleIndex)) {
            return Reference(boxedGeneric->cache[tupleIndex]);
          } else {
            auto tupleName = fmt::format("{}", TypeName(tupleType));
            std::string genericName = fmt::format("{}{}", boxedGeneric->name, tupleName);
            genericName[genericEnvironment.prefix.size() - 1] = ']';
            genericName[genericEnvironment.prefix.size() - tupleName.size()] = '[';

            StatementContext genericContext = {.name = genericName, .expectedType = context.expectedType};
            // Match source syntax when debugging if possible
            genericEnvironment.prefix = boxedGeneric->definitionEnvironment.prefix + genericName;
            boxedGeneric->translationUnit.interpret(boxedGeneric->astNode, genericEnvironment, outputFile, genericContext);
            auto genericValue = interpret(boxedGeneric->astNode, genericEnvironment, outputFile, genericContext);
            auto cached = new Reference(genericValue);
            boxedGeneric->cache[tupleIndex] = cached;
            return Reference(cached);
          }
        }

        auto rightVal = interpret(node.right, environment, outputFile, context);
        auto rightType = rightVal.getType();
        Types::OptionalType arrayElement;

        // left[right]
        //     ^
        if (auto sliceElement = Types::Pool().sliceElementType(leftType)) {
          TypeIndex elementType = *sliceElement;
          TODO("Indices for slices");
        } else if (auto elementType = Types::Pool().multiPointerElement(leftType)) {
          auto leftLiteral = toRegister(&leftVal, outputFile, environment);
          auto rightLiteral = toRegister(&rightVal, outputFile, environment);
          if (!Types::Pool().isInt(rightType)) {
            crash(node.right, "Index must be an integer, but was of type '{}'", TypeName(rightType));
          }
          auto result = Reference(StackValue(environment.addTemporary(), *elementType));
          fmt::println(outputFile, "{} = getelementptr {}, ptr {}, {} {}", result, LlvmName(*elementType), leftLiteral, LlvmName(rightType), rightLiteral);
          return result;
        } else if (auto arrayElement = Types::Pool().sizedArrayElement(leftType)) {
          TODO("Access sized array elements");
        } else crashBinOp(node.operation, &leftVal, &rightVal);
        break;
      }
      case TokenType::LeftParen: {

        auto getArguments = [this, &environment, &outputFile](Types::TypeSpan paramTypes, Encodings::InputList argsNode) {
          std::vector<Reference> arguments(argsNode.requiredInputs.size());
          i32 i = 0;
          for (auto arg : argsNode.requiredInputs) {
            auto paramType = paramTypes[i++];
            StatementContext context{.expectedType = paramType};
            auto argument = interpret(arg, environment, outputFile, context);
            auto targetType = argument.isAssignableTo(paramType);
            if (!targetType) {
              crash(arg, fmt::runtime("Unable to assign argument of type '{}' to parameter of type '{}'"), TypeName(argument.getType()), TypeName(paramType));
            }
            // if (!) arguments.push_back();
            for (auto arg : argsNode.optionalInputs) {
              TODO("Passing in named arguments");
            }
          }
        };
        auto function = interpret(node.left, environment, outputFile, context);
        auto args = parser.getInputList(node.right);
        std::span<TypeIndex> expectedTypes;

        std::vector<Reference> arguments;

        if (auto func = function.unboxFunction()) {
          std::vector<std::string> parameters;

          auto funcType = (*func)->type;
          auto returnType = funcType.returnType;
          auto parameterTypes = Types::Pool().tupleElements(funcType.parameters);

          bool literalReturn = Types::Pool().isLlvmLiteralType(returnType);
          bool hasReturn = !Types::Pool().isVoid(returnType);
          i32 resultIndex;
          if (hasReturn) {
            resultIndex = environment.addTemporary();
            if (!literalReturn) {
              auto result = Reference(StackValue(resultIndex, returnType));
              auto align = Types::Pool().getSizing(returnType).alignment.byteAlignment();
              fmt::println(outputFile, "{} = alloca {}, align {}", result, LlvmName(returnType), align);
              parameters.push_back(fmt::format("ptr sret({}) align {} {}", LlvmName(returnType), align, result));
            }
          }

          for (i32 i = 0; i < arguments.size(); i++) {
            if (auto argType = arguments[i].isAssignableTo(parameterTypes[i])) {
              if (Types::Pool().isLlvmLiteralType(*argType)) {
                auto value = toRegister(&arguments[i], outputFile, environment);
                parameters.push_back(fmt::format("{} {}", LlvmName(*argType), value));
              } else {
                auto value = toByValPointer(arguments[i], outputFile, environment, args.requiredInputs[i]);
                parameters.push_back(fmt::format("ptr byval({}) {}", LlvmName(*argType), value));
              }
            } else {
              crash(
                args.requiredInputs[i],
                "Invalid argument in function call (can't pass argument of type {} to parameter of type {})",
                TypeName(arguments[i].getType()),
                TypeName(parameterTypes[i]));
            }
          }

          if (literalReturn) {
            auto result = Reference(RegisterValue(resultIndex, returnType));
            fmt::print(outputFile, "{} = ", result);
          }
          outputFile << "call ";
          if (literalReturn) {
            fmt::print(outputFile, "{} ", LlvmName(returnType));
          } else {
            outputFile << "void ";
          }
          fmt::println(outputFile, "{}({})", (*func)->globalName, fmt::join(parameters, ", "));

          if (Types::Pool().isVoid(returnType)) {
            return Reference::Void();
          }
          if (literalReturn) {
            return Reference(RegisterValue(resultIndex, returnType));
          }
          return Reference(StackValue(resultIndex, returnType));
          // TODO: optional arguments
        } else if (auto type = function.unboxType()) {
          auto structDefinition = Types::Pool().getStruct(*type);
          if (!structDefinition.has_value()) {
            crash(nodeIndex, "Can't construct non-struct type {}", TypeName(*type));
          }
          auto structLlvmName = LlvmName(*type);
          auto structName = TypeName(*type);
          RegisterValue structVal(environment.addTemporary(), *type);

          Types::Struct& definition = **structDefinition;
          if (definition.fields.size() != arguments.size()) {
            crash(
              nodeIndex,
              "Construction of type '{}' requires {} fields, but "
              "{} were passed in.",
              TypeName(*type),
              definition.fields.size(),
              arguments.size());
          }

          for (i32 i = 0; i < arguments.size(); i++) {
            auto field = definition.fields[i];
            if (auto argType = arguments[i].isAssignableTo(field.type)) {
              auto fieldValue = toRegister(&arguments[i], outputFile, environment);
              auto fieldValueName = fieldValue;
              auto fieldTypeLlvmName = LlvmName(field.type);
              if (i == 0) {
                Reference ref(structVal);
                fmt::println(outputFile, "{} = insertvalue {} zeroinitializer, {} {}, 0", ref, structLlvmName, fieldTypeLlvmName, fieldValueName);
              } else {
                Reference prevStruct(RegisterValue(structVal.name));
                structVal.name = environment.addTemporary();
                Reference ref(structVal);
                fmt::println(outputFile, "{} = insertvalue {} {}, {} {}, {}", ref, structLlvmName, prevStruct, fieldTypeLlvmName, fieldValueName, i);
              }
            } else {
              crash(
                args.requiredInputs[i],
                "Type mismatch for field '{}.{}' of type '{}'. Unable "
                "to assign value of type {}",
                structName,
                field.name,
                TypeName(field.type),
                TypeName(arguments[i].getType()));
            }
          }
          return Reference(structVal);
        } else {
          crash(nodeIndex, "Unable to call value of type '{}' as a function", TypeName(function.getType()));
        }
      }
      default:
        crash(nodeIndex, "Unknown binary operation {}", node.operation->lexeme);
      }
    }
    case NodeType::FunctionLiteral: {
      auto node = parser.getFunctionLiteral(nodeIndex);
      TypeIndex returnType = Types::Pool()._void;
      if (node.returnType.has_value()) {
        auto boxedReturnType = interpret(node.returnType.value(), environment, outputFile, context).unboxType();
        if (!boxedReturnType) {
          crash(nodeIndex, "Return type of function must be a compile-time known type");
        }
        returnType = *boxedReturnType;
      }

      std::stringstream instruction;
      std::string llvmName;
      bool forwardDeclare = !node.body.has_value();

      if (context.name.has_value()) {
        prefix = context.name.value();
        llvmName = forwardDeclare ? fmt::format("@\"{}\"", prefix) : environment.addConstant(prefix);
      } else {
        if (forwardDeclare) {
          crash(
            nodeIndex,
            "Can't forward declare anoymnous function; Anonymous functions "
            "require a body");
        }
        llvmName = fmt::format("@{}{}", environment.prefix, environment.nextGlobalIndex());
      }

      Environment functionEnvironment = Environment(&environment, prefix);
      StatementContext functionContext(context);
      functionContext.returnType = returnType;

      Types::LLVMStorage returnStorage = Types::Pool().storageType(returnType);
      // TODO: NEXT

      fmt::print(instruction, "{} ", (forwardDeclare ? "declare" : "define"));
      if (returnStorage == Types::LLVMStorage::LITERAL) {
        fmt::print(instruction, "{} ", LlvmName(returnType));
      } else {
        fmt::print(instruction, "void ");
      }
      fmt::print(instruction, "{}(", llvmName);

      bool hasParameters = false;
      if (returnStorage == Types::LLVMStorage::VARIABLE) {
        // TODO: factor out %return register
        fmt::print(instruction, "ptr noalias sret({}) align {} %return", LlvmName(returnType), Types::Pool().getSizing(returnType).alignment.byteAlignment());
        hasParameters = true;
      }
      auto parameters = parser.getInputList(node.parameters);
      std::vector<TypeIndex> parameterTypes;
      for (NodeIndex parameterIndex : parameters.requiredInputs) {
        if (hasParameters) {
          instruction << ", ";
        }
        hasParameters = true;

        auto parameterDefinition = parser.getDefinition(parameterIndex);
        if (!parameterDefinition.type.has_value()) {
          crash(parameterIndex, "Parameters must have a type");
        }

        auto parameterType = interpret(*parameterDefinition.type, functionEnvironment, outputFile, context).unboxType();
        if (!parameterType) {
          crash(parameterIndex, "Parameter type must be a compile time-known type");
        }

        // fmt::println("Parameter type: {} ({})",
        // Types::Pool().typeName(*parameterType), parameterType->value);
        parameterTypes.push_back(*parameterType);
        bool isLiteralParameter = Types::Pool().isLlvmLiteralType(*parameterType);

        std::string_view paramName = parameterDefinition.name->lexeme;

        auto typeName = *parameterType;
        Reference parameter =
          isLiteralParameter ? Reference(functionEnvironment.makeTemporary(*parameterType)) : Reference(StackValue(std::string(paramName), *parameterType));
        if (isLiteralParameter) {
          fmt::print(instruction, "{} {}", LlvmName(typeName), parameter);
        } else {
          // TODO: type alignment; for now align to s64
          fmt::print(instruction, "ptr byval({}) {}", LlvmName(typeName), parameter);
        }

        functionEnvironment.define(paramName, parameter);
      }

      for (NodeIndex parameterIndex : parameters.optionalInputs) {
        TODO("Named parameters/default values");
      }

      if (!forwardDeclare) {
        // TODO: attributes
        // https://llvm.org/docs/LangRef.html#function-attributes
        instruction << ") {\n";

        auto body = parser.getBlock(node.body.value());
        std::vector<TypeIndex> parameterTypes;
        for (auto statement : body.elements) {
          interpret(statement, functionEnvironment, instruction, functionContext);
        }

        if (!environment.hasReturned) {
          if (returnType == Types::Pool()._void) {
            instruction << "ret void\n";
          } else {
            crash(nodeIndex, "Return required for all code paths");
          }
        }
        // TODO: ensure return value
        instruction << "}\n\n";
      }

      globalsStack.push(instruction.str());
      auto [_, tupleType] = Types::Pool().tupleOf(parameterTypes, outputFile);
      auto functionType = Types::FunctionType(tupleType, returnType);
      // TypeIndex functionType = Types::Pool().addFunction();
      // fmt::println("Making function of type {}", functionType);
      return Reference(Function(functionType, llvmName));
    }
    case NodeType::Unary: {
      auto node = parser.getUnary(nodeIndex);
      if (node.operation == UnaryOps::CompilerBuiltin) {
        auto builtinToken = parser.getToken(parser.getNode(nodeIndex).token);
        auto argumentNodes = parser.getInputList(node.operand).requiredInputs;
        switch (builtinToken->type) {
        case TokenType::BUILTIN_NumCast: {
          auto arguments = argumentNodes | std::views::transform(
                                             [this, &environment, &outputFile, &context](const NodeIndex x) { return interpret(x, environment, outputFile, context); });
          if (arguments.size() != 2) {
            crash(nodeIndex, "Expected 2 arguments for builtin extend, but found {}", arguments.size());
          }
          auto object = arguments[0];
          auto targetType = arguments[1].unboxType();
          if (!targetType) {
            crash(argumentNodes[1], "Second arguments for builtin extend needs to be a compile-time known type");
          }

          auto objectType = arguments[0].getType();
          Reference resultName(environment.makeTemporary(*targetType));
          std::string_view instructionName = objectType.value > targetType->value ? "trunc" : "ext";
          auto downcast = objectType.value > targetType->value;
          std::string_view typePrefix;

          if (Types::Pool().isFloat(objectType) && Types::Pool().isFloat(*targetType)) {
            typePrefix = "fp";
          } else if (Types::Pool().isSignedInt(objectType) && Types::Pool().isSignedInt(objectType)) {
            typePrefix = "s";
          } else if (Types::Pool().isUnsignedInt(objectType) && Types::Pool().isUnsignedInt(*targetType)) {
            typePrefix = "z";
          } else {
            crash(nodeIndex, "Unable to cast from {} to {}", TypeName(objectType), TypeName(*targetType));
          }
          auto objectLiteral = toRegister(&object, outputFile, environment);
          fmt::println(outputFile, "{} = {}{} {} {} to {}", resultName, typePrefix, instructionName, LlvmName(objectType), objectLiteral, LlvmName(*targetType));

          return resultName;
          break;
        }
        case TokenType::BUILTIN_CImport: {
          if (argumentNodes.size() != 2) {
            crash(nodeIndex, "Builtin '@cImport' must take 2 literal arguments, but was given {}", argumentNodes.size());
          }

          auto includeFile = inputFilePath.stem().append(parser.getToken(argumentNodes[0])->lexeme);
          auto fileName = includeFile.string();
          auto fileNameView = std::string_view(fileName);
          CompilerContext::inst().c.includes.push_back(fileNameView);
          auto prefix = std::string(parser.getToken(argumentNodes[1])->lexeme);

          return Reference(cBindings(fileNameView, prefix, outputFile));
          break;
        }
        case TokenType::BUILTIN_CDefine:
        case TokenType::BUILTIN_CInclude:
        case TokenType::BUILTIN_CIncludeDir:
        case TokenType::BUILTIN_CLink: {
          auto& clangArgs = builtinToken->type == TokenType::BUILTIN_CDefine       ? CompilerContext::inst().c.defines
                            : builtinToken->type == TokenType::BUILTIN_CInclude    ? CompilerContext::inst().c.includes
                            : builtinToken->type == TokenType::BUILTIN_CIncludeDir ? CompilerContext::inst().c.includeDirs
                                                                                   : CompilerContext::inst().c.linkLibraries;
          if (argumentNodes.size() == 1 && parser.nodeType(argumentNodes[0]) == NodeType::Literal) {
            auto fileTokenIndex = parser.getNode(argumentNodes[0]).token;
            clangArgs.push_back(parser.getToken(fileTokenIndex)->lexeme);
          } else {
            crash(nodeIndex, "Builtin '@{}' must take one argument literal argument", builtinToken->lexeme);
          }
          return Reference::Void();
        }
        default: {
          crash(nodeIndex, "Malformed builtin '@{}'", builtinToken->lexeme);
        }
        }
      }
      if (node.operation == UnaryOps::Import) {
        auto fileName = parser.getToken(node.operand)->lexeme;
        Environment* import = compile(inputFilePath.parent_path().append(fileName), outputFileStream);
        return Reference(import);
      }
      auto value = interpret(node.operand, environment, outputFile, context);
      switch (node.operation) {
      case UnaryOps::Dereference:
        return Reference(dereference(&value, outputFile, environment, nodeIndex));
      case UnaryOps::Reference: {
        if (auto type = value.unboxType()) {
          return Reference(Types::Pool().pointerTo(*type));
        } else if (auto stackVal = value.lValue()) {
          return Reference(RegisterValue(stackVal->name, Types::Pool().pointerTo(stackVal->type)));
        }
      }
      case UnaryOps::Not: {
        if (auto boolean = value.unboxBool()) {
          return Reference(!boolean);
        }
        auto type = Types::Pool()._bool;
        if (value.getType() != type) {
          crash(node.operand, "Unary not operator '!' can only be used on boolean types; Operand was of type '{}'", TypeName(type));
        }
        auto valueName = toRegister(&value, outputFile, environment);
        auto resultName = Reference(environment.makeTemporary(type));
        fmt::println(outputFile, "{} = not i1 {}", resultName, valueName);
        return Reference(resultName);
      }
      case UnaryOps::Slice: {
        auto elementType = value.unboxType();
        if (!elementType.has_value()) {
          crash(node.operand, "Element type for array type (slice, fixed-size, etc) must be a compile-time known type, but was a '{}'", TypeName(value.getType()));
        }
        return Reference(Types::Pool().multiPointerTo(*elementType));
      }
      case UnaryOps::MultiPointer: {
        auto elementType = value.unboxType();
        if (!elementType.has_value()) {
          crash(
            node.operand,
            "Element type for array type (slice, fixed-size, etc) must be a "
            "compile-time known type, but was a '{}'",
            TypeName(value.getType()));
        }
        return Reference(Types::Pool().sliceOf(*elementType));
      }
      case UnaryOps::CompilerBuiltin:
      case UnaryOps::Import:
        break;
      }
    }
    case NodeType::If: {
      auto node = parser.getIf(nodeIndex);
      // TODO: add br instruction
      auto condition = interpret(node.condition, environment, outputFile, context);
      auto actualType = condition.getType();
      auto type = Types::Pool()._bool;
      if (actualType != type) {
        crash(node.condition, "Condition of an 'if' statement needs to be of type 'bool'");
      }

      auto comptime = condition.isComptime();
      auto blockIndex = environment.addTemporary();
      auto resultVariable = Reference(StackValue(blockIndex));
      auto ifLabel = environment.addLabel("if", blockIndex);
      auto endLabel = environment.addLabel("endif", blockIndex);
      auto elseLabel = environment.addLabel("else", blockIndex);

      // If
      std::stringstream ifInstruction;
      if (!comptime) {
        ifInstruction << ifLabel << ":\n";
      }
      auto ifResult = interpret(node.value, environment, ifInstruction, context);
      auto ifReturns = environment.hasReturned;
      environment.hasReturned = false;
      std::optional<TypeIndex> resultType = ifResult.getType();

      // Else
      auto hasElse = node.elseValue.has_value();
      std::stringstream elseInstruction;
      auto elseContext(context);
      elseContext.expectedType = resultType;
      Reference elseResult = node.elseValue.has_value() ? interpret(node.elseValue.value(), environment, elseInstruction, elseContext) : Reference::Void();
      auto elseReturns = environment.hasReturned;
      environment.hasReturned = false;

      if (hasElse && resultType) {
        auto elseType = elseResult.getType();
        resultType = Types::Pool().coerce(*resultType, elseType);
      }

      if (comptime) {
        auto conditionValue = condition.unboxBool().value();
        if (conditionValue) {
          outputFile << ifInstruction.str();
          environment.hasReturned = ifReturns;
          return ifResult;
        } else {
          outputFile << elseInstruction.str();
          environment.hasReturned = elseReturns;
          return elseResult;
        }
      }

      // Results
      auto voidType = Types::Pool()._void;
      auto hasResultType = (resultType.value_or(voidType) != voidType) && node.elseValue.has_value();
      auto loadedCondition = toRegister(&condition, outputFile, environment);

      if (node.elseValue.has_value()) {
        fmt::println(outputFile, "br i1 {}, label %{}, label %{}", loadedCondition, ifLabel, elseLabel);
        outputFile << ifInstruction.str();
        if (!ifReturns) {
          fmt::println(outputFile, "br label %{}", endLabel);
        }
        fmt::println(outputFile, "{}:", elseLabel);
        outputFile << elseInstruction.str();
        if (!elseReturns) {
          fmt::println(outputFile, "br label %{}", endLabel);
        }
      } else {
        fmt::println("br i1 {}, label %{}, label %{}", loadedCondition, ifLabel, endLabel);
        outputFile << ifInstruction.str();
        if (!ifReturns) {
          fmt::println("br label %{}", endLabel);
        }
      }
      if (ifReturns && elseReturns) {
        environment.hasReturned = true;
        return Reference(Never{});
      }

      if (resultType) {
        auto type = resultType.value();
        // TODO: is phi with poison needed here instead?
        if (ifReturns) {
          return elseResult;
        } else {
          return ifResult;
        }
        auto phiResult = Reference(environment.makeTemporary(type));
        fmt::println("{}  = phi {} [{}, %{}], [{}, %{}]", phiResult, LlvmName(type), ifLabel, ifResult, elseLabel, elseResult);
        return phiResult;
      }

      return Reference::Void();
    }
    case NodeType::Struct: {
      auto node = parser.getStruct(nodeIndex);
      // TODO: methods
      auto named = context.name.has_value();
      std::string llvmName = named ? environment.addGlobal(context.name.value()) : environment.addGlobal();
      std::stringstream typeInstruction;
      typeInstruction << llvmName << " = type {";
      std::string typeName = named ? std::string(context.name.value()) : "Anonymous type";
      auto [typeIndex, structIndex] = Types::Pool().makeStruct(typeName, llvmName);
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
          Types::OptionalType type = interpret(definitionNode.type.value(), environment, outputFile, context).unboxType();
          if (!type) {
            crash(nodeIndex, "Type for field '{}' must be known at compile time", fieldName);
          }

          // TODO: default values
          Types::Pool().getStruct(structIndex).defineField(fieldName, *type);
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
      return Reference(typeIndex);
    }
    case NodeType::DotAcces: {
      auto node = parser.getDotAccess(nodeIndex);
      Reference object = interpret(node.object, environment, outputFile, context);

      std::string_view fieldName = node.fieldName->lexeme;

      if (auto type = object.unboxType()) {
        if (auto enumDefinition = Types::Pool().getEnum(*type)) {
          if (auto value = (*enumDefinition)->get(fieldName)) {
            return Reference(IntLiteral(*value, *type));
          }
          crash(nodeIndex, "Unknown variant '{}' in enum '{}'", fieldName, TypeName(*type));
        }
        TODO("Static member variables");
      }

      if (auto import = object.unboxEnv()) {
        auto fileEnv = *import;
        if (auto value = fileEnv->find(fieldName)) {
          return Reference(*value);
        }
        auto members = fileEnv->defs | std::views::transform([](const auto& x) { return TypeName(x.second.getType()); });
        crash(nodeIndex, "Unable to find member '{}' in module\nAvailable fields are {}", fieldName, fmt::join(members, ", "));
      }

      // TODO: auto dereference pointers
      auto type = object.getType();
      Types::OptionalType dereferenced = Types::Pool().dereference(type);
      if (dereferenced.has_value()) {
        StackValue dereffed = dereference(&object, outputFile, environment, node.object);
        object.value = dereffed;
        type = *dereferenced;
      }
      auto boxedField = Types::Pool().getFieldIndex(type, fieldName);

      if (!boxedField.has_value()) {
        crash(node.fieldName, "No field '{}' found in struct '{}'", fieldName, TypeName(type));
      }

      auto [field, fieldIndex] = boxedField.value();
      auto fieldPointer = environment.addTemporary();
      TypeIndex fieldType = field->type;

      if (auto stackValue = object.unbox<StackValue>()) {
        auto result = Reference(StackValue(fieldPointer));
        fmt::println(outputFile, "{} = getelementptr inbounds {}, ptr {}, i32 0, i32 {}", result, LlvmName(type), object, fieldIndex);
        return result;
      } else if (auto registerValue = object.unbox<RegisterValue>()) {
        auto result = Reference(RegisterValue(fieldPointer));
        fmt::println(outputFile, "{} = extractvalue {} {}, {}", result, LlvmName(type), object, fieldIndex);
        return result;
      } else {
        TODO("error for getting struct field");
      }
    }
    case NodeType::InputList: {
      crash(nodeIndex, "Input list nodes shouldn't be directly interpreted");
    }
    case NodeType::Enum: {
      auto node = parser.getEnumDefinition(nodeIndex);
      // TODO: ADT
      TypeIndex rawType;
      if (node.rawType) {
        auto rawValue = interpret(*node.rawType, environment, outputFile, context);
        if (auto type = rawValue.unboxType()) {
          rawType = *type;
          if (!Types::Pool().isInt(rawType)) {
            crash(*node.rawType, "Enum raw type '{}' isn't an integer", TypeName(rawType));
          }
        } else {
          crash(*node.rawType, "Enum raw type doesn't refer to a compile time-known integer type; is a value of type '{}'", TypeName(rawValue.getType()));
        }
      } else {
        auto bitSize = std::bit_width(node.values.size());
        // TODO: zero-sized enum?
        // TODO: non-Po2-sized enums
        if (bitSize <= 8) {
          rawType = Types::TypePool().u8;
        } else if (bitSize <= 16) {
          rawType = Types::TypePool().u16;
        } else if (bitSize <= 32) {
          rawType = Types::TypePool().u32;
        } else if (bitSize <= 64) {
          rawType = Types::TypePool().u64;
        } else {
          crash(nodeIndex, "Unable to create enum with values that can't fit in 64 bits: {} values", node.values.size());
        }
      }

      auto [typeIndex, enumIndex] = Types::Pool().addEnum(rawType, context.name.has_value() ? std::string(context.name.value()) : "Anonymous Enum");
      i32 valueCount = 0;
      Types::Enum& enumDefinition = Types::Pool().getEnum(enumIndex);
      for (auto nameToken : node.values) {
        auto name = parser.getToken(nameToken)->lexeme;

        if (!enumDefinition.define(name, valueCount)) {
          crash(parser.getToken(nameToken), "Duplicate variant '{}' for enum '{}'", name, TypeName(typeIndex));
        }
        valueCount++;
      }
      return Reference(typeIndex);
    }
    case NodeType::MultiLineString: {
      TODO("Multi line string compilation");
    }
    }
    crash(nodeIndex, "Unknown node type");
  }

  std::pair<std::string, i32> escapeSourceString(std::string_view str, TokenPointer token) {
    std::string escaped;
    i32 byteLength = 0;
    escaped.reserve(str.size());

    bool isEscaping = false;

    // TODO: unicode support
    for (auto c : str) {
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
        default:
          crash(token, "Unkown escape sequence \\{}", c);
        }
        byteLength++;
      }
    }
    return {escaped, byteLength};
  }

  void crashBinOp(TokenPointer token, Reference* leftVal, Reference* rightVal) {
    crash(token, "Unable to perform binary operation '{}' on types '{}' and '{}'", token->lexeme, TypeName(leftVal->getType()), TypeName(rightVal->getType()));
  }

  Environment* run(std::ostream& outputFile) {
    StatementContext context;
    for (auto node : program) {
      interpret(node, fileEnvironment, outputFile, context);
      while (!globalsStack.empty()) {
        outputFile << globalsStack.front() << "\n";
        globalsStack.pop();
      }
    }
    return &fileEnvironment;
  }

  static std::string readFile(fs::path filePath) {
    std::ifstream inputFile(filePath);

    if (!inputFile.is_open()) {
      fmt::println(std::cerr, "Error: could not open the file {}", filePath.string());
      exit(1);
    }

    std::string fileContents = std::string(std::istreambuf_iterator<char>(inputFile), std::istreambuf_iterator<char>());

    return fileContents;
  }

  static Environment* compile(fs::path fileName, std::ofstream& outFile) {
    static std::unordered_map<std::string, Environment*> compiledFiles;
    fileName = fs::absolute(fileName);

    if (compiledFiles.contains(fileName)) {
      return compiledFiles[fileName];
    }

    std::string fileContents = readFile(fileName);

    Tokenizer tokenizer(fileContents, fileName);
    Parser parser(tokenizer);
    std::vector<NodeIndex> program = parser.parse();

    TranslationUnit interpreter(parser, program, fileName, outFile);
    return interpreter.run(outFile);
  }

  template <typename... Args> void crash(TokenPointer token, fmt::format_string<Args...> fmt, Args&&... args) {
    auto& out = std::cerr;
    auto location = parser.tokenizer.locationOf(token->lexeme);
    fmt::println(out, "Compiler error in file {} at line {}:{}", inputFilePath.string(), location.line, location.column);
    location.underline(out);
    fmt::println(out, fmt, std::forward<Args>(args)...);
    exit(1);
  }

  template <typename... Args> void crash(NodeIndex node, fmt::format_string<Args...> fmt, Args&&... args) {
    auto token = parser.getToken(node);
    fmt::println(std::cerr, "Crashing on substring {} for node {}", token->lexeme, (i32)parser.getNode(node).nodeType);
    crash(token, fmt, std::forward<Args>(args)...);
  }
};
