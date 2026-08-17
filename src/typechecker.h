#pragma once

#include "ast.h"
#include "common.h"
#include "fmt/ostream.h"
#include "fmt/ranges.h"
#include "parser.h"
#include "types.h"
#include "value.h"
#include <iostream>
#include <utility>

struct Compiler;

struct TypeChecker {
  struct TypeValue {
    TypeIndex type;
    bool lValue = false;
  };
  Compiler& compiler;
  using ReturnType = TypeValue;
  Environment& env;
  Parser& parser;
#define TYPECHECK_STACK \
  TypeIndex expected;   \
  NodeIndex nodeIndex;
  struct StackItems {
    TYPECHECK_STACK
  };
  union {
    StackItems stackItems;
    struct {
      TYPECHECK_STACK
    };
  };
  vector<ReturnType> astTypes;

  void setVisitedNode(NodeIndex index) {
    nodeIndex = index;
  }

  Reference compile(NodeIndex index, TypeIndex expected = Pool().infer);
  TypeIndex materialize(
    NodeIndex nodeIndex,
    string_view name = "",
    string_view linkageScope = ""
  );
  string_view currentFunctionLinkageScope();

  ReturnType block(Encodings::Block node) {
    return {Pool()._void};
  }

  ReturnType arrayLiteral(ChildSpan elements, NodeIndex elementType) {
    TypeIndex expectedElement =
      elementType ? materialize(elementType) : Pool().infer;
    if (auto sized = Pool().sizedArray(expected)) {
      auto coerced = Pool().coerce(expectedElement, sized->dereferencedType);
      if (!expectedElement) {
        crash(
          elementType,
          "Array literal element type expected to be {}, but was explicitly "
          "declared to be {}",
          TypeName(sized->dereferencedType),
          TypeName(expectedElement)
        );
      }
      expectedElement = coerced;
      if (sized->length != 0 && sized->length != elements.size()) {
        crash(
          nodeIndex,
          "Expected length {} for sized array type {}, but array literal has "
          "{} elements",
          sized->length,
          TypeName(expected),
          elements.size()
        );
      }
    }

    for (auto node : elements) {
      auto elementType = check(node, expectedElement).type;
      auto coerced = Pool().coerce(expectedElement, elementType);
      if (!coerced) {
        crash(
          node,
          "Unable to create array with elements of both type '{}' and '{}'",
          TypeName(expectedElement),
          TypeName(elementType)
        );
      }
      expectedElement = coerced;
    }

    // TODO: comptime arrays
    auto actualElement = Pool().isAssignable(expectedElement, Pool().infer);
    if (!actualElement) {
      crash(
        nodeIndex,
        "Unable to create array literal with type {}",
        TypeName(expectedElement)
      );
    }
    expectedElement = actualElement;
    for (auto node : elements) {
      check(node, expectedElement);
    }
    return {Pool().sizedArrayOf(expectedElement, elements.size())};
  }

  ReturnType& readType(NodeIndex node) {
    return astTypes[node.value - 1];
  }

  void setType(NodeIndex node, ReturnType type) {
    readType(node) = type;
  }

  ReturnType visit(NodeIndex node, TypeIndex expectedType) {
    auto currentType = readType(node);
    if (currentType.type == expectedType) {
      return currentType;
    }
    if (currentType.type) {
      if (!Pool().coerce(currentType.type, expectedType)) {
        crash(
          node,
          "Can't coerce expression of type '{}' to type '{}'",
          TypeName(currentType.type),
          TypeName(expectedType)
        );
      }
      if (expectedType == Pool().infer) {
        return currentType;
      }
    }
    StackItems tempStack = {.expected = expectedType, .nodeIndex = node};
    std::swap(tempStack, stackItems);
    log("Checking against type '{}'", TypeName(expected));
    auto result = astVisit(node, parser, *this);
    if (auto coerced = Pool().coerce(result.type, expectedType)) {
      result.type = coerced;
      setType(node, result);
    } else {
      crash(
        node,
        "Can't coerce expression of type '{}' to type '{}'",
        TypeName(result.type),
        TypeName(expectedType)
      );
    }
    stackItems = tempStack;
    return result;
  }

  ReturnType check(NodeIndex node, TypeIndex expectedType = Pool().infer) {
    return visit(node, expectedType);
  }

  ReturnType when(
    NodeIndex condition,
    span<pair<NodeIndex, NodeIndex>> cases,
    NodeIndex elseBody
  ) {
    auto conditionType = check(condition).type;
    ReturnType resultType{.type = Pool().infer, .lValue = true};
    if (elseBody) {
      cases = {cases.data(), cases.size() + 1};
    }
    for (auto [caseCondition, caseBody] : cases) {
      if (caseCondition) {
        auto caseConditionType = check(caseCondition, conditionType).type;
        if (!Pool().coerce(conditionType, caseConditionType)) {
          crash(
            caseCondition,
            "'when' case condition expected to be of type '{}', but was '{}'",
            TypeName(conditionType),
            TypeName(caseConditionType)
          );
        }
      }

      ReturnType result = check(caseBody);
      if (auto coereced = Pool().coerce(resultType.type, result.type)) {
        resultType.type = coereced;
      } else {
        resultType.type = Pool()._void;
      }
      if (result.type != Pool().never) {
        resultType.lValue = resultType.lValue && result.lValue;
        log("'when' can be lValue?: {}", resultType.lValue);
      }
    }
    log("Typechecker says 'when' is an lValue?: {}", resultType.lValue);
    return resultType;
  }

  ReturnType declaration(Encodings::Declaration node) {
    auto def = parser.getDefinition(node.definition);
    auto type =
      def.type ? materialize(def.type, def.name->lexeme) : Pool().infer;
    auto assignmentToken = parser.getToken(nodeIndex)->type;
    auto assigneeType = check(node.value, type).type;
    if (assignmentToken == TokenType::Colon && type == Pool().infer) {
      return {Pool()._void};
    }
    if (auto fullType = Pool().isAssignable(assigneeType, type)) {
      check(node.value, fullType);
    } else {
      crash(
        nodeIndex,
        "Unable to assign value of type {} to type {}",
        TypeName(assigneeType),
        TypeName(type)
      );
    }
    log(
      "Declaring '{}: {}'",
      def.name->lexeme,
      TypeName(check(node.value).type)
    );
    return {Pool()._void};
  }

  ReturnType definition(Encodings::Definition node) {
    if (node.type) check(node.type, Pool().type);
    return {Pool()._void};
  }

  ReturnType character(TokenPointer token) {
    return {Pool()._u8};
  }

  ReturnType string(TokenPointer token) {
    return {Pool().sliceOf(Pool()._u8)};
  }

  ReturnType nullString(TokenPointer token) {
    return {Pool().pointerTo(Pool()._u8)};
  }

  ReturnType decimal(TokenPointer token) {
    return {Pool().floatLiteral};
  }

  ReturnType integer(TokenPointer token) {
    return {Pool().intLiteral};
  }

  ReturnType hexInt(TokenPointer token) {
    return {Pool().intLiteral};
  }

  ReturnType boolean(bool value) {
    return {Pool()._bool};
  }

  ReturnType identifier(TokenPointer token) {
    if (auto value = env.find(token->lexeme)) {
      auto lValue = value->lValue() != nullptr;
      log("'{}' is l value?: {}", token->lexeme, lValue);
      return {value->getType(), lValue};
    }
    crash(token, "Undefined symbol '{}'", token->lexeme);
  }

  ReturnType opaque(TokenPointer token) {
    return {Pool().type};
  }

  ReturnType self(TokenPointer token) {
    return {Pool().type};
  }

  ReturnType undefined(TokenPointer token) {
    return {Pool().infer};
  }

  ReturnType cudaBuiltin(TokenPointer token, string_view call) {
    return {Pool()._u32};
  }

  ReturnType assign(NodeIndex left, NodeIndex right) {
    auto leftType = check(left);
    if (!leftType.lValue) {
      crash(nodeIndex, "Unable to assign to non-lvalue");
    }
    auto rightType = check(right, leftType.type);
    // TODO: does this matter?
    if (
      auto assignedType = Pool().isAssignable(leftType.type, rightType.type)
    ) {
      check(right, assignedType);
    }
    return {Pool()._void};
  }

  ReturnType binopAssign(NodeIndex left, NodeIndex right, TokenType opType) {
    auto leftType = check(left);
    if (!leftType.lValue) {
      crash(nodeIndex, "Unable to assign to non-lvalue");
    }
    expected = leftType.type;
    binopVisit(left, right, nodeIndex, opType, parser, *this);
    return {Pool()._void};
  }

  TypeIndex comparison(NodeIndex a, NodeIndex b) {
    auto aType = check(a).type;
    auto bType = check(b, aType).type;

    if (auto coerced = Pool().coerce(aType, bType)) {
      check(a, coerced);
      check(b, coerced);
      return {Pool()._bool};
    }

    crash(
      a,
      "Unable to do comparison on non-coercible types '{}' and '{}'",
      TypeName(aType),
      TypeName(bType)
    );
  }

  ReturnType equal(NodeIndex a, NodeIndex b) {
    return {comparison(a, b), false};
  }

  ReturnType notEqual(NodeIndex a, NodeIndex b) {
    return {comparison(a, b), false};
  }

  ReturnType lt(NodeIndex a, NodeIndex b) {
    return {comparison(a, b), false};
  }

  ReturnType gt(NodeIndex a, NodeIndex b) {
    return {comparison(a, b), false};
  }

  ReturnType leq(NodeIndex a, NodeIndex b) {
    return {comparison(a, b), false};
  }

  ReturnType geq(NodeIndex a, NodeIndex b) {
    return {comparison(a, b), false};
  }

  ReturnType findOperator(
    NodeIndex objectNode,
    string_view methodName,
    span<NodeIndex> arguments = {}
  ) {
    auto objectType = check(objectNode);
    // TODO: multiple resolutions
    if (
      auto [aType, method] = env.getMethod(objectType.type, methodName); method
    ) {
      check(objectNode, aType);
      auto function = method->unboxFunction();
      auto params = Pool().tupleElements(function->type.parameters);
      if (params.size() - 1 != arguments.size()) {
        crash(
          objectNode,
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
      for (auto i = 0; i < arguments.size(); i++) {
        if ((u32)(i + 1) >= params.size()) {
          crash(
            arguments[i],
            "Internal error: method '{}.{}' expected parameter {} but only "
            "has {} parameter(s)",
            TypeName(aType),
            methodName,
            i + 1,
            params.size()
          );
        }
        auto paramType = params[i + 1];
        check(arguments[i], paramType);
      }
      return {function->type.returnType};
    }

    vector<TypeName> argTypes;
    argTypes.push_back(TypeName(objectType.type));
    for (auto arg : arguments) {
      argTypes.push_back(TypeName(check(arg).type));
    }
    // TODO: highlight symbol
    crash(
      objectNode,
      "No method or built-in operator '{}' for type(s) {}",
      methodName,
      fmt::join(argTypes, ", ")
    );
  }

  ReturnType arithmeticOperation(
    NodeIndex a,
    NodeIndex b,
    string_view methodName
  ) {
    auto left = check(a).type;
    auto right = check(b).type;
    if (auto type = Pool().coerce(left, right)) {
      if (Pool().isFloat(type) || Pool().isInt(type)) {
        return {type};
      }
    }

    // TODO: multiple resolutions
    if (auto [aType, method] = env.getMethod(left, methodName); method) {
      check(a, aType);
      auto function = method->unboxFunction();
      auto params = Pool().tupleElements(function->type.parameters);
      if (params.size() != 2) {
        crash(
          a,
          "Unable to use method '{}.{}' as a(n) {} operator. "
          "{} operators must take 2 arguments, but this takes {}",
          TypeName(aType),
          methodName,
          methodName,
          methodName,
          methodName,
          params.size()
        );
      }
      if (params.size() <= 1) {
        crash(
          a,
          "Internal error: operator method '{}.{}' is missing its rhs "
          "parameter",
          TypeName(aType),
          methodName
        );
      }
      auto bType = params[1];
      check(b, bType);
      return {function->type.returnType};
    }

    // TODO: highlight symbol
    crash(
      a,
      "Unable to perform binary operation '{}' on types '{}' and '{}'",
      methodName,
      TypeName(left),
      TypeName(right)
    );
  }

  ReturnType add(NodeIndex a, NodeIndex b) {
    return arithmeticOperation(a, b, "add");
  }

  ReturnType subtract(NodeIndex a, NodeIndex b) {
    return arithmeticOperation(a, b, "subtract");
  }

  ReturnType multiply(NodeIndex a, NodeIndex b) {
    return arithmeticOperation(a, b, "multiply");
  }

  ReturnType power(NodeIndex base, NodeIndex exponent) {
    auto baseType = check(base).type;
    auto exponentType = check(exponent).type;
    if (!Pool().isNumber(baseType)) {
      crash(
        base,
        "Exponentiation requires a numeric base, but found '{}'",
        TypeName(baseType)
      );
    }
    if (!Pool().isInt(exponentType)) {
      crash(
        exponent,
        "Exponentiation requires an integer exponent, but found '{}'",
        TypeName(exponentType)
      );
    }
    return {baseType};
  }

  ReturnType divide(NodeIndex a, NodeIndex b) {
    return arithmeticOperation(a, b, "divide");
  }

  ReturnType leftDivide(NodeIndex a, NodeIndex b) {
    return arithmeticOperation(a, b, "leftDivide");
  }

  ReturnType remainder(NodeIndex a, NodeIndex b) {
    return arithmeticOperation(a, b, "remainder");
  }

  ReturnType logicOp(NodeIndex a, NodeIndex b) {
    auto boolean = Pool()._bool;
    auto left = check(a, boolean).type;
    auto right = check(b, boolean).type;
    if (Pool().coerce(left, right) == boolean) {
      return {boolean};
    }
    crash(
      a,
      "Logical operations must take 2 '{}', but types were '{}' and '{}'",
      TypeName(boolean),
      TypeName(left),
      TypeName(right)
    );
  }

  ReturnType logicAnd(NodeIndex a, NodeIndex b) {
    return logicOp(a, b);
  }

  ReturnType logicOr(NodeIndex a, NodeIndex b) {
    return logicOp(a, b);
  }

  ReturnType bitwiseOp(NodeIndex a, NodeIndex b) {
    auto left = check(a, expected).type;
    auto right = check(b, expected).type;
    // left = Pool().rawType(left);
    // right = Pool().rawType(right);
    auto resultType = Pool().coerce(left, right);
    if (resultType && Pool().isInt(resultType)) {
      return {resultType};
    }
    crash(
      nodeIndex,
      "Bitwise operations must take 2 of the same integer type, but types were "
      "'{}' and '{}'",
      TypeName(left),
      TypeName(right)
    );
  }

  ReturnType bitwiseAnd(NodeIndex a, NodeIndex b) {
    return bitwiseOp(a, b);
  }

  ReturnType bitwiseOr(NodeIndex a, NodeIndex b) {
    return bitwiseOp(a, b);
  }

  ReturnType xorOp(NodeIndex a, NodeIndex b) {
    return bitwiseOp(a, b);
  }

  ReturnType shiftOp(NodeIndex a, NodeIndex b) {
    // TODO: log-sized ints
    return bitwiseOp(a, b);
  }

  ReturnType shiftLeft(NodeIndex a, NodeIndex b) {
    return shiftOp(a, b);
  }

  ReturnType shiftRight(NodeIndex a, NodeIndex b) {
    return shiftOp(a, b);
  }

  ReturnType whileLoop(NodeIndex condition, NodeIndex body) {
    return {Pool()._void};
  }

  ReturnType sizedArray(NodeIndex length, NodeIndex type) {
    return {Pool().type};
  }

  ReturnType checkArrayIndex(
    NodeIndex index,
    TypeIndex elementType,
    bool lValue
  ) {
    auto indexType = check(index).type;
    if (
      indexType == Pool().unsignedRangeLiteral ||
      indexType == Pool().rangeLiteral
    ) {
      return {Pool().sliceOf(elementType), false};
    } else if (!Pool().isInt(indexType)) {
      crash(
        index,
        "Can't index array or slice with type '{}'; Must be an int or range",
        TypeName(indexType)
      );
    }

    return {elementType, lValue};
  }

  static Logger log;

  ReturnType index(NodeIndex object, NodeIndex index);

  ReturnType call(NodeIndex function, Encodings::ArgumentList args) {
    auto callerType = check(function).type;
    if (callerType == Pool().type) {
      auto constructedType = materialize(function);
      // TODO: number conversions
      return {constructedType};
    } else if (auto functionType = Pool().functionType(callerType)) {
      return {functionType->returnType};
    } else if (auto functionType = Pool().unboxBoundFunction(callerType)) {
      return {functionType->returnType};
    } else {
      if (args.positional.size() == 1 && args.named.empty()) {
        return multiply(function, args.positional[0]);
      }
      crash(
        nodeIndex,
        "Unable to call/construct/multiply type '{}' with {} positional and "
        "{} "
        "named arguments",
        TypeName(callerType),
        args.positional.size(),
        args.named.size()
      );
    }
  }

  ReturnType exclusiveRange(NodeIndex lowerBound, NodeIndex upperBound) {
    // TODO: unsigned
    return {Pool().rangeLiteral};
  }

  ReturnType align(NodeIndex alignmentIndex, NodeIndex valueIndex) {
    check(alignmentIndex, Pool().intLiteral);
    return check(valueIndex);
  }

  ReturnType impl(NodeIndex targetType, NodeIndex block) {
    return {Pool().type};
  }

  ReturnType generic(Encodings::ParameterList params, NodeIndex value) {
    for (auto parameter : params.requiredParameters) {
      auto definition = parser.getDefinition(parameter);
      if (!definition.type) {
        crash(parameter, "Generic parameters must have a type");
      }
      check(definition.type, Pool().type);
    }
    if (!params.optionalParameters.empty()) {
      crash(nodeIndex, "Generic parameters cannot have default values");
    }
    return {Pool().generic};
  }

  void invalidate() {
    std::fill(astTypes.begin(), astTypes.end(), ReturnType{});
  }

  ReturnType functionLiteral(
    Encodings::ParameterList params,
    NodeIndex returnIndex,
    NodeIndex body
  ) {
    auto functionScope = currentFunctionLinkageScope();
    vector<TypeIndex> paramTypes;
    paramTypes.reserve(
      params.requiredParameters.size() + params.optionalParameters.size()
    );
    for (auto required : params.requiredParameters) {
      auto definition = parser.getDefinition(required);
      check(definition.type, Pool().type);
      paramTypes.push_back(
        materialize(definition.type, definition.name->lexeme, functionScope)
      );
    }

    for (auto _ : params.optionalParameters) {
      TODO("Default parameters in function literals");
    }
    auto [_, tupleType] = Pool().tupleOf(std::move(paramTypes));
    TypeIndex returnType = Pool()._void;
    if (returnIndex) {
      check(returnIndex, Pool().type);
      returnType = materialize(returnIndex, "return", functionScope);
    }
    return {
      Pool().addFunction({.parameters = tupleType, .returnType = returnType})
    };
  }

  ReturnType numCast(Encodings::ArgumentList args) {
    auto inType = check(args.positional[0]).type;
    TypeIndex outType = expected;
    if (args.positional.size() == 2) {
      outType = materialize(args.positional[1]);
    }
    log("Numcasting to type {}", TypeName(outType));
    if (!Pool().isNumber(inType)) {
      crash(
        nodeIndex,
        "Unable to do @numCast on non-numeric type {}",
        TypeName(inType)
      );
    }
    if (!Pool().isNumber(outType)) {
      crash(
        nodeIndex,
        "Unable to @numCast to non-numeric type {}",
        TypeName(outType)
      );
    }
    bool can = Pool().isNumber(inType) && Pool().isNumber(outType);
    // bool can = Pool().isUnsignedInt(inType) && Pool().isUnsignedInt(outType);
    // can = can || Pool().isSignedInt(inType) && Pool().isSignedInt(outType);
    // can = can || Pool().isFloat(inType) && Pool().isFloat(outType);
    // can = can || Pool().isInt(inType) && Pool().isFloat(outType);
    // can = can || Pool().isFloat(inType) && Pool().isInt(outType);
    if (can) return {outType};
    crash(
      nodeIndex,
      "Unable to @numCast from type {} to {}",
      TypeName(inType),
      TypeName(outType)
    );
  }

  ReturnType bitCast(Encodings::ArgumentList args) {
    auto inType = check(args.positional[0]).type;
    TypeIndex outType = expected;
    if (args.positional.size() == 2) {
      outType = materialize(args.positional[1]);
    }
    if (Pool().isInfer(outType)) {
      crash(nodeIndex, "Unable to perform @bitCast to unknown type");
    }
    auto inSize = Pool().getSizing(inType);
    auto outSize = Pool().getSizing(outType);
    if (inSize.byteSize != outSize.byteSize) {
      crash(
        nodeIndex,
        "Unable to do @numCast on non-numeric type {}",
        TypeName(inType),
        inSize.byteSize,
        TypeName(outType),
        outSize.byteSize
      );
    }
    return {outType};
  }

  ReturnType cImport(Encodings::ArgumentList args) {
    return {Pool().environment};
  }

  ReturnType crashBuiltin() {
    return {Pool().never};
  }

  ReturnType cudaPtx(NodeIndex module) {
    check(module, Pool().cudaEnvType);
    return {Pool().pointerTo(Pool()._u8)};
  }

  ReturnType cDefine(Encodings::ArgumentList args) {
    return {Pool()._void};
  }

  ReturnType cUndef(Encodings::ArgumentList args) {
    return {Pool()._void};
  }

  ReturnType cInclude(Encodings::ArgumentList args) {
    return {Pool()._void};
  }

  ReturnType cIncludeDir(Encodings::ArgumentList args) {
    return {Pool()._void};
  }

  ReturnType link(Encodings::ArgumentList args) {
    return {Pool()._void};
  }

  ReturnType linkDir(Encodings::ArgumentList args) {
    return {Pool()._void};
  }

  ReturnType type(NodeIndex node) {
    return {Pool().type};
  }

  ReturnType import(TokenPointer fileName) {
    return {Pool().environment};
  }

  ReturnType dereference(NodeIndex operand) {
    auto value = check(operand);
    if (auto dereffed = Pool().dereference(value.type)) {
      return {dereffed, true};
    }
    crash(
      operand,
      "Can't dereference non-pointer type {}",
      TypeName(value.type)
    );
  }

  ReturnType reference(NodeIndex operand) {
    auto value = check(operand);
    if (value.type == Pool().type) {
      return {Pool().type};
    } else if (value.lValue) {
      return {Pool().pointerTo(value.type)};
    }
    crash(operand, "Can't get pointer to non-lvalue");
  }

  ReturnType unaryNot(NodeIndex operand) {
    check(operand, Pool()._bool);
    return {Pool()._bool};
  }

  ReturnType sliceType(NodeIndex operand) {
    check(operand, Pool().type);
    return {Pool().type};
  }

  // [^]a
  ReturnType multiPointerTo(NodeIndex operand) {
    auto value = check(operand);
    if (value.type == Pool().type) {
      return {Pool().type};
    } else if (value.lValue) {
      return {Pool().multiPointerTo(value.type)};
    }
    crash(operand, "Can't get pointer to non-lvalue");
  }

  // a[^]
  ReturnType multiPointerFrom(NodeIndex operand) {
    auto value = check(operand);
    if (auto sliceElement = Pool().sliceElementType(value.type)) {
      return {Pool().multiPointerTo(sliceElement)};
    } else if (auto sizedArray = Pool().sizedArray(value.type)) {
      if (!value.lValue) {
        crash(operand, "Can't take multi pointer from non-lvalue sized array");
      }
      return {Pool().multiPointerTo(sizedArray->dereferencedType)};
    } else {
      crash(
        operand,
        "Can't take multipointer from type '{}'",
        TypeName(value.type)
      );
    }
  }

  ReturnType unaryMinus(NodeIndex operand) {
    auto value = check(operand, expected);
    if (Pool().isNumber(value.type)) {
      return {value.type};
    }

    return findOperator(operand, "negative");
  }

  ReturnType bitwiseNot(NodeIndex operand) {
    auto value = check(operand, expected);
    if (Pool().isInt(value.type)) {
      return {value.type};
    }

    TODO("bitwise not for non-int types");
  }

  ReturnType makeSlice(NodeIndex operand) {
    auto value = check(operand);
    log("Making slice from type {}", TypeName(value.type));
    if (auto sizedArray = Pool().sizedArray(value.type)) {
      if (!value.lValue)
        crash(operand, "Unable to slice non-lValue sized array");

      return {Pool().sliceOf(sizedArray->dereferencedType)};
    }

    crash(
      operand,
      "Unable to slice type '{}' because it isn't a sized array",
      TypeName(value.type)
    );
  }

  ReturnType returnExpr(NodeIndex value) {
    if (auto returnType = env.returnType()) {
      bool hasValue = (bool)value;
      if (hasValue != (returnType != Pool()._void)) {
        crash(value, "Function with 'void' return type can't return a value");
      }
      if (value) {
        check(value, returnType);
      }
    }

    // TODO: does lvalue make sense here?
    return {Pool().never, true};
  }

  ReturnType usingExpr(NodeIndex operand) {
    auto value = compile(operand);
    if (auto imported = value.unboxEnv()) {
      env.usings.push_back(imported);
      for (auto nested : imported->usings) {
        env.usings.push_back(nested);
      }
      return {Pool()._void};
    }
    return {Pool()._void};
  }

  ReturnType cudaImport(TokenPointer fileName) {
    auto filePath =
      fs::absolute(
        parser.tokenizer.inputFilePath.parent_path() / fileName->lexeme
      )
        .lexically_normal();
    return {Pool().cudaEnvType};
  }

  ReturnType ifExpr(Encodings::If node) {
    check(node.condition, Pool()._bool);
    if (node.elseClause) {
      auto ifType = check(node.ifClause);
      auto elseType = check(node.elseClause);
      auto resultType = Pool().coerce(ifType.type, elseType.type);
      resultType = Pool().coerce(expected, resultType);
      if (resultType) {
        check(node.ifClause, resultType);
        check(node.elseClause, resultType);
        return {resultType, ifType.lValue && elseType.lValue};
      }
    }
    return {Pool()._void};
  }

  ReturnType structExpr(Encodings::Struct node) {
    return {Pool().type};
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
      "Type checker error in file {} at line {}:{}",
      parser.tokenizer.inputFilePath.string(),
      location.line,
      location.column
    );
    location.underline(out);
    fmt::println(out, fmt, std::forward<Args>(args)...);

    log("Crashed node index: {}", nodeIndex.value);
    // dumpTypes(out);
    abort();
  }

  void dumpTypes(std::ostream& out = std::cerr) {
    if (!log.canLog()) return;
    u32 i = 1;
    for (auto type : astTypes) {
      if (!type.type) {
        i++;
        continue;
      }
      fmt::println(out, "{:<6}: {}", i, TypeName(type.type));
      i++;
    }
  }

  template <typename... Args>
  [[noreturn]] void crash(
    NodeIndex node,
    fmt::format_string<Args...> fmt,
    Args&&... args
  ) {
    crash(parser.getToken(node), fmt, std::forward<Args>(args)...);
  }

  ReturnType dotAccess(Encodings::DotAccessor node) {
    auto targetType = expected;
    if (!node.object) {
      if (Pool().getEnum(targetType)) {
        return {targetType};
      } else {
        crash(
          nodeIndex,
          "Prefix operator dot access requires an inferred enum type, but "
          "inferred type was '{}'",
          TypeName(targetType)
        );
      }
    }
    auto [type, lValue] = check(node.object);
    targetType = type;

    auto fieldName = node.fieldName->lexeme;
    if (targetType == Pool().type) {
      auto type = materialize(node.object);
      if (auto enumDefinition = Pool().getEnum(type)) {
        if (auto value = enumDefinition->get(fieldName)) {
          return {type};
        }
        crash(
          nodeIndex,
          "Unknown variant '{}' in enum '{}'",
          fieldName,
          TypeName(type)
        );
      } else if (auto member = env.getStatic(type, fieldName)) {
        return {member->getType()};
      } else {
        crash(
          node.fieldName,
          "Unable to find type member '{}.{}'",
          TypeName(type),
          fieldName
        );
      }
    }

    if (targetType == Pool().environment) {
      auto object = compile(node.object, targetType);
      if (auto env = object.unboxEnv()) {
        if (auto object = env->find(fieldName)) {
          return {object->getType(), object->lValue() != nullptr};
        }
        env->debug();
        fmt::println("No definition for name '{}'; Alternatives:", fieldName);
        for (auto [name, _] : env->defs) {
          fmt::println("\t{}", name);
        }
        crash(node.fieldName, "No definition for name '{}'", fieldName);
      }
      crash(node.fieldName, "Internal error: missing environment");
    }

    if (targetType == Pool().cudaEnvType) {
      auto object = compile(node.object, targetType);
      if (auto env = object.unbox<CudaEnv>()) {
        if (auto object = env->env->find(fieldName)) {
          return {object->getType()};
        }
        env->env->debug();
        crash(
          node.fieldName,
          "Cuda environment missing definition for name '{}'",
          fieldName
        );
      }
      crash(node.fieldName, "Internal error: missing environment");
    }

    if (auto dereffed = Pool().dereference(targetType)) {
      targetType = dereffed;
      lValue = true;
    }
    auto field = Pool().getFieldPath(targetType, fieldName);
    if (field) {
      return {field.type, lValue};
    }
    if (
      auto [selfType, method] = env.getMethod(targetType, fieldName); method
    ) {
      auto methodType = method->unboxFunction();
      if (
        auto dereffedPtr = Pool().dereference(selfType);
        dereffedPtr == targetType && !lValue
      ) {
        crash(
          node.object,
          "Unable to bind non-lValue to first argument of type {}",
          TypeName(selfType)
        );
      }
      return {Pool().boundFunction(methodType->type)};
    }
    crash(
      node.fieldName,
      "Unable to find field '{}' for type '{}'",
      fieldName,
      TypeName(targetType)
    );
  }

  ReturnType argList(NodeIndex nodeIndex) {
    if (expected == Pool().infer) {
      crash(nodeIndex, "Can't evaluate value tuple without expected type");
    }

    auto structDefinition = Pool().getStruct(expected);
    if (!structDefinition && !Pool().unbox<Union>(expected)) {
      crash(
        nodeIndex,
        "Can't construct non-aggregate type {}",
        TypeName(expected)
      );
    }
    return {expected};
  }

  ReturnType enumExpr(Encodings::Enum node) {
    return {Pool().type};
  }

  ReturnType multiLineString(NodeIndex nodeIndex) {
    return {Pool().sliceOf(Pool()._u8)};
  }

  ReturnType forLoop(Encodings::ForLoop node) {
    auto iteratorType = check(node.iterator).type;
    if (
      Pool().sliceElementType(iteratorType) ||
      iteratorType == Pool().rangeLiteral ||
      iteratorType == Pool().unsignedRangeLiteral
    ) {
      return {Pool()._void};
    }
    crash(
      node.iterator,
      "iterator for for loop must be either a range literal or a slice, but "
      "was of type '{}'",
      TypeName(iteratorType)
    );
  }

  ReturnType apply(NodeIndex functionNode, NodeIndex argNode) {
    auto callerType = check(functionNode).type;

    // TODO: bound function
    log("Applying function type {}", TypeName(callerType));
    if (auto unboundFunction = Pool().unbox<FunctionType>(callerType)) {
      auto params = Pool().tupleElements(unboundFunction->parameters);
      // TODO: default args
      if (params.size() != 1) {
        crash(
          functionNode,
          "Can only apply functions that take 1 parameter, but provided "
          "function takes {}",
          params.size()
        );
      }
      if (params.empty()) {
        crash(
          functionNode,
          "Internal error: unary function lost its parameter"
        );
      }
      check(argNode, params[0]);
      return {unboundFunction->returnType};
    } else if (auto boundFunction = Pool().unboxBoundFunction(callerType)) {
      auto params = Pool().tupleElements(boundFunction->parameters);
      // TODO: default args
      if (params.size() != 2) {
        crash(
          functionNode,
          "Can only apply methods that take 1 non-bound parameter, but "
          "provided "
          "function takes {}",
          params.size() - 1
        );
      }
      if (params.size() <= 1) {
        crash(
          functionNode,
          "Internal error: bound method lost its non-self parameter"
        );
      }
      check(argNode, params[1]);
      return {boundFunction->returnType};
    } else {
      return multiply(functionNode, argNode);
    }
  }

  ReturnType nullPointer() {
    return {Pool().pointerTo(Pool()._void)};
  }

  ReturnType builtinName(NodeIndex expr) {
    auto exprType = check(expr);
    if (!Pool().getEnum(exprType.type)) {
      crash(
        expr,
        "Type of argument to @name needs to be an enum, but '{}' is not",
        TypeName(exprType.type)
      );
    }
    return {Pool().sliceOf(Pool()._u8)};
  }

  TypeChecker(Compiler& compiler, Environment& env, Parser& parser)
      : compiler(compiler), env(env), parser(parser) {
    expected = TypeIndex::null();
    nodeIndex = {0};
    astTypes.resize(parser.nodes.size());
  }

  ReturnType sizeOf(NodeIndex type) {
    return {Pool().intLiteral};
  }

  ReturnType alignOf(NodeIndex type) {
    return {Pool().intLiteral};
  }

  ReturnType bitSize(NodeIndex type) {
    return {Pool().intLiteral};
  }

  ReturnType ptrCast(NodeIndex arg) {
    if (!expected) {
      crash(nodeIndex, "Unable to @ptrCast without expected type");
    }

    auto argType = check(arg).type;
    if (!Pool().dereference(argType) || !Pool().multiPointerElement(argType)) {
      crash(
        arg,
        "Unable to @ptrCast from non-pointer type '{}' to expected type '{}'",
        TypeName(argType),
        TypeName(expected)
      );
    }

    if (
      !Pool().dereference(expected) || !Pool().multiPointerElement(expected)
    ) {
      crash(
        nodeIndex,
        "Unable to @ptrCast from type '{}' to non-pointer type '{}'",
        TypeName(argType),
        TypeName(expected)
      );
    }
    return {expected};
  }

  ReturnType bInclude(TokenPointer fileName) {
    return {Pool().u8slice};
  }

  ReturnType rawValue(NodeIndex value) {
    return {Pool().rawType(check(value).type)};
  }
};

static_assert(AstVisitor<TypeChecker>, "TypeChecker must implement AstVisitor");
