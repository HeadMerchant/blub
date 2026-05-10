#pragma once

#include "ast.h"
#include "fmt/ranges.h"
#include "parser.h"
#include "types.h"
#include "value.h"

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
  TypeIndex expected;
  span<ReturnType> astTypes;
  NodeIndex nodeIndex;

  void setVisitedNode(NodeIndex index) {
    nodeIndex = index;
  }

  TypeIndex materialize(NodeIndex nodeIndex);

  ReturnType block(Encodings::Block& node) {
    return {Pool()._void};
  }

  ReturnType arrayLiteral(Encodings::Block& node) {
    TypeIndex expectedElement = Pool().infer;
    if (auto sized = Pool().sizedArray(expected)) {
      expectedElement = sized->dereferencedType;
      if (sized->length != 0 && sized->length != node.elements.size()) {
        parser.crash(
          nodeIndex,
          "Expected length {} for sized array type {}, but array literal has "
          "{} elements",
          sized->length,
          TypeName(expected),
          node.elements.size()
        );
      }
    }

    for (auto node : node.elements) {
      auto elementType = check(node, expectedElement).type;
      auto coerced = Pool().coerce(expectedElement, elementType);
      if (!coerced) {
        parser.crash(
          node,
          "Unable to create array with elements of both type '{}' and '{}'",
          TypeName(expectedElement),
          TypeName(elementType)
        );
      }
      expectedElement = coerced;
    }

    for (auto node : node.elements) {
      pushType(node, expectedElement);
    }
    return {Pool().sizedArrayOf(expectedElement, node.elements.size())};
  }

  void setType(NodeIndex node, TypeIndex type) {
    astTypes[node.value].type = type;
  }

  ReturnType visit(
    NodeIndex node,
    TypeIndex expectedType,
    TypeIndex skipValue
  ) {
    if (
      astTypes[node.value].type && astTypes[node.value].type != expectedType
    ) {
      return {expectedType};
    }
    auto oldType = expected;
    NodeIndex previousNode = nodeIndex;
    expected = expectedType;
    auto result = astVisit(node, parser, *this);
    if (auto coerced = Pool().coerce(result.type, expectedType)) {
      setType(node, coerced);
    } else {
      parser.crash(
        node,
        "Can't coerce expression to type '{}'",
        TypeName(expectedType)
      );
    }
    expected = oldType;
    nodeIndex = previousNode;
    return result;
  }

  ReturnType pushType(NodeIndex node, TypeIndex expectedType) {
    return visit(node, expectedType, expectedType);
  }

  ReturnType check(NodeIndex node, TypeIndex expectedType = Pool().infer) {
    return visit(node, expectedType, TypeIndex::null());
  }

  ReturnType when(NodeIndex condition, span<pair<NodeIndex, NodeIndex>> cases) {
    auto conditionType = check(condition).type;
    ReturnType resultType{.type = Pool().infer, .lValue = true};
    for (auto [caseCondition, caseBody] : cases) {
      auto caseConditionType = check(caseCondition, conditionType).type;
      if (!Pool().coerce(conditionType, caseConditionType)) {
        parser.crash(
          caseCondition,
          "'when' case condition expected to be of type '{}', but was '{}'",
          TypeName(conditionType),
          TypeName(caseConditionType)
        );
      }

      ReturnType result = check(caseBody);
      if (auto coereced = Pool().coerce(resultType.type, result.type)) {
        resultType.type = coereced;
      } else {
        resultType.type = Pool()._void;
      }
      if (result.type != Pool().never) {
        resultType.lValue &= result.lValue;
      }
    }
    return resultType;
  }

  ReturnType declaration(Encodings::Declaration& node) {
    parser.crash(nodeIndex, "Internal error; Can't typecheck declaration");
  }

  ReturnType definition(Encodings::Definition& node) {
    parser.crash(nodeIndex, "Internal error; Can't typecheck definition");
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
      return {value->getType(), value->lValue().has_value()};
    }
    parser.crash(token, "Undefined symbol '{}'", token->lexeme);
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
      parser.crash(nodeIndex, "Unable to assign to non-lvalue");
    }
    auto rightType = check(right, leftType.type);
    // TODO: does this matter?
    if (
      auto assignedType = Pool().isAssignable(leftType.type, rightType.type)
    ) {
      pushType(right, assignedType);
    }
    return {Pool()._void};
  }

  ReturnType binopAssign(NodeIndex left, NodeIndex right, TokenType opType) {
    auto leftType = check(left);
    if (!leftType.lValue) {
      parser.crash(nodeIndex, "Unable to assign to non-lvalue");
    }
    expected = leftType.type;
    binopVisit(left, right, nodeIndex, opType, parser, *this);
    return {Pool()._void};
  }

  TypeIndex comparison(NodeIndex a, NodeIndex b) {
    auto aType = check(a).type;
    auto bType = check(b).type;

    if (auto coerced = Pool().coerce(aType, bType)) {
      pushType(a, coerced);
      pushType(b, coerced);
      return {Pool()._bool};
    }

    parser.crash(
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
      pushType(objectNode, aType);
      auto function = method->unboxFunction();
      auto params = Pool().tupleElements(function->type.parameters);
      if (params.size() - 1 != arguments.size()) {
        parser.crash(
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
        auto paramType = params[i + 1];
        pushType(arguments[i], paramType);
      }
      return {function->type.returnType};
    }

    vector<TypeName> argTypes;
    argTypes.push_back(TypeName(objectType.type));
    for (auto arg : arguments) {
      argTypes.push_back(TypeName(check(arg).type));
    }
    // TODO: highlight symbol
    parser.crash(
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
      pushType(a, aType);
      auto function = method->unboxFunction();
      auto params = Pool().tupleElements(function->type.parameters);
      if (params.size() != 2) {
        parser.crash(
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
      auto bType = params[1];
      pushType(b, bType);
      return {function->type.returnType};
    }

    // TODO: highlight symbol
    parser.crash(
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
    parser.crash(
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
    auto boolean = Pool()._bool;
    auto left = check(a, expected).type;
    auto right = check(b, expected).type;
    auto resultType = Pool().coerce(left, right);
    if (resultType && Pool().isInt(resultType)) {
      return {resultType};
    }
    parser.crash(
      nodeIndex,
      "Logical operations must take 2 '{}', but types were '{}' and '{}'",
      TypeName(boolean),
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

  void checkArrayIndex(NodeIndex index) {
    auto indexType = check(index).type;
    if (indexType == Pool().floatLiteral) {
      pushType(index, Pool()._usize);
    } else if (indexType == Pool().unsignedRangeLiteral) {
      // Good
    } else if (indexType == Pool().rangeLiteral) {
      parser.crash(
        index,
        "Slicing operation must use an unsigned range as index"
      );
    } else if (!Pool().isUnsignedInt(indexType)) {
      parser.crash(
        index,
        "Can't index array or slice with type '{}'; Must be an unsigned int",
        TypeName(indexType)
      );
    }
  }

  ReturnType index(NodeIndex object, NodeIndex index) {
    auto objectType = check(object);

    if (auto sizedArray = Pool().sizedArray(objectType.type)) {
      checkArrayIndex(index);
      return {sizedArray->dereferencedType, objectType.lValue};
    } else if (auto sliceElement = Pool().sliceElementType(objectType.type)) {
      checkArrayIndex(index);
      return {sliceElement, true};
    } else {
      TODO("Generic indexing");
    }
  }

  ReturnType call(NodeIndex function, Encodings::ArgumentList& args) {
    auto callerType = check(function).type;
    if (callerType == Pool().type) {
      auto constructedType = materialize(function);
      // TODO: number conversions
      return {constructedType};
    } else if (auto functionType = Pool().functionType(callerType)) {
      return {functionType->returnType};
    } else {
      if (args.requiredArgs.size() == 1 && args.optionalArgs.empty()) {
        return multiply(function, args.requiredArgs[0]);
      }
      parser.crash(
        nodeIndex,
        "Unable to call/construct/multiply type '{}' with {} positional and "
        "{} "
        "named arguments",
        TypeName(callerType),
        args.requiredArgs.size(),
        args.optionalArgs.size()
      );
    }
  }

  ReturnType exclusiveRange(NodeIndex nodeIndex) {
    // TODO: unsigned
    return {Pool().rangeLiteral};
  }

  ReturnType align(NodeIndex nodeIndex) {
    auto node = parser.getBinaryOp(nodeIndex);
    pushType(node.left, Pool().intLiteral);
    return check(node.right);
  }

  ReturnType impl(NodeIndex nodeIndex) {
    return {Pool().type};
  }

  ReturnType functionLiteral(NodeIndex nodeIndex) {
    // TODO
    auto function = parser.getFunctionLiteral(nodeIndex);
    auto params = parser.getParameterList(function.parameters);

    vector<TypeIndex> paramTypes;
    paramTypes.reserve(
      params.requiredParameters.size() + params.optionalParameters.size()
    );
    for (auto required : params.requiredParameters) {
      auto definition = parser.getDefinition(required);
      check(definition.type, Pool().type);
      paramTypes.push_back(materialize(definition.type));
    }

    for (auto _ : params.optionalParameters) {
      TODO("Default parameters in function literals");
    }
    auto [_, tupleType] = Pool().tupleOf(std::move(paramTypes));
    TypeIndex returnType = Pool()._void;
    if (function.returnType) {
      check(function.returnType, Pool().type);
      returnType = materialize(function.returnType);
    }
    return {
      Pool().addFunction({.parameters = tupleType, .returnType = returnType})
    };
  }

  ReturnType numCast(Encodings::UnaryOp& node) {
    TODO("typecheck numcast");
  }

  ReturnType bitCast(Encodings::UnaryOp& node) {
    TODO("typecheck bitcast");
  }

  ReturnType cImport(Encodings::UnaryOp& node) {
    return {Pool()._void};
  }

  ReturnType cDefine(Encodings::UnaryOp& node) {
    return {Pool()._void};
  }

  ReturnType cInclude(Encodings::UnaryOp& node) {
    return {Pool()._void};
  }

  ReturnType cIncludeDir(Encodings::UnaryOp& node) {
    return {Pool()._void};
  }

  ReturnType link(Encodings::UnaryOp& node) {
    return {Pool()._void};
  }

  ReturnType linkDir(Encodings::UnaryOp& node) {
    return {Pool()._void};
  }

  ReturnType type(Encodings::UnaryOp& node) {
    return {Pool().type};
  }

  ReturnType import(Encodings::UnaryOp& node) {
    return {Pool()._void};
  }

  ReturnType dereference(Encodings::UnaryOp& node) {
    auto value = check(node.operand);
    if (auto dereffed = Pool().dereference(value.type)) {
      return {dereffed, true};
    }
    parser.crash(
      node.operand,
      "Can't dereference non-pointer type {}",
      TypeName(value.type)
    );
  }

  ReturnType reference(Encodings::UnaryOp& node) {
    auto value = check(node.operand);
    if (value.type == Pool().type) {
      return {Pool().type};
    } else if (value.lValue) {
      return {Pool().pointerTo(value.type)};
    }
    parser.crash(node.operand, "Can't get pointer to non-lvalue");
  }

  ReturnType unaryNot(Encodings::UnaryOp& node) {
    check(node.operand, Pool()._bool);
    return {Pool()._bool};
  }

  ReturnType sliceType(Encodings::UnaryOp& node) {
    check(node.operand, Pool().type);
    return {Pool().type};
  }

  // [^]a
  ReturnType multiPointerTo(Encodings::UnaryOp& node) {
    auto value = check(node.operand);
    if (auto dereffed = Pool().dereference(value.type)) {
      return {dereffed, true};
    }
    parser.crash(
      node.operand,
      "Can't dereference non-pointer type {}",
      TypeName(value.type)
    );
  }

  // a[^]
  ReturnType multiPointerFrom(Encodings::UnaryOp& node) {
    auto value = check(node.operand);
    if (auto sliceElement = Pool().sliceElementType(value.type)) {
      return {Pool().multiPointerTo(sliceElement)};
    } else if (auto sizedArray = Pool().sizedArray(value.type)) {
      if (!value.lValue) {
        parser.crash(
          node.operand,
          "Can't take multi pointer from non-lvalue sized array"
        );
      }
      return {Pool().multiPointerTo(sizedArray->dereferencedType)};
    } else {
      parser.crash(
        node.operand,
        "Can't take multipointer from type '{}'",
        TypeName(value.type)
      );
    }
  }

  ReturnType unaryMinus(Encodings::UnaryOp& node) {
    auto value = check(node.operand, expected);
    if (Pool().isNumber(value.type)) {
      return {value.type};
    }

    return findOperator(node.operand, "negative");
  }

  ReturnType bitwiseNot(Encodings::UnaryOp& node) {
    auto value = check(node.operand, expected);
    if (Pool().isInt(value.type)) {
      return {value.type};
    }

    TODO("bitwise not for non-int types");
  }

  ReturnType makeSlice(Encodings::UnaryOp& node) {
    auto value = check(node.operand, expected);
    if (auto sizedArray = Pool().sizedArray(value.type)) {
      if (!value.lValue)
        parser.crash(node.operand, "Unable to slice non-lValue sized array");

      return {Pool().sliceOf(sizedArray->dereferencedType)};
    }

    parser.crash(
      node.operand,
      "Unable to slice type '{}' because it isn't a sized array",
      TypeName(value.type)
    );
  }

  ReturnType returnExpr(Encodings::UnaryOp& node) {
    if (auto returnType = env.returnType()) {
      bool hasValue = (bool)node.operand;
      if (hasValue != (returnType != Pool()._void)) {
        TODO("Error here");
      }
      if (node.operand) {
        check(node.operand, returnType);
      }
    }

    // TODO: does lvalue make sense here?
    return {Pool().never, true};
  }

  ReturnType usingExpr(Encodings::UnaryOp& node) {
    return {Pool()._void};
  }

  ReturnType cudaImport(Encodings::UnaryOp& node) {
    return {Pool().environment};
  }

  ReturnType ifExpr(Encodings::If& node) {
    check(node.condition, Pool()._bool);
    if (node.elseClause) {
      auto ifType = check(node.ifClause);
      auto elseType = check(node.elseClause);
      auto resultType = Pool().coerce(ifType.type, elseType.type);
      resultType = Pool().coerce(expected, resultType);
      if (resultType) {
        pushType(node.ifClause, resultType);
        pushType(node.elseClause, resultType);
        return {resultType, ifType.lValue && elseType.lValue};
      }
    }
    return {Pool()._void};
  }

  ReturnType structExpr(NodeIndex nodeIndex) {
    return {Pool().type};
  }

  ReturnType dotAccess(NodeIndex nodeIndex) {
    auto node = parser.getDotAccess(nodeIndex);
    auto targetType = expected;
    if (!node.object) {
      if (Pool().getEnum(targetType)) {
        return {targetType};
      } else {
        parser.crash(
          nodeIndex,
          "Prefix operator dot access requires an inferred enum type, but "
          "inferred type was '{}'",
          TypeName(targetType)
        );
      }
    }
    targetType = check(node.object).type;

    auto fieldName = node.fieldName->lexeme;
    if (targetType == Pool().type) {
      if (
        fieldName == "size" || fieldName == "alignment" ||
        fieldName == "bitSize"
      ) {
        return {Pool().intLiteral};
      }

      auto type = materialize(node.object);
      if (auto member = env.getStatic(type, fieldName)) {
        return {member->getType()};
      } else {
        parser.crash(
          node.fieldName,
          "Unable to find type member '{}.{}'",
          TypeName(type),
          fieldName
        );
      }
    }

    auto field = Pool().getFieldIndex(targetType, fieldName);
    if (field) {
      return {field->second};
    }
    parser.crash(
      node.fieldName,
      "Unable to find field '{}' for type '{}'",
      fieldName,
      TypeName(targetType)
    );
  }

  ReturnType argList(NodeIndex nodeIndex) {
    if (expected == Pool().infer) {
      parser.crash(
        nodeIndex,
        "Can't evaluate value tuple without expected type"
      );
    }

    auto structDefinition = Pool().getStruct(expected);
    if (!structDefinition.has_value()) {
      parser.crash(
        nodeIndex,
        "Can't construct non-struct type {}",
        TypeName(expected)
      );
    }
    return {expected};
  }

  ReturnType enumExpr(Encodings::Enum& node) {
    return {Pool().type};
  }

  ReturnType multiLineString(NodeIndex nodeIndex) {
    return {Pool().sliceOf(Pool()._u8)};
  }

  ReturnType forLoop(NodeIndex nodeIndex) {
    return {Pool()._void};
  }

  ReturnType apply(NodeIndex functionNode, NodeIndex argNode) {
    auto callerType = check(functionNode).type;

    // TODO: bound function
    if (auto function = Pool().unbox<FunctionType>(callerType)) {
      auto params = Pool().tupleElements(function->parameters);
      // TODO: default args
      if (params.size() != 1) {
        parser.crash(
          functionNode,
          "Can only apply functions that take 1 parameter, but provided "
          "function takes {}",
          params.size()
        );
      }
      check(argNode, params[0]);
      return {function->returnType};
    } else {
      return multiply(functionNode, argNode);
    }
  }
};

static_assert(AstVisitor<TypeChecker>, "TypeChecker must implement AstVisitor");
