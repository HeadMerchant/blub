#pragma once
#include "parser.h"
#include "tokenizer.h"
template <typename T>

concept AstVisitor = requires(
  T t,
  NodeIndex nodeIndex,
  TokenPointer token,
  Encodings::UnaryOp& unaryOp,
  Encodings::Block& block,
  Encodings::If& ifExpr,
  Encodings::Enum& enumExpr,
  Encodings::Declaration& declaration,
  Encodings::Definition& definition,
  Encodings::ArgumentList& argList
) {
  typename T::ReturnType;
  { t.setVisitedNode(nodeIndex) };
  { t.block(block) } -> std::same_as<typename T::ReturnType>;
  { t.arrayLiteral(block) } -> std::same_as<typename T::ReturnType>;
  {
    t.when(nodeIndex, span<pair<NodeIndex, NodeIndex>>{})
  } -> std::same_as<typename T::ReturnType>;
  { t.declaration(declaration) } -> std::same_as<typename T::ReturnType>;
  { t.definition(definition) } -> std::same_as<typename T::ReturnType>;
  { t.character(token) } -> std::same_as<typename T::ReturnType>;
  { t.string(token) } -> std::same_as<typename T::ReturnType>;
  { t.nullString(token) } -> std::same_as<typename T::ReturnType>;
  { t.decimal(token) } -> std::same_as<typename T::ReturnType>;
  { t.integer(token) } -> std::same_as<typename T::ReturnType>;
  { t.hexInt(token) } -> std::same_as<typename T::ReturnType>;
  { t.boolean(bool{}) } -> std::same_as<typename T::ReturnType>;
  { t.identifier(token) } -> std::same_as<typename T::ReturnType>;
  { t.opaque(token) } -> std::same_as<typename T::ReturnType>;
  { t.self(token) } -> std::same_as<typename T::ReturnType>;
  { t.undefined(token) } -> std::same_as<typename T::ReturnType>;
  {
    t.cudaBuiltin(token, string_view{})
  } -> std::same_as<typename T::ReturnType>;
  { t.assign(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  {
    t.binopAssign(nodeIndex, nodeIndex, TokenType{})
  } -> std::same_as<typename T::ReturnType>;
  { t.whileLoop(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  {
    t.sizedArray(nodeIndex, nodeIndex)
  } -> std::same_as<typename T::ReturnType>;
  { t.index(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.call(nodeIndex, argList) } -> std::same_as<typename T::ReturnType>;
  { t.exclusiveRange(nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.align(nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.impl(nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.functionLiteral(nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.numCast(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.bitCast(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.cImport(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.cDefine(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.cInclude(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.cIncludeDir(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.link(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.linkDir(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.type(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.import(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.dereference(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.reference(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.unaryNot(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.sliceType(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.multiPointerTo(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.multiPointerFrom(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.unaryMinus(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.bitwiseNot(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.makeSlice(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.returnExpr(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.usingExpr(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.cudaImport(unaryOp) } -> std::same_as<typename T::ReturnType>;
  { t.ifExpr(ifExpr) } -> std::same_as<typename T::ReturnType>;
  { t.structExpr(nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.dotAccess(nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.argList(nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.enumExpr(enumExpr) } -> std::same_as<typename T::ReturnType>;
  { t.multiLineString(nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.forLoop(nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.apply(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.equal(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.notEqual(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.lt(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.gt(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.leq(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.geq(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.add(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.subtract(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.multiply(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.divide(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  {
    t.leftDivide(nodeIndex, nodeIndex)
  } -> std::same_as<typename T::ReturnType>;
  { t.logicAnd(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.logicOr(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  {
    t.bitwiseAnd(nodeIndex, nodeIndex)
  } -> std::same_as<typename T::ReturnType>;
  { t.bitwiseOr(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.xorOp(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.remainder(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  { t.shiftLeft(nodeIndex, nodeIndex) } -> std::same_as<typename T::ReturnType>;
  {
    t.shiftRight(nodeIndex, nodeIndex)
  } -> std::same_as<typename T::ReturnType>;
};

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

template <AstVisitor T>
T::ReturnType binopVisit(
  NodeIndex a,
  NodeIndex b,
  NodeIndex binopIndex,
  TokenType op,
  Parser& parser,
  T& t
) {
  switch (op) {
  case TokenType::DoubleEqual: {
    return t.equal(a, b);
  }
  case TokenType::NotEqual: {
    return t.notEqual(a, b);
  }
  case TokenType::Lt: {
    return t.lt(a, b);
  }
  case TokenType::Gt: {
    return t.gt(a, b);
  }
  case TokenType::Leq: {
    return t.leq(a, b);
  }
  case TokenType::Geq: {
    return t.geq(a, b);
  }
  case TokenType::Plus: {
    return t.add(a, b);
  }
  case TokenType::Minus: {
    return t.subtract(a, b);
  }
  case TokenType::Mult: {
    return t.multiply(a, b);
  }
  case TokenType::Div: {
    return t.divide(a, b);
  }
  case TokenType::LeftDiv: {
    return t.leftDivide(a, b);
  }
  case TokenType::Remainder: {
    return t.remainder(a, b);
  }
  case TokenType::LogicAnd: {
    return t.logicAnd(a, b);
  }
  case TokenType::LogicOr: {
    return t.logicOr(a, b);
  }
  case TokenType::BitAnd: {
    return t.bitwiseAnd(a, b);
  }
  case TokenType::BitOr: {
    return t.bitwiseOr(a, b);
  }
  case TokenType::Xor: {
    return t.xorOp(a, b);
  }
  case TokenType::ShiftLeft: {
    return t.shiftLeft(a, b);
  }
  case TokenType::ShiftRight: {
    return t.shiftRight(a, b);
  }
  case TokenType::While: {
    return t.whileLoop(a, b);
  }
  case TokenType::LeftSquareBracket: {
    // [left]right
    // ^
    if (parser.nodeTokenPrecedes(binopIndex, a)) {
      return t.sizedArray(a, b);
    }
    // left[right]
    //     ^
    return t.index(a, b);
  }
  case TokenType::LeftParen: {
    NodeIndex function = a;
    auto argList = parser.getArgumentList(b);
    return t.call(function, argList);
  }
  case TokenType::ExclusiveRange: {
    return t.exclusiveRange(a, b);
  }
  case TokenType::BUILTIN_Align: {
    return t.align(a, b);
  }
  case TokenType::Impl: {
    return t.impl(a, b);
  }
  default:
    parser.crash(a, "Unknown binary operation");
  }
}

template <AstVisitor T>
T::ReturnType astVisit(NodeIndex nodeIndex, Parser& parser, T& t) {
  auto encoded = parser.getNode(nodeIndex);
  t.setVisitedNode(nodeIndex);
  switch (encoded.nodeType) {
  case NodeType::Block: {
    auto token = parser.getToken(nodeIndex);
    switch (token->type) {
    case TokenType::LeftCurlyBrace: {
      return t.block(nodeIndex);
    }
    case TokenType::LeftSquareBracket: {
      return t.arrayLiteral(nodeIndex);
    }
    case TokenType::LeftParen: {
      return TODO("remove");
    }
    case TokenType::When: {
      auto node = parser.getBlock(nodeIndex);
      auto condition = node.elements[0];
      // Always odd number in encoded children, so floor division is fine
      auto caseNodes = std::bit_cast<span<pair<NodeIndex, NodeIndex>>>(
        node.elements.subspan(1, node.elements.size() / 2)
      );
      return t.when(condition, caseNodes);
    }
    default:
      TODO("Default for block nodes");
      break;
    }
  }
  case NodeType::Declaration: {
    auto node = parser.getDeclaration(nodeIndex);
    return t.declaration(node);
  }
  case NodeType::Definition: {
    auto node = parser.getDefinition(nodeIndex);
    return t.definition(node);
  }
  case NodeType::Literal: {
    auto node = parser.getLiteral(nodeIndex);
    auto token = node.token;
    if (token->type == TokenType::Char) {
      return t.character(token);
    }
    if (token->type == TokenType::String) {
      return t.string(token);
    }
    if (token->type == TokenType::NullTerminatedString) {
      return t.nullString(token);
    }
    if (token->type == TokenType::Decimal) {
      return t.decimal(token);
    }
    if (token->type == TokenType::Integer) {
      return t.integer(token);
    }
    if (token->type == TokenType::HexInt) {
      return t.hexInt(token);
    }
    if (token->type == TokenType::True) {
      return t.boolean(true);
    }
    if (token->type == TokenType::False) {
      return t.boolean(false);
    }
    if (token->type == TokenType::Identifier) {
      return t.identifier(token);
    }
    if (token->type == TokenType::Opaque) {
      return t.opaque(token);
    }
    if (token->type == TokenType::Self) {
      return t.self(token);
    }
    if (token->type == TokenType::Undef) {
      return t.undefined(token);
    }

    auto cudaFunction = cudaBuiltins.find(token->type);
    if (cudaFunction != cudaBuiltins.end()) {
      return t.cudaBuiltin(token, cudaFunction->second);
    }

    parser.crash(nodeIndex, "Unknown literal {}", token->lexeme);
  }

  case NodeType::Assignment: {
    auto node = parser.getNode(nodeIndex);
    TokenPointer token = parser.getToken(node.token);
    if (auto opType = Token::binopFromCompoundAssignment(token->type)) {
      t.binopAssign(node.left, node.right, opType);
    } else if (token->type != TokenType::Assign) {
      parser.crash(
        token,
        "Unknown compound assignment operator '{}'",
        token->lexeme
      );
    } else {
      return t.assign(node.left, node.right);
    }
  }

  case NodeType::BinaryOp: {
    auto node = parser.getBinaryOp(nodeIndex);

    auto opType = node.operation->type;

    auto a = node.left;
    auto b = node.right;
    return binopVisit(a, b, opType, parser, t);
  }
  case NodeType::FunctionLiteral: {
    t.functionLiteral(nodeIndex);
  }
  case NodeType::Unary: {
    auto node = parser.getUnary(nodeIndex);

    switch (node.operation) {
    case UnaryOps::CompilerBuiltin:
      if (node.operation == UnaryOps::CompilerBuiltin) {
        auto builtinToken = parser.getToken(parser.getNode(nodeIndex).token);
        switch (builtinToken->type) {
        case TokenType::BUILTIN_NumCast: {
          return t.numCast(node);
        }
        case TokenType::BUILITN_BitCast: {
          return t.bitCast(node);
        }
        case TokenType::BUILTIN_CImport: {
          return t.cImport(node);
        }
        case TokenType::BUILTIN_CDefine: {
          return t.cDefine(node);
        }
        case TokenType::BUILTIN_CInclude: {
          return t.cInclude(node);
        }
        case TokenType::BUILTIN_CIncludeDir: {
          return t.cIncludeDir(node);
        }
        case TokenType::BUILTIN_Link: {
          return t.link(node);
        }
        case TokenType::BUILTIN_LinkDir: {
          return t.linkDir(node);
        }
        case TokenType::BUILTIN_Type: {
          return t.type(node);
        }
        default: {
          parser
            .crash(nodeIndex, "Malformed builtin '@{}'", builtinToken->lexeme);
        }
        }
      }
    case UnaryOps::Import: {
      return t.import(node);
    }
    case UnaryOps::Dereference: {
      return t.dereference(node);
    }
    case UnaryOps::Reference: {
      return t.reference(node);
    }
    case UnaryOps::Not: {
      return t.unaryNot(node);
    }
    case UnaryOps::SliceType: {
      return t.sliceType(node);
    }
    case UnaryOps::MultiPointerTo: {
      return t.multiPointerTo(node);
    }
    case UnaryOps::MultiPointerFrom: {
      return t.multiPointerFrom(node);
    }
    case UnaryOps::Minus: {
      return t.unaryMinus(node);
    }
    case UnaryOps::BitNot: {
      return t.bitwiseNot(node);
    }
    case UnaryOps::MakeSlice: {
      return t.makeSlice(node);
    }
    case UnaryOps::Return: {
      return t.returnExpr(node);
    }
    case UnaryOps::Using: {
      return t.usingExpr(node);
    }
    case UnaryOps::CudaImport: {
      return t.cudaImport(node);
    }
    }
  }
  case NodeType::If: {
    auto node = parser.getIf(nodeIndex);
    return t.ifExpr(node);
  }
  case NodeType::Struct: {
    return t.structExpr(nodeIndex);
  }
  case NodeType::DotAccess: {
    return t.dotAccess(nodeIndex);
  }
  case NodeType::ArgumentList: {
    return t.argList(nodeIndex);
  }
  case NodeType::ParameterList: {
    parser.crash(
      nodeIndex,
      "Input list nodes shouldn't be directly interpreted"
    );
  }
  case NodeType::Enum: {
    return t.enumExpr(nodeIndex);
  }
  case NodeType::MultiLineString: {
    return t.multiLineString(nodeIndex);
  }
  case NodeType::ForLoop: {
    return t.forLoop(nodeIndex);
  }
  case NodeType::Apply: {
    auto node = parser.getNode(nodeIndex);
    return t.apply(node.left, node.right);
  }
  }
  parser.crash(nodeIndex, "Unknown node type");
}
