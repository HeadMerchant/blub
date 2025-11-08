#pragma once
#include "common.h"
#include "logging.h"
#include "tokenizer.h"
#include <bit>
#include <cassert>
#include <fmt/core.h>
#include <iostream>
#include <limits>
#include <optional>
#include <span>
#include <stdexcept>
#include <vector>

enum class NodeType {
  Declaration,
  Unary,
  Literal,
  FunctionLiteral,
  Block,
  InputList,
  BinaryOp,
  Definition,
  If,
  Struct,
  DotAcces,
  Enum,
  MultiLineString,
};

enum class UnaryOps {
  Not,
  Reference,
  Dereference,
  Slice,
  MultiPointer,
  CompilerBuiltin,
  Import,
};

struct NodeIndex {
  i32 value;
};

struct DataIndex {
  i32 value;
};

struct TokenIndex {
  i32 value;
};

struct ASTNode {
  i32 left;
  i32 right;
  TokenIndex token;
  NodeType nodeType;
};

using TokenPointer = const Tokenization::Token*;
using NodeList = std::vector<TokenIndex>;
using DataSpan = std::span<i32>;
using ChildSpan = std::span<NodeIndex>;
using TokenSpan = std::span<TokenIndex>;
using OptionalNode = std::optional<NodeIndex>;
static i32 MAX_NODE = std::numeric_limits<i32>::max();

namespace Encodings {
struct Declaration {
  NodeIndex definition;
  NodeIndex value;
};

struct UnaryOp {
  NodeIndex operand;
  UnaryOps operation;
};

struct DotAccessor {
  NodeIndex object;
  TokenPointer fieldName;
};

struct BinaryOp {
  NodeIndex left;
  NodeIndex right;
  TokenPointer operation;
};

struct Literal {
  TokenPointer token;
};

struct Identifier {
  TokenPointer token;
};

struct FunctionLiteral {
  NodeIndex parameters;
  OptionalNode returnType;
  OptionalNode body;
};

struct Block {
  ChildSpan elements;
};

struct Definition {
  TokenPointer name;
  OptionalNode type;
};

struct If {
  NodeIndex condition;
  NodeIndex value;
  // Don't need a separate node type for else because else only stores a single
  // expression
  // TODO: consider adding back else node for clearer messages?
  OptionalNode elseValue;
};

struct Struct {
  ChildSpan children;
};

struct InputList {
  ChildSpan requiredInputs;
  ChildSpan optionalInputs;
  enum class InputType {
    Parameter,
    Argument,
  };
  InputType inputType;
};

struct Enum {
  OptionalNode rawType;
  TokenSpan values;
};

}; // namespace Encodings

using namespace Tokenization;
class Parser {
public:
  Parser(Tokenizer& tokenizer) : tokenizer(tokenizer), tokens(tokenizer.tokens), log(logger(LogLevel::DEBUG)) {}
  const Tokenizer& tokenizer;
  const std::vector<Token>& tokens;
  std::vector<i32> extraData;
  std::vector<ASTNode> nodes;
  TokenIndex current = {0};
  std::ostream& log;

  const Token& peek(TokenIndex ahead = {0}) {
    return tokens[current.value + ahead.value];
  }

  const TokenPointer advance() {
    if (!isAtEnd()) current.value++;
    TokenPointer token = previous();
    // log << "Advancing: " << token->lexeme << "\n";
    return token;
  }

  void accept(TokenType type) {
    if (isAtEnd()) return;
    if (peek().type == type) {
      advance();
    }
  }

  void acceptN(TokenType type) {
    while (!isAtEnd() && peek().type == type)
      advance();
  }

  bool check(TokenType type, TokenIndex ahead = {0}) {
    if (isAtEnd(ahead)) {
      return false;
    }
    return peek(ahead).type == type;
  }

  bool check(std::span<TokenType> types, TokenIndex ahead = {0}) {
    if (isAtEnd(ahead)) {
      return false;
    }
    for (TokenType type : types) {
      if (peek(ahead).type == type) return true;
    }
    return false;
  }

  bool isAtEnd(TokenIndex ahead = {0}) {
    return current.value + ahead.value >= tokens.size() || peek(ahead).type == TokenType::EndOfFile;
  }

  const TokenPointer previous() {
    return tokens.data() + current.value - 1;
  }
  TokenPointer getToken(TokenIndex token) const {
    return &tokens[token.value];
  }
  TokenPointer getToken(NodeIndex node) const {
    return toPointer(nodes[node.value].token);
  }

  TokenPointer match(TokenType type) {
    if (check(type)) {
      return advance();
    }
    return nullptr;
  }

  TokenPointer match(std::vector<TokenType>& types) {
    for (auto type : types) {
      if (check(type)) {
        return advance();
      }
    }
    return nullptr;
  }

  TokenPointer consume(TokenType type, std::string message) {
    if (check(type)) return advance();
    log << advance();
    throw std::invalid_argument(message);
  }

  void consumeAtLeast1(TokenType needed, std::string message) {
    consume(needed, message);
    while (check(needed))
      advance();
  }

  const Token& latestToken() {
    auto last = tokens.size() - 1;
    return tokens[last > current.value ? current.value : last];
  }

  const NodeIndex nodeIndex() {
    return NodeIndex{.value = (i32)nodes.size() - 1};
  }

  const DataIndex dataIndex() {
    return {(i32)extraData.size() - 1};
  }

  NodeIndex addNode(ASTNode node) {
    nodes.push_back(node);
    return nodeIndex();
  }

  ASTNode getNode(NodeIndex index) {
    return nodes[index.value];
  }

  NodeType nodeType(NodeIndex index) {
    return nodes[index.value].nodeType;
  }

  ASTNode getNode(NodeIndex index, NodeType type) {
    auto encoded = nodes[index.value];
    assert(encoded.nodeType == type);
    return encoded;
  }

  ChildSpan getChildren() {
    return std::bit_cast<ChildSpan>(std::span<i32>(extraData));
  }

  DataIndex addData(NodeIndex node) {
    return addData(node.value);
  }

  DataIndex addData(i32 data) {
    DataIndex start = {(i32)extraData.size()};
    extraData.push_back(data);
    return start;
  }

  DataIndex addData(DataSpan newData) {
    DataIndex start = {(i32)extraData.size()};
    extraData.insert(extraData.end(), newData.begin(), newData.end());
    return start;
  }

  DataIndex addData(ChildSpan newData) {
    return addData(std::bit_cast<DataSpan>(newData));
  }

  TokenIndex toIndex(TokenPointer token) {
    return {(i32)(token - tokens.data())};
  }

  TokenPointer toPointer(TokenIndex index) const {
    return tokens.data() + index.value;
  }

  NodeIndex addNode(Encodings::Declaration node, TokenIndex token) {
    return addNode(
      ASTNode{
        .left = node.definition.value,
        .right = node.value.value,
        .token = token,
        .nodeType = NodeType::Declaration,
      });
  }

  Encodings::Declaration getDeclaration(NodeIndex index) {
    auto encoded = getNode(index, NodeType::Declaration);
    return {.definition = {encoded.left}, .value = {encoded.right}};
  }

  NodeIndex addNode(Encodings::UnaryOp node, TokenPointer token) {
    return addNode(ASTNode{.left = static_cast<i32>(node.operation), .right = node.operand.value, .token = toIndex(token), .nodeType = NodeType::Unary});
  }

  Encodings::UnaryOp getUnary(NodeIndex index) {
    auto encoded = getNode(index, NodeType::Unary);
    return {.operand = {encoded.right}, .operation = static_cast<UnaryOps>(encoded.left)};
  }

  NodeIndex addNode(Encodings::Literal node) {
    auto token = toIndex(node.token);
    return addNode(ASTNode{.left = token.value, .token = token, .nodeType = NodeType::Literal});
  }

  Encodings::Literal getLiteral(NodeIndex node) {
    auto encoded = getNode(node, NodeType::Literal);
    return {.token = toPointer(encoded.token)};
  }

  OptionalNode readOptional(NodeIndex node, i32 expectedIndex) {
    if (expectedIndex >= node.value) return std::nullopt;
    return NodeIndex{expectedIndex};
  }

  i32 encodeOptional(OptionalNode child) {
    if (child.has_value()) return child->value;
    return MAX_NODE;
  }

  NodeIndex addNode(Encodings::FunctionLiteral node, TokenIndex token) {
    // Block stored directed after args
    i32 nextIndex = nodes.size();
    auto dataIndex = addData(encodeOptional(node.returnType));
    addData(encodeOptional(node.body));
    return addNode(ASTNode{.left = dataIndex.value, .right = node.parameters.value, .token = token, .nodeType = NodeType::FunctionLiteral});
  }

  Encodings::FunctionLiteral getFunctionLiteral(NodeIndex node) {
    auto encoded = getNode(node, NodeType::FunctionLiteral);
    auto& parameters = extraData;

    auto startIndex = encoded.left;
    return {.parameters = {encoded.right}, .returnType = readOptional(node, parameters[startIndex]), .body = readOptional(node, parameters[startIndex + 1])};
  }

  NodeIndex addNode(Encodings::Block node, TokenIndex token) {
    auto index = addData(node.elements);

    return addNode({.left = index.value, .right = (i32)node.elements.size(), .token = token, .nodeType = NodeType::Block});
  }

  Encodings::Block getBlock(NodeIndex node) {
    auto encoded = getNode(node, NodeType::Block);

    auto statements = getChildren();
    return {.elements = getChildren().subspan(encoded.left, encoded.right)};
  }

  NodeIndex addNode(Encodings::BinaryOp node) {
    return addNode(ASTNode{.left = node.left.value, .right = node.right.value, .token = toIndex(node.operation), .nodeType = NodeType::BinaryOp});
  }

  Encodings::BinaryOp getBinaryOp(NodeIndex node) {
    auto encoded = getNode(node, NodeType::BinaryOp);
    return {.left = {encoded.left}, .right = {encoded.right}, .operation = toPointer(encoded.token)};
  }

  NodeIndex addNode(Encodings::Definition node, TokenPointer token) {
    // Type is guaranteed to not have the same index as the definition
    return addNode(ASTNode{.left = toIndex(node.name).value, .right = encodeOptional(node.type), .token = toIndex(token), .nodeType = NodeType::Definition});
  }

  Encodings::Definition getDefinition(NodeIndex node) {
    auto encoded = getNode(node, NodeType::Definition);
    return {.name = toPointer({encoded.left}), .type = readOptional(node, encoded.right)};
  }

  NodeIndex addNode(Encodings::If node, TokenPointer token) {
    std::vector<NodeIndex> children = {node.condition, node.value};
    auto dataIndex = addData(ChildSpan(children));

    return addNode(ASTNode{.left = dataIndex.value, .right = encodeOptional(node.elseValue), .token = toIndex(token), .nodeType = NodeType::If});
  }

  Encodings::If getIf(NodeIndex node) {
    auto encoded = getNode(node, NodeType::If);
    return {.condition = {extraData[encoded.left]}, .value = {extraData[encoded.left + 1]}, .elseValue = readOptional(node, encoded.right)};
  }

  NodeIndex addNode(Encodings::Struct node, TokenPointer token) {
    auto dataIndex = addData(node.children);
    return addNode(ASTNode{.left = dataIndex.value, .right = (i32)node.children.size(), .token = toIndex(token), .nodeType = NodeType::Struct});
  }

  Encodings::Struct getStruct(NodeIndex node) {
    auto encoded = getNode(node, NodeType::Struct);

    return {.children = getChildren().subspan(encoded.left, encoded.right)};
  }

  NodeIndex addNode(Encodings::DotAccessor node, TokenPointer token) {
    return addNode(ASTNode{.left = node.object.value, .right = toIndex(node.fieldName).value, .token = toIndex(token), .nodeType = NodeType::DotAcces});
  }

  Encodings::DotAccessor getDotAccess(NodeIndex node) {
    auto encoded = getNode(node, NodeType::DotAcces);
    return {.object = {encoded.left}, .fieldName = toPointer({encoded.right})};
  }

  NodeIndex addNode(Encodings::Enum node, TokenPointer token) {
    auto dataIndex = addData(encodeOptional(node.rawType));
    addData(std::bit_cast<DataSpan>(node.values));
    return addNode(ASTNode{.left = dataIndex.value, .right = (i32)node.values.size(), .token = toIndex(token), .nodeType = NodeType::Enum});
  }

  Encodings::Enum getEnumDefinition(NodeIndex node) {
    auto encoded = getNode(node, NodeType::Enum);
    auto data = std::span<i32>(extraData).subspan(encoded.left, encoded.right);
    return {.rawType = readOptional(node, data[0]), .values = std::bit_cast<TokenSpan>(data.subspan(1))};
  }

public:
  bool nodeTokenPrecedes(NodeIndex a, NodeIndex b) {
    return getNode(a).token.value < getNode(b).token.value;
  }

  std::vector<NodeIndex> parse() {
    std::vector<NodeIndex> statements;
    while (!isAtEnd()) {
      statements.push_back(statement());
    }
    return statements;
  }

  NodeIndex statement() {
    if (check(TokenType::StatementBreak)) {
      advance();
    }

    NodeIndex node = assignment();

    if (!isAtEnd()) {
      consumeAtLeast1(TokenType::StatementBreak, "Expected a breaking statement");
    }
    return node;
  }

  NodeIndex assignment() {
    bool isDefinition = check(TokenType::Identifier) && check(TokenType::Colon, {1});
    if (isDefinition) {
      return declaration();
    }

    NodeIndex expr = expression();

    if (check(TokenType::Assign)) {
      TokenPointer token = advance();
      auto value = expression();
      return addNode(Encodings::BinaryOp{.left = expr, .right = value, .operation = token});
    }

    return expr;
  }

  NodeIndex declaration() {
    NodeIndex name = definition();
    if (check(TokenType::Assign) || check(TokenType::Colon)) {
      TokenPointer token = advance();
      NodeIndex value = expression();
      log << "Assigning " << getDefinition(name).name->lexeme << "\n";
      return addNode(Encodings::Declaration{.definition = name, .value = value}, toIndex(token));
    }

    log << "Defining " << getDefinition(name).name->lexeme << "\n";
    return name;
  }

  NodeIndex definition() {
    TokenPointer name = consume(TokenType::Identifier, "Expected an identifier for a definition");
    consume(TokenType::Colon, "Expected a ':' for type declaration");
    TokenPointer token = previous();
    bool infer = check(TokenType::Colon) || check(TokenType::Assign);
    if (infer) {
      return addNode(Encodings::Definition{.name = name, .type = std::nullopt}, token);
    }

    NodeIndex type = expression();
    return addNode(Encodings::Definition{.name = name, .type = type}, token);
  }

  NodeIndex expression() {
    return equality();
  }

  NodeIndex equality() {
    static std::vector<TokenType> types = {TokenType::DoubleEqual, TokenType::NotEqual};

    NodeIndex expr = comparison();
    while (match(types)) {
      TokenPointer op = previous();
      auto node = Encodings::BinaryOp{.left = expr, .right = comparison(), .operation = op};
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex comparison() {
    static std::vector<TokenType> types = {};
    return term();
  }

  NodeIndex term() {
    static std::vector<TokenType> types = {TokenType::Plus, TokenType::Minus};
    auto expr = factor();
    while (match(types)) {
      TokenPointer op = previous();
      auto node = Encodings::BinaryOp{.left = expr, .right = factor(), .operation = op};
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex factor() {
    static std::vector<TokenType> types = {TokenType::Mult, TokenType::Div, TokenType::ShiftRight};
    auto expr = unary();
    while (match(types)) {
      TokenPointer op = previous();
      auto node = Encodings::BinaryOp{.left = expr, .right = unary(), .operation = op};
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex unary() {
    NodeIndex expr;
    if (check(TokenType::Not)) {
      TokenPointer op = previous();
      auto node = Encodings::UnaryOp{.operand = call(), .operation = UnaryOps::Not};
      expr = addNode(node, op);
    } else if (check(TokenType::Pointer)) {
      fmt::println("!!Reference operation!!");
      TokenPointer op = advance();
      auto node = Encodings::UnaryOp{.operand = call(), .operation = UnaryOps::Reference};
      expr = addNode(node, op);
    } else {
      expr = call();
    }

    if (check(TokenType::Pointer)) {
      fmt::println("!!Dereference operation!!");
      auto node = Encodings::UnaryOp{.operand = expr, .operation = UnaryOps::Dereference};
      expr = addNode(node, advance());
    }

    if (check(TokenType::Dot)) {
      auto token = advance();
      auto node = Encodings::DotAccessor({.object = expr, .fieldName = consume(TokenType::Identifier, "Expected identifier after '.' accessor")});
      expr = addNode(node, token);
    }
    return expr;
  }

  NodeIndex call() {
    static std::vector<TokenType> builtinTokens = {
      TokenType::BUILTIN_RegisterType,
      TokenType::BUILTIN_NumCast,
      TokenType::BUILTIN_CDefine,
      TokenType::BUILTIN_CInclude,
      TokenType::BUILTIN_CIncludeDir,
      TokenType::BUILTIN_CLink};
    if (auto builtin = match(builtinTokens)) {
      consume(TokenType::LeftParen, "Compiler builtins must be called like functions");
      auto args = inputList(Encodings::InputList::InputType::Argument);
      return addNode(Encodings::UnaryOp{.operand = args, .operation = UnaryOps::CompilerBuiltin}, builtin);
    }

    NodeIndex expr = access();
    if (check(TokenType::LeftParen)) {
      auto token = advance();
      auto node = Encodings::BinaryOp{.left = expr, .right = inputList(Encodings::InputList::InputType::Argument), .operation = token};
      return addNode(node);
    }

    return expr;
  }

  NodeIndex inputList(Encodings::InputList::InputType definitionOrUsage, TokenType closingBracket = TokenType::RightParen) {
    // std::vector<NodeIndex> args;
    // while (!check(TokenType::RIGHT_PAREN)) {
    //   log << "finding arg\n";
    //   args.push_back(expression());
    // }
    bool hasMultiple = false;
    std::vector<NodeIndex> requiredInputs;
    std::vector<NodeIndex> optionalInputs;
    i32 listLength = 0;

    // allow combined length to fit in signed 8
    static const i32 MAX_ARGUMENTS = 127;
    while (!check(closingBracket)) {
      if (hasMultiple) {
        consume(TokenType::Comma, "Expected separating comma between parameters");
      }

      // Allow trailing comma
      if (check(closingBracket)) {
        break;
      }

      // TODO: consider how to clean up this code
      if (definitionOrUsage == Encodings::InputList::InputType::Argument) {
        if ((requiredInputs.size() + optionalInputs.size()) > MAX_ARGUMENTS) {
          throw std::invalid_argument("Maximum arguments exceeded");
        }
        if (check(TokenType::Identifier) && check(TokenType::Assign, {1})) {
          optionalInputs.push_back(assignment());
        } else if (!optionalInputs.empty()) {
          throw std::invalid_argument("All arguments following a named argument must be named");
        } else {
          requiredInputs.push_back(expression());
        }
      } else if (definitionOrUsage == Encodings::InputList::InputType::Parameter) {
        if ((requiredInputs.size() + optionalInputs.size()) > MAX_ARGUMENTS) {
          throw std::invalid_argument("Maximum arguments exceeded");
        }
        NodeIndex parameter = declaration();
        NodeType parameterType = nodeType(parameter);

        bool isOptional = parameterType == NodeType::Declaration;
        if (isOptional) {
          optionalInputs.push_back(parameter);
        } else if (!optionalInputs.empty()) {
          throw std::invalid_argument("All arguments following a named argument must be named");
        } else {
          requiredInputs.push_back(parameter);
        }
      } else {
        assert(false);
      }
      hasMultiple = true;
    }

    consume(closingBracket, "Expected ')' for declaration");

    auto dataIndex = addData(requiredInputs);
    addData(optionalInputs);
    i32 packedInputLength = packInt(requiredInputs.size(), optionalInputs.size(), static_cast<i8>(definitionOrUsage), 0);
    return addNode(ASTNode{.left = dataIndex.value, .right = packedInputLength, .nodeType = NodeType::InputList});
  }

  NodeIndex addNode(Encodings::InputList node) {
    auto dataIndex = addData(node.requiredInputs);
    addData(node.optionalInputs);
    i32 packedInputLength = packInt(node.requiredInputs.size(), node.optionalInputs.size(), static_cast<i8>(node.inputType), 0);
    return addNode(ASTNode{.left = dataIndex.value, .right = packedInputLength, .nodeType = NodeType::InputList});
  }

  Encodings::InputList getInputList(NodeIndex node) {
    auto encoded = getNode(node, NodeType::InputList);
    auto [requiredLength, optionalLength, inputType, _] = unpackInt(encoded.right);
    auto children = getChildren();
    return {
      .requiredInputs = children.subspan(encoded.left, requiredLength),
      .optionalInputs = children.subspan(encoded.left + requiredLength, optionalLength),
      .inputType = static_cast<Encodings::InputList::InputType>(inputType)};
  }

  NodeIndex access() {
    NodeIndex expr = primary();
    if (check(TokenType::LeftSquareBracket)) {
      auto token = advance();
      if (check(TokenType::RightSquareBracket)) {
        return addNode(Encodings::UnaryOp{.operand = expr, .operation = UnaryOps::Slice}, token);
      }
      auto index = expression();
      consume(TokenType::RightSquareBracket, "Expected a closing ']' after indexing or slicing operation");
      expr = addNode(Encodings::BinaryOp{.left = expr, .right = index, .operation = token});
    }

    return expr;
  }

  NodeIndex primary() {
    static std::vector<TokenType> types = {
      TokenType::String,         TokenType::Decimal,        TokenType::Integer,       TokenType::NullTerminatedString, TokenType::True,
      TokenType::False,          TokenType::Identifier,     TokenType::Opaque,        TokenType::CudaBlockIdxX,        TokenType::CudaBlockIdxY,
      TokenType::CudaBlockIdxZ,  TokenType::CudaBlockDimX,  TokenType::CudaBlockDimY, TokenType::CudaBlockDimZ,        TokenType::CudaThreadIdxX,
      TokenType::CudaThreadIdxY, TokenType::CudaThreadIdxZ, TokenType::CudaGridDimX,  TokenType::CudaGridDimY,         TokenType::CudaGridDimZ,
    };

    if (auto token = match(types)) {
      auto node = Encodings::Literal{.token = token};
      return addNode(node);
    }

    if (check(TokenType::Function)) {
      return function();
    }

    if (check(TokenType::Struct)) {
      return structDefinition();
    }

    if (check(TokenType::Enum)) {
      return enumLiteral();
    }

    if (check(TokenType::If)) {
      auto ifToken = advance();
      consume(
        TokenType::LeftParen,
        "Condition for 'if' statement needs to be "
        "surrounded by parentheses '('");
      auto condition = expression();
      consume(
        TokenType::RightParen,
        "Condition for 'if' statement needs to be "
        "surrounded by parentheses ')'");
      auto value = expression();

      auto ifNode = Encodings::If{.condition = condition, .value = value};
      OptionalNode elseNode = std::nullopt;
      if (check(TokenType::Else)) {
        advance();
        elseNode = expression();
      }
      ifNode.elseValue = elseNode;
      return addNode(ifNode, ifToken);
    }

    if (match(TokenType::LeftParen)) {
      auto token = previous();
      if (match(TokenType::RightParen)) {
        return addNode(Encodings::Block{.elements = ChildSpan()}, toIndex(token));
      }
      NodeIndex grouping = expression();
      if (check(TokenType::Comma)) {
        std::vector<NodeIndex> tupleElements{grouping};
        while (!match(TokenType::RightParen)) {
          consume(TokenType::Comma, "Tuples require commas to separate elements");
          if (match(TokenType::RightParen)) {
            break;
          }
          tupleElements.push_back(expression());
        }
        return addNode(Encodings::Block{.elements = ChildSpan(tupleElements)}, toIndex(token));
      }
      consume(TokenType::RightParen, "Expected a closing parenthesis");
      return grouping;
    }

    if (check(TokenType::LeftSquareBracket)) {
      TokenPointer startToken = advance();

      // Multi-pointer
      if (check(TokenType::Pointer) && check(TokenType::RightSquareBracket, {1})) {
        advance();
        advance();

        NodeIndex elementType = expression();
        return addNode(Encodings::UnaryOp{.operand = elementType, .operation = UnaryOps::MultiPointer}, startToken);
      }

      // Slice
      if (check(TokenType::RightSquareBracket)) {
        advance();
        NodeIndex elementType = expression();
        return addNode(Encodings::UnaryOp{.operand = elementType, .operation = UnaryOps::Slice}, startToken);
      }

      std::vector<NodeIndex> items;

      // Array literal
      while (!check(TokenType::RightSquareBracket)) {
        if (items.size() > 0) {
          consume(TokenType::Comma, "Expected separating comma between elements of array literal");
        }

        // Allow trailing comma
        if (check(TokenType::RightSquareBracket)) {
          break;
        }

        items.push_back(expression());

        // TODO: update this as new closing tokens get added
        std::vector<TokenType> closingTokens = {
          TokenType::StatementBreak,
          TokenType::Comma,
          TokenType::RightCurlyBrace,
          TokenType::RightSquareBracket,
          TokenType::RightParen,
        };
        // Sized array
        if (items.size() == 1 && check(TokenType::RightSquareBracket) && !check(std::span(closingTokens), {1})) {
          advance();
          NodeIndex elementType = expression();
          return addNode(Encodings::BinaryOp{.left = items[0], .right = elementType, .operation = startToken});
        }
      }

      consume(TokenType::RightSquareBracket, "Unclosed array literal; Expected ']'");
      auto node = Encodings::Block{.elements = ChildSpan(items)};
      return addNode(node, toIndex(startToken));
    }

    log << fmt::format("Last token: '{}'", latestToken().lexeme) << std::endl;
    throw std::invalid_argument("Unable to parse");
  }

  NodeIndex function() {
    if (check(TokenType::Struct)) {
      return structDefinition();
    }

    consume(TokenType::Function, "Expected 'fn' keyword");
    auto keyword = previous();
    // Params
    consume(TokenType::LeftParen, "Expected '(' for parameter declaration");

    NodeIndex parameters = inputList(Encodings::InputList::InputType::Parameter);

    std::optional<NodeIndex> returnType = std::nullopt;
    if (check(TokenType::ThinArrow)) {
      advance();
      // TODO: make sure this can be evaluated at comptime
      returnType = expression();
    }

    OptionalNode body = std::nullopt;
    // Forward declaration
    if (check(TokenType::StatementBreak)) {
      advance();
    } else {
      body = block();
    }
    auto node = Encodings::FunctionLiteral{.parameters = parameters, .returnType = returnType, .body = body};
    return addNode(node, toIndex(keyword));
  }

  NodeIndex block() {
    consume(TokenType::LeftCurlyBrace, "Expected '{'");
    auto startToken = previous();
    std::vector<NodeIndex> statements;
    std::vector<TokenType> types = {TokenType::RightCurlyBrace};
    acceptN(TokenType::StatementBreak);
    while (!match(types)) {
      // log << "looping\n";
      // peek().print();
      // std::cout << "Finding statements\n";
      statements.push_back(statement());
      acceptN(TokenType::StatementBreak);
    }

    auto node = Encodings::Block{.elements = ChildSpan(statements)};
    return addNode(node, toIndex(startToken));
  }

  NodeIndex structDefinition() {
    consume(TokenType::Struct, "Expected 'struct' token at the beginning of struct definition");
    auto token = previous();

    // TODO: distinct types
    std::vector<NodeIndex> definitions;
    consume(TokenType::LeftCurlyBrace, "Struct field definitions must be declared between {}");
    acceptN(TokenType::StatementBreak);
    while (!check(TokenType::RightCurlyBrace)) {
      definitions.push_back(declaration());
      if (!check(TokenType::RightCurlyBrace)) {
        consumeAtLeast1(TokenType::StatementBreak, "Statement break needed after field definition");
      }
    }
    consume(TokenType::RightCurlyBrace, "Struct field definitions must be declared between {}");

    return addNode(Encodings::Struct{.children = ChildSpan(definitions)}, token);
  }

  NodeIndex whileLoop() {
    TokenPointer token = consume(TokenType::While, "'while' loop requires 'while' keyword");
    NodeIndex condition = expression();
    NodeIndex loopBody = block();
    Encodings::BinaryOp loop = {.left = condition, .right = loopBody, .operation = token};
    return addNode(loop);
  }

  NodeIndex import() {
    // TODO: allow chaining
    auto token = consume(TokenType::Import, "import token required for import expression");
    auto fileNode = consume(TokenType::String, "import must be followed by a file path string");
    return addNode(Encodings::UnaryOp({.operand = {toIndex(fileNode).value}, .operation = UnaryOps::Import}), token);
  }

  NodeIndex enumLiteral() {
    auto token = consume(TokenType::Enum, "enum token required for enum literal");
    OptionalNode rawType;
    if (check(TokenType::LeftParen)) {
      advance();
      rawType = expression();
      consume(TokenType::RightParen, "Expected closing parenthesis after raw value for enum");
    }
    std::vector<TokenIndex> enumValues;
    consume(TokenType::LeftSquareBracket, "enum literals require values to be specified between '{' and '}'");
    acceptN(TokenType::StatementBreak);
    while (!match(TokenType::RightSquareBracket)) {
      auto token = consume(TokenType::Identifier, "enum values must be valid identifiers");
      enumValues.push_back(toIndex(token));
      if (!match(TokenType::Comma)) {
        acceptN(TokenType::StatementBreak);
        consume(TokenType::RightSquareBracket, "Expected closing '}' after final enum value definition");
        break;
      }
      acceptN(TokenType::StatementBreak);
    }
    return addNode(Encodings::Enum{.rawType = rawType, .values = std::span(enumValues)}, token);
  }

  NodeIndex generic() {
    auto token = consume(TokenType::Generic, "Expected 'gn' keyword");

    // Params
    consume(TokenType::LeftSquareBracket, "Expected '[' for parameter declaration");

    bool hasMultiple = false;
    std::vector<NodeIndex> requiredInputs;
    while (!match(TokenType::RightSquareBracket)) {
      if (hasMultiple) {
        consume(TokenType::Comma, "");
        if (match(TokenType::RightSquareBracket)) {
          break;
        }
      }

      hasMultiple = true;
      auto parameterName = consume(TokenType::Identifier, "Expected generic parameter name to be an identifier");
      requiredInputs.push_back(addNode(Encodings::Literal(parameterName)));
    }
    auto parameters =
      addNode(Encodings::InputList{.requiredInputs = ChildSpan(requiredInputs), .optionalInputs = ChildSpan(), .inputType = Encodings::InputList::InputType::Parameter});
    auto value = expression();
    return addNode(Encodings::BinaryOp{.left = parameters, .right = value, .operation = token});
  }

  auto locationOf(NodeIndex node) const {
    auto token = getToken(node);
    return tokenizer.locationOf(token->lexeme);
  }

  template <typename... Args> void crash(NodeIndex node, fmt::format_string<Args...> fmt, Args&&... args) const {
    auto& out = std::cerr;
    auto location = locationOf(node);
    fmt::println(out, "Error in file {} at line {}:{}", tokenizer.inputFilePath.string(), location.line, location.column);
    fmt::println(out, fmt, std::forward<Args>(args)...);
    exit(1);
  }
};
