// TODO: function calls
#pragma once
#include "../tokenizer.h"
#include <bit>
#include <cassert>
#include <cstdint>
#include <iostream>
#include <span>
#include <stdexcept>
#include <vector>
#include "../logging.h"

enum class NodeType {
  DECLARATION,
  UNARY,
  LITERAL,
  IDENTIFIER,
  FUNCTION_LITERAL,
  FUNCTION_CALL,
  BLOCK,
  PARAMETER_LIST,
  CALL,
  ARGS_LIST,
  ARRAY_LITERAL,
  BINARY_OP,
  POINTER_OP,
  ASSIGNMENT,
  DEFINITION
};

typedef uint32_t i32;

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

  // void print(int level = 0) {
  //     std::string indentation(level, '\t');
  //     std::cout << indentation << static_cast<int>(type) << ": ";
  //     token.print();
  //     for (auto child : children) {
  //         child->print(level+1);
  //     }
  // }
};

typedef const Token *TokenPointer;

namespace Encodings {
struct Declaration {
  NodeIndex identifier;
  NodeIndex value;
};

struct UnaryOp {
  TokenPointer token;
  NodeIndex operand;
};

enum class PointerOpType {
  REFERENCE,
  DEREFERENCE
};

struct PointerOp {
  NodeIndex operand;
  PointerOpType opType;
};

struct BinaryOp {
  TokenPointer token;
  NodeIndex left;
  NodeIndex right;
};

struct Literal {
  TokenPointer token;
};

struct Identifier {
  TokenPointer token;
};

struct FunctionLiteral {
  std::span<NodeIndex> parameters;
  NodeIndex body;
};

struct FunctionCall {
  NodeIndex functionValue;
  std::span<NodeIndex> arguments;
};

struct Block {
  std::span<NodeIndex> statements;
};

struct ArrayLiteral {
  std::span<NodeIndex> elements;
};

struct Definition {
  TokenPointer name;
  NodeIndex type;
  bool inferType;
};

enum class AssignmentType {
  NAME,
  DOT,
  BRACKET
};

struct Assignment {
  NodeIndex assignee;
  NodeIndex value;
};
}; // namespace Encodings

typedef std::vector<TokenIndex> NodeList;
typedef std::span<i32> DataSpan;
typedef std::span<NodeIndex> ChildSpan;

class Parser {
public:
  Parser(std::vector<Token> &tokens) : tokens(tokens), log(logger(LogLevel::DEBUG)) {}
  const std::vector<Token> &tokens;
  std::vector<i32> extraData;
  std::vector<ASTNode> nodes;
  TokenIndex current = {0};
  std::ostream& log;

  const Token &peek(TokenIndex ahead = {0}) {
    return tokens[current.value + ahead.value];
  }

  const TokenPointer advance() {
    if (!isAtEnd())
      current.value++;
    TokenPointer token = previous();
    log << "Advancing: " << token->lexeme << "\n";
    return token;
  }

  void accept(TokenType type) {
    if (isAtEnd())
      return;
    if (peek().type == type) {
      advance();
    }
  }

  bool check(TokenType type, TokenIndex ahead = {0}) {
    if (isAtEnd(ahead)) {
      return false;
    }
    return peek(ahead).type == type;
  }

  bool isAtEnd(TokenIndex ahead = {0}) {
    return peek(ahead).type == TokenType::END_OF_FILE ||
           current.value >= tokens.size();
  }

  const TokenPointer previous() { return tokens.data() + current.value - 1; }
  Token getToken(TokenIndex token) { return tokens[token.value]; }

  bool match(std::vector<TokenType> &types) {
    for (auto type : types) {
      if (check(type)) {
        advance();
        return true;
      }
    }
    return false;
  }

  const TokenPointer consume(TokenType type, std::string message) {
    if (check(type))
      return advance();
    log << advance();
    throw std::invalid_argument(message);
  }

  const Token &latestToken() {
    auto last = static_cast<i32>(tokens.size() - 1);
    return tokens[last > current.value ? current.value : last];
  }

  const NodeIndex nodeIndex() {
    return NodeIndex{.value = (i32)nodes.size() - 1};
  }

  const DataIndex dataIndex() { return {(i32)extraData.size() - 1}; }

  NodeIndex addNode(ASTNode node) {
    nodes.push_back(node);
    return nodeIndex();
  }

  NodeIndex addNode(Encodings::Identifier node) {
    TokenIndex tokenIndex = toIndex(node.token);
    return addNode(ASTNode{.left = tokenIndex.value,
                           .token = tokenIndex,
                           .nodeType = NodeType::IDENTIFIER});
  }

  ASTNode getNode(NodeIndex index) {
    return nodes[index.value];
  }

  ASTNode getNode(NodeIndex index, NodeType type) {
    auto encoded = nodes[index.value];
    assert(encoded.nodeType == type);
    return encoded;
  }

  ChildSpan getChildren() {
    return std::bit_cast<ChildSpan>(std::span<i32>(extraData));
  }

  Encodings::Identifier getIdentifier(NodeIndex index) {
    auto encoded = getNode(index, NodeType::IDENTIFIER);
    return Encodings::Identifier{.token = toPointer({encoded.left})};
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

  TokenPointer toPointer(TokenIndex index) {
    return tokens.data() + index.value;
  }

  NodeIndex addNode(Encodings::ArrayLiteral node, TokenIndex token) {
    DataIndex dataIndex = addData(node.elements);
    return addNode(ASTNode{
        .left = dataIndex.value,
        .right = (i32)node.elements.size(),
        .token = token,
        .nodeType = NodeType::ARRAY_LITERAL,
    });
  }

  Encodings::ArrayLiteral getArrayLiteral(NodeIndex index) {
    auto encoded = getNode(index, NodeType::ARRAY_LITERAL);
    return Encodings::ArrayLiteral{
        .elements = getChildren().subspan(encoded.left, encoded.right)};
  }

  NodeIndex addNode(Encodings::Declaration node, TokenIndex token) {
    return addNode(ASTNode{
        .left = node.identifier.value,
        .right = node.value.value,
        .token = token,
        .nodeType = NodeType::DECLARATION,
    });
  }

  Encodings::Declaration getDeclaration(NodeIndex index) {
    auto encoded = getNode(index, NodeType::DECLARATION);
    return {.identifier = {encoded.left}, .value = {encoded.right}};
  }

  NodeIndex addNode(Encodings::UnaryOp node) {
    auto token = toIndex(node.token);
    return addNode(ASTNode{.left = token.value,
                           .right = node.operand.value,
                           .token = token,
                           .nodeType = NodeType::UNARY});
  }

  Encodings::UnaryOp getUnary(NodeIndex index) {
    auto encoded = getNode(index, NodeType::UNARY);
    return {.token = toPointer(encoded.token), .operand = {encoded.right}};
  }

  NodeIndex addNode(Encodings::Literal node) {
    auto token = toIndex(node.token);
    return addNode(ASTNode{.left = token.value, .token = token, .nodeType = NodeType::LITERAL});
  }

  Encodings::Literal getLiteral(NodeIndex node) {
    auto encoded = getNode(node, NodeType::LITERAL);
    return {.token = toPointer(encoded.token)};
  }

  NodeIndex addNode(Encodings::FunctionLiteral node, TokenIndex token) {
    // Block stored directed after args
    auto dataIndex = addData(node.parameters);
    extraData.push_back(node.body.value);
    return addNode(ASTNode{.left = dataIndex.value,
                           .right = (i32)node.parameters.size(),
                           .token = token,
                           .nodeType = NodeType::FUNCTION_LITERAL});
  }

  Encodings::FunctionLiteral getFunctionLiteral(NodeIndex node) {
    auto encoded = getNode(node, NodeType::FUNCTION_LITERAL);
    auto parameters = getChildren();

    auto startIndex = encoded.left;
    auto length = encoded.right;
    return {.parameters = parameters.subspan(startIndex, length),
            .body = parameters[startIndex + length]};
  }

  NodeIndex addNode(Encodings::FunctionCall node, TokenIndex token) {
    auto index = addData(node.functionValue);

    addData(node.arguments);

    return addNode(ASTNode{.left = index.value,
                           .right = (i32)node.arguments.size(),
                           .token = token,
                           .nodeType = NodeType::FUNCTION_CALL});
  }

  Encodings::FunctionCall getFunctionCall(NodeIndex node) {
    auto encoded = getNode(node, NodeType::FUNCTION_CALL);

    auto args = std::bit_cast<ChildSpan>(std::span<i32>(extraData));

    auto startIndex = encoded.left;
    auto length = encoded.right;
    return {.functionValue = args[startIndex],
            .arguments = args.subspan(startIndex + 1, length)};
  }

  NodeIndex addNode(Encodings::Block node, TokenIndex token) {
    auto index = addData(node.statements);

    return addNode({.left = index.value,
                    .right = (i32)node.statements.size(),
                    .token = token,
                    .nodeType = NodeType::BLOCK});
  }

  Encodings::Block getBlock(NodeIndex node) {
    auto encoded = getNode(node, NodeType::BLOCK);

    auto statements = getChildren();
    return {.statements = getChildren().subspan(encoded.left, encoded.right)};
  }

  NodeIndex addNode(Encodings::BinaryOp node) {
    auto token = toIndex(node.token);
    return addNode(ASTNode{.left = node.left.value,
                           .right = node.right.value,
                           .token = token,
                           .nodeType = NodeType::BINARY_OP});
  }

  Encodings::BinaryOp getBinaryOp(NodeIndex node) {
    auto encoded = getNode(node, NodeType::BINARY_OP);
    return {.token = toPointer(encoded.token), .left = {encoded.left}, .right = {encoded.right} };
  }

  NodeIndex addNode(Encodings::Assignment node, TokenPointer token) {
    std::cout << "Adding assignment " << *token << "\n";
    return addNode(ASTNode {.left = node.assignee.value, .right = node.value.value, .token = toIndex(token), .nodeType = NodeType::ASSIGNMENT});
  }

  Encodings::Assignment getAssignment(NodeIndex node) {
    auto encoded = getNode(node);
    return {.assignee = {encoded.left}, .value = {encoded.right}};
  }

  NodeIndex addNode(Encodings::Definition node, TokenPointer token) {
    // Type is guaranteed to not have the same index as the definition
    NodeIndex typeIndex = node.inferType ? NodeIndex {nodeIndex().value + 1} : node.type;
    return addNode(ASTNode {.left = toIndex(node.name).value, .right = typeIndex.value, .token = toIndex(token), .nodeType = NodeType::DEFINITION});
  }

  Encodings::Definition getDefinition(NodeIndex node) {
    auto encoded = getNode(node, NodeType::DEFINITION);
    bool inferType = encoded.right == node.value;
    return {.name = toPointer({encoded.left}), .type = {encoded.right}, .inferType = inferType };
  }

  NodeIndex addNode(Encodings::PointerOp node, TokenPointer token) {
    return addNode(ASTNode {.left = node.operand.value, .right = (i32) node.opType, .token = toIndex(token), .nodeType = NodeType::POINTER_OP});
  }

  Encodings::PointerOp getPointerOp(NodeIndex node) {
    auto encoded = getNode(node, NodeType::POINTER_OP);
    return {.operand = {encoded.left}, .opType = (Encodings::PointerOpType) encoded.right};
  }

public:
  std::vector<NodeIndex> parse() {
    std::vector<NodeIndex> statements;
    // try {
    while (!isAtEnd()) {
      statements.push_back(statement());
    }
    return statements;
    // } catch (const std::exception& e) {
    //     std::cerr << "Next token:\n";
    //     latestToken().print();
    //     throw e;
    // }
  }

  NodeIndex statement() {
    if (check(TokenType::STATEMENT_BREAK)) {
      advance();
    }
    NodeIndex node = assignment();
    if (!isAtEnd()) {
      consume(TokenType::STATEMENT_BREAK, "Expected a breaking statement");
    }
    return node;
  }

  NodeIndex assignment() {
    bool isDefinition = check(TokenType::IDENTIFIER) && check(TokenType::COLON, {1});
    if (isDefinition) {
      NodeIndex name = definition();
      if (check(TokenType::ASSIGNMENT) || check(TokenType::COLON)) {
        TokenPointer token = advance();
        NodeIndex value = expression();
        std::cout << "Assigning " << getDefinition(name).name->lexeme << "\n";
        return addNode(Encodings::Declaration {.identifier = name, .value = value}, toIndex(token));
      }

      std::cout << "Defining " << getDefinition(name).name->lexeme << "\n";
      return name;
    }

    NodeIndex expr = expression();

    if (check(TokenType::ASSIGNMENT)) {
      TokenPointer token = advance();
      auto value = expression();
      return addNode(Encodings::Assignment {.assignee = expr, .value = value}, token);
    }

    return expr;
  }

  NodeIndex definition() {
    TokenPointer name = consume(TokenType::IDENTIFIER, "Expected an identifier for a definition");
    consume(TokenType::COLON, "Expected a ':' for type declaration");
    TokenPointer token = previous();
    bool infer = check(TokenType::COLON) || check(TokenType::ASSIGNMENT);
    if (infer) {
      return addNode(Encodings::Definition {.name = name, .inferType = infer}, token);
    }

    NodeIndex type = expression();
    return addNode(Encodings::Definition {.name = name, .type = type, .inferType = false}, token);
  }

  NodeIndex expression() { return equality(); }

  NodeIndex equality() {
    static std::vector<TokenType> types = {TokenType::DOUBLE_EQUAL,
                                           TokenType::NOT_EQUAL};

    NodeIndex expr = comparison();
    while (match(types)) {
      TokenPointer op = previous();
      auto node =
          Encodings::BinaryOp{.token = op, .left = expr, .right = comparison()};
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex comparison() {
    static std::vector<TokenType> types = {};
    return term();
  }

  NodeIndex term() {
    static std::vector<TokenType> types = {TokenType::PLUS, TokenType::MINUS};
    auto expr = factor();
    while (match(types)) {
      TokenPointer op = previous();
      auto node =
          Encodings::BinaryOp{.token = op, .left = expr, .right = factor()};
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex factor() {
    static std::vector<TokenType> types = {TokenType::MULT, TokenType::DIV};
    auto expr = unary();
    while (match(types)) {
      TokenPointer op = previous();
      auto node =
          Encodings::BinaryOp{.token = op, .left = expr, .right = unary()};
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex unary() {
    static std::vector<TokenType> ops = {TokenType::NOT};
    NodeIndex expr;
    if (match(ops)) {
      TokenPointer op = previous();
      auto node = Encodings::UnaryOp{.token = op, .operand = call()};
      expr = addNode(node);
    } else if (check(TokenType::POINTER)) {
      TokenPointer op = advance();
      auto node = Encodings::PointerOp{.operand = call(), .opType = Encodings::PointerOpType::REFERENCE};
      expr = addNode(node, op);
    } else {
      expr = call();
    }

    if (check(TokenType::POINTER)) {
      auto node = Encodings::PointerOp({.operand = expr, .opType = Encodings::PointerOpType::DEREFERENCE});
      expr = addNode(node, advance());
    }
    // TODO: dot accessor
    return expr;
  }

  NodeIndex call() {
    NodeIndex expr = primary();
    static std::vector<TokenType> open = {TokenType::LEFT_PAREN};
    if (match(open)) {
      log << "Looking in here\n";
      auto token = previous();
      std::vector<NodeIndex> args;
      while (!check(TokenType::RIGHT_PAREN)) {
        log << "finding arg\n";
        args.push_back(expression());
      }

      consume(TokenType::RIGHT_PAREN,
              "Expected a closing ')' after arguments for function call");
      auto node = Encodings::FunctionCall{.functionValue = expr,
                                          .arguments = ChildSpan(args)};
      return addNode(node, toIndex(token));
    }

    return expr;
  }

  NodeIndex primary() {
    static std::vector<TokenType> types = {TokenType::STRING,
                                           TokenType::DECIMAL, TokenType::INT};
    if (match(types)) {
      auto node = Encodings::Literal{.token = previous()};
      return addNode(node);
    }

    if (check(TokenType::FUNCTION)) {
      return function();
    }

    if (check(TokenType::IDENTIFIER)) {
      auto node = Encodings::Identifier{.token = advance()};
      return addNode(node);
    }

    static std::vector<TokenType> leftParen = {TokenType::LEFT_PAREN};
    if (match(leftParen)) {
      NodeIndex grouping = expression();
      consume(TokenType::RIGHT_PAREN, "Expected a closing parenthesis");
      return grouping;
    }

    if (check(TokenType::LEFT_BRACKET)) {
      TokenPointer startToken = advance();
      std::vector<NodeIndex> items;

      // Array literal
      while (!check(TokenType::RIGHT_BRACKET)) {
        if (items.size() > 0) {
          consume(
              TokenType::COMMA,
              "Expected separating comma between elements of array literal");
        }

        // Allow trailing comma
        if (check(TokenType::RIGHT_BRACKET)) {
          break;
        }

        items.push_back(expression());
      }

      consume(TokenType::RIGHT_BRACKET, "Unclosed array literal; Expected ']'");
      auto node = Encodings::ArrayLiteral{.elements = ChildSpan(items)};
      return addNode(node, toIndex(startToken));
    }

    log << "Last token:\n";
    log << latestToken();
    throw std::invalid_argument("Unable to parse");
  }

  NodeIndex function() {
    // Keyword
    consume(TokenType::FUNCTION, "Expected 'fn' keyword");
    auto keyword = previous();
    // Params
    consume(TokenType::LEFT_PAREN, "Expected '(' for parameter declaration");

    std::vector<NodeIndex> parameters;
    while (!check(TokenType::RIGHT_PAREN)) {
      if (parameters.size() > 0) {
        consume(TokenType::COMMA,
                "Expected separating comma between elements of array literal");
      }

      // Allow trailing comma
      if (check(TokenType::RIGHT_PAREN)) {
        break;
      }

      parameters.push_back(expression());
    }
    consume(TokenType::RIGHT_PAREN, "Expected ')' for declaration");

    // Block
    auto body = block();
    auto node = Encodings::FunctionLiteral {.parameters = ChildSpan(parameters), .body = body};
    return addNode(node, toIndex(keyword));
  }

  NodeIndex block() {
    consume(TokenType::LEFT_CURLY_BRACE, "Expected '{'");
    auto startToken = previous();
    std::vector<NodeIndex> statements;
    std::vector<TokenType> types = {TokenType::RIGHT_CURLY_BRACE};
    while (!match(types)) {
      log << "looping\n";
      // peek().print();
      // std::cout << "Finding statements\n";
      statements.push_back(statement());
    }

    auto node = Encodings::Block {.statements = ChildSpan(statements)};
    return addNode(node, toIndex(startToken));
  }
};

