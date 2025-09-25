#pragma once
#include "tokenizer.h"
#include <bit>
#include <cassert>
#include <iostream>
#include <fmt/core.h>
#include <limits>
#include <optional>
#include <span>
#include <stdexcept>
#include <vector>
#include "logging.h"
#include "common.h"

enum class NodeType {
  DECLARATION,
  UNARY,
  LITERAL,
  FUNCTION_LITERAL,
  BLOCK,
  INPUT_LIST,
  ARRAY_LITERAL,
  BINARY_OP,
  DEFINITION,
  IF,
  STRUCT,
  DOT_ACCESS,
  IMPORT,
  // UNION,
  // ENUM,
  // TRAIT,
  // IMPL,
  // MUT
};

enum class UnaryOps {
  NOT,
  REFERENCE,
  DEREFERENCE,
  SLICE,
  MUTLI_POINTER,
  COMPILER_BUILTIN,
  IMPORT,
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

  // void print(int level = 0) {
  //     std::string indentation(level, '\t');
  //     std::cout << indentation << static_cast<int>(type) << ": ";
  //     token.print();
  //     for (auto child : children) {
  //         child->print(level+1);
  //     }
  // }
};

using TokenPointer = const Token*;
using NodeList = std::vector<TokenIndex> ;
using DataSpan = std::span<i32> ;
using ChildSpan = std::span<NodeIndex> ;
using TokenSpan = std::span<TokenPointer>;
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
  std::span<NodeIndex> statements;
};

struct ArrayLiteral {
  std::span<NodeIndex> elements;
};

struct Definition {
  TokenPointer name;
  OptionalNode type;
};

struct If {
  NodeIndex condition;
  NodeIndex value;
  // Don't need a separate node type for else because else only stores a single expression
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

struct Import {
  TokenSpan path;
  // TODO: import multiple from module
};

}; // namespace Encodings


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

  void acceptN(TokenType type) {
    while (!isAtEnd() && peek().type == type) advance();
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
    return current.value + ahead.value >= tokens.size() || peek(ahead).type == TokenType::END_OF_FILE;
  }

  const TokenPointer previous() { return tokens.data() + current.value - 1; }
  TokenPointer getToken(TokenIndex token) { return &tokens[token.value]; }

  TokenPointer match(std::vector<TokenType> &types) {
    for (auto type : types) {
      if (check(type)) {
        return advance();
      }
    }
    return nullptr;
  }

  TokenPointer consume(TokenType type, std::string message) {
    if (check(type))
      return advance();
    log << advance();
    throw std::invalid_argument(message);
  }

  void consumeAtLeast1(TokenType needed, std::string message) {
    consume(needed, message);
    while (check(needed)) advance();
  }

  const Token &latestToken() {
    auto last = tokens.size() - 1;
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
        .left = node.definition.value,
        .right = node.value.value,
        .token = token,
        .nodeType = NodeType::DECLARATION,
    });
  }

  Encodings::Declaration getDeclaration(NodeIndex index) {
    auto encoded = getNode(index, NodeType::DECLARATION);
    return {.definition = {encoded.left}, .value = {encoded.right}};
  }

  NodeIndex addNode(Encodings::UnaryOp node, TokenPointer token) {
    return addNode(ASTNode{.left = static_cast<i32>(node.operation),
                           .right = node.operand.value,
                           .token = toIndex(token),
                           .nodeType = NodeType::UNARY});
  }

  Encodings::UnaryOp getUnary(NodeIndex index) {
    auto encoded = getNode(index, NodeType::UNARY);
    return {.operand = {encoded.right}, .operation = static_cast<UnaryOps>(encoded.left)};
  }

  NodeIndex addNode(Encodings::Literal node) {
    auto token = toIndex(node.token);
    return addNode(ASTNode{.left = token.value, .token = token, .nodeType = NodeType::LITERAL});
  }

  Encodings::Literal getLiteral(NodeIndex node) {
    auto encoded = getNode(node, NodeType::LITERAL);
    return {.token = toPointer(encoded.token)};
  }
  
  OptionalNode readOptional(NodeIndex node, i32 expectedIndex) {
    if (expectedIndex >= node.value) return std::nullopt;
    return NodeIndex {expectedIndex};
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
    return addNode(ASTNode{.left = dataIndex.value,
                           .right = node.parameters.value,
                           .token = token,
                           .nodeType = NodeType::FUNCTION_LITERAL});
  }

  Encodings::FunctionLiteral getFunctionLiteral(NodeIndex node) {
    auto encoded = getNode(node, NodeType::FUNCTION_LITERAL);
    auto& parameters = extraData;

    auto startIndex = encoded.left;
    return {.parameters = {encoded.right},
            .returnType = readOptional(node, parameters[startIndex]),
            .body = readOptional(node, parameters[startIndex + 1])
          };
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
    return addNode(ASTNode{.left = node.left.value,
                           .right = node.right.value,
                           .token = toIndex(node.operation),
                           .nodeType = NodeType::BINARY_OP});
  }

  Encodings::BinaryOp getBinaryOp(NodeIndex node) {
    auto encoded = getNode(node, NodeType::BINARY_OP);
    return {.left = {encoded.left}, .right = {encoded.right}, .operation = toPointer(encoded.token)};
  }

  NodeIndex addNode(Encodings::Definition node, TokenPointer token) {
    // Type is guaranteed to not have the same index as the definition
    return addNode(ASTNode {.left = toIndex(node.name).value, .right = encodeOptional(node.type), .token = toIndex(token), .nodeType = NodeType::DEFINITION});
  }

  Encodings::Definition getDefinition(NodeIndex node) {
    auto encoded = getNode(node, NodeType::DEFINITION);
    return {.name = toPointer({encoded.left}), .type = readOptional(node, encoded.right)};
  }


  NodeIndex addNode(Encodings::If node, TokenPointer token) {
    std::vector<NodeIndex> children = {node.condition, node.value};
    auto dataIndex = addData(ChildSpan(children));

    return addNode(ASTNode {.left = dataIndex.value, .right = encodeOptional(node.elseValue), .token = toIndex(token), .nodeType = NodeType::IF});
  }
  
  Encodings::If getIf(NodeIndex node) {
    auto encoded = getNode(node, NodeType::IF);
    return {.condition = {extraData[encoded.left]}, .value = {extraData[encoded.left + 1]}, .elseValue = readOptional(node, encoded.right) };
  }

  NodeIndex addNode(Encodings::Struct node, TokenPointer token) {
    auto dataIndex = addData(node.children);
    return addNode(ASTNode {.left = dataIndex.value, .right = (i32) node.children.size(), .token = toIndex(token), .nodeType = NodeType::STRUCT});
  }

  Encodings::Struct getStruct(NodeIndex node) {
    auto encoded = getNode(node, NodeType::STRUCT);
    
    return {.children = getChildren().subspan(encoded.left, encoded.right)};
  }

  NodeIndex addNode(Encodings::DotAccessor node, TokenPointer token) {
    return addNode(ASTNode {.left = node.object.value, .right = toIndex(node.fieldName).value, .token = toIndex(token), .nodeType = NodeType::DOT_ACCESS});
  }

  Encodings::DotAccessor getDotAccess(NodeIndex node) {
    auto encoded = getNode(node,NodeType::DOT_ACCESS);
    return {.object = {encoded.left}, .fieldName = toPointer({encoded.right})};
  }

  Encodings::Import getImportStatement(NodeIndex node) {
    auto encoded = getNode(node, NodeType::IMPORT);
    return {.path = std::bit_cast<TokenSpan>(DataSpan(extraData).subspan(encoded.left, encoded.right))};
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
    if (check(TokenType::STATEMENT_BREAK)) {
      advance();
    }

    NodeIndex node;
    if (check(TokenType::IMPORT)) {
      auto token = advance();

      std::vector<TokenIndex> packagePath;
      auto packageName = consume(TokenType::IDENTIFIER, "Expected package name after 'import'");
      auto dataIndex = addData(toIndex(packageName).value);
      i32 dataLength = 1;
      while(!check(TokenType::STATEMENT_BREAK)) {
        consume(TokenType::DOT, "Imports require dots between subpackage names");
        auto package = consume(TokenType::IDENTIFIER, "Expected package name after 'import'");
        addData(toIndex(package).value);
        dataLength += 1;
      }
      node = addNode(ASTNode{.left = dataIndex.value, .right = dataLength, .token = toIndex(token), .nodeType = NodeType::IMPORT});
    } else {
      node = assignment();
    }

    if (!isAtEnd()) {
      consumeAtLeast1(TokenType::STATEMENT_BREAK, "Expected a breaking statement");
    }
    return node;
  }

  NodeIndex assignment() {
    bool isDefinition = check(TokenType::IDENTIFIER) && check(TokenType::COLON, {1});
    if (isDefinition) {
      return declaration();
    }

    NodeIndex expr = expression();

    if (check(TokenType::ASSIGNMENT)) {
      TokenPointer token = advance();
      auto value = expression();
      return addNode(Encodings::BinaryOp {.left = expr, .right = value, .operation = token});
    }

    return expr;
  }

  NodeIndex declaration() {
      NodeIndex name = definition();
      if (check(TokenType::ASSIGNMENT) || check(TokenType::COLON)) {
        TokenPointer token = advance();
        NodeIndex value = expression();
        log << "Assigning " << getDefinition(name).name->lexeme << "\n";
        return addNode(Encodings::Declaration {.definition = name, .value = value}, toIndex(token));
      }

      log << "Defining " << getDefinition(name).name->lexeme << "\n";
      return name;
  }
  
  NodeIndex definition() {
    TokenPointer name = consume(TokenType::IDENTIFIER, "Expected an identifier for a definition");
    consume(TokenType::COLON, "Expected a ':' for type declaration");
    TokenPointer token = previous();
    bool infer = check(TokenType::COLON) || check(TokenType::ASSIGNMENT);
    if (infer) {
      return addNode(Encodings::Definition {.name = name, .type = std::nullopt}, token);
    }

    NodeIndex type = expression();
    return addNode(Encodings::Definition {.name = name, .type = type}, token);
  }

  NodeIndex expression() { return equality(); }

  NodeIndex equality() {
    static std::vector<TokenType> types = {TokenType::DOUBLE_EQUAL,
                                           TokenType::NOT_EQUAL};

    NodeIndex expr = comparison();
    while (match(types)) {
      TokenPointer op = previous();
      auto node =
          Encodings::BinaryOp{.left = expr, .right = comparison(), .operation = op};
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
          Encodings::BinaryOp{.left = expr, .right = factor(), .operation = op};
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex factor() {
    static std::vector<TokenType> types = {TokenType::MULT, TokenType::DIV, TokenType::SHIFT_RIGHT};
    auto expr = unary();
    while (match(types)) {
      TokenPointer op = previous();
      auto node =
          Encodings::BinaryOp{.left = expr, .right = unary(), .operation = op};
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex unary() {
    NodeIndex expr;
    if (check(TokenType::NOT)) {
      TokenPointer op = previous();
      auto node = Encodings::UnaryOp{.operand = call(), .operation = UnaryOps::NOT};
      expr = addNode(node, op);
    } else if (check(TokenType::POINTER)) {
      fmt::println("!!Reference operation!!");
      TokenPointer op = advance();
      auto node = Encodings::UnaryOp{.operand = call(), .operation = UnaryOps::REFERENCE};
      expr = addNode(node, op);
    } else {
      expr = call();
    }

    if (check(TokenType::POINTER)) {
      fmt::println("!!Dereference operation!!");
      auto node = Encodings::UnaryOp{.operand = expr, .operation = UnaryOps::DEREFERENCE};
      expr = addNode(node, advance());
    }

    if (check(TokenType::DOT)) {
      auto token = advance();
      auto node = Encodings::DotAccessor({.object = expr, .fieldName = consume(TokenType::IDENTIFIER, "Expected identifier after '.' accessor")});
      expr = addNode(node, token);
    }
    return expr;
  }

  NodeIndex call() {
    static std::vector<TokenType> builtinTokens = {TokenType::BUILTIN_RegisterType, TokenType::BUILTIN_NumCast};
    if (auto builtin = match(builtinTokens)) {
      consume(TokenType::LEFT_PAREN, "Compiler builtins must be called like functions");
      auto args = inputList(Encodings::InputList::InputType::Argument);
      return addNode(Encodings::UnaryOp{.operand = args, .operation = UnaryOps::COMPILER_BUILTIN}, builtin);
    }
    
    NodeIndex expr = access();
    if (check(TokenType::LEFT_PAREN)) {
      auto token = advance();
      auto node = Encodings::BinaryOp{.left = expr, .right = inputList(Encodings::InputList::InputType::Argument), .operation = token};
      return addNode(node);
    }

    return expr;
  }
  
  NodeIndex inputList(Encodings::InputList::InputType definitionOrUsage) {
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
    while (!check(TokenType::RIGHT_PAREN)) {
      if (hasMultiple) {
        consume(TokenType::COMMA,
                "Expected separating comma between parameters");
      }

      // Allow trailing comma
      if (check(TokenType::RIGHT_PAREN)) {
        break;
      }

      // TODO: consider how to clean up this code
      if (definitionOrUsage == Encodings::InputList::InputType::Argument) {
        if ((requiredInputs.size() + optionalInputs.size()) > MAX_ARGUMENTS) {
          throw std::invalid_argument("Maximum arguments exceeded");
        }
        if (check(TokenType::IDENTIFIER) && check(TokenType::ASSIGNMENT, {1})) {
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

        bool isOptional = parameterType == NodeType::DECLARATION;
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

    consume(TokenType::RIGHT_PAREN, "Expected ')' for declaration");

    auto dataIndex = addData(requiredInputs);
    addData(optionalInputs);
    i32 packedInputLength = packInt(requiredInputs.size(), optionalInputs.size(), static_cast<i8>(definitionOrUsage), 0);
    return addNode(ASTNode {.left = dataIndex.value, .right = packedInputLength, .nodeType = NodeType::INPUT_LIST});
  }

  Encodings::InputList getInputList(NodeIndex node) {
    auto encoded = getNode(node, NodeType::INPUT_LIST);
    auto [requiredLength, optionalLength, inputType, _] = unpackInt(encoded.right);
    auto children = getChildren();
    return {.requiredInputs = children.subspan(encoded.left, requiredLength), .optionalInputs = children.subspan(encoded.left+requiredLength, optionalLength), .inputType = static_cast<Encodings::InputList::InputType>(inputType)};
  }

  NodeIndex access() {
    NodeIndex expr = primary();
    if (check(TokenType::LEFT_BRACKET)) {
      auto token = advance();
      if (check(TokenType::RIGHT_BRACKET)) {
        return addNode(Encodings::UnaryOp {.operand = expr, .operation = UnaryOps::SLICE}, token);
      }
      auto index = expression();
      consume(TokenType::RIGHT_BRACKET, "Expected a closing ']' after indexing or slicing operation");
      expr = addNode(Encodings::BinaryOp {.left = expr, .right = index, .operation = token});
    }
    
    return expr;
  }

  NodeIndex primary() {
    static std::vector<TokenType> types = {TokenType::STRING,
                                           TokenType::DECIMAL, TokenType::INT, TokenType::NULL_TERMINATED_STRING,
                                           TokenType::TRUE, TokenType::FALSE, TokenType::IDENTIFIER,
                                           TokenType::OPAQUE,
                                         };
    if (auto token = match(types)) {
      auto node = Encodings::Literal{.token = token};
      return addNode(node);
    }

    if (check(TokenType::FUNCTION)) {
      return function();
    }

    if (check(TokenType::STRUCT)) {
      return structDefinition();
    }

    if (check(TokenType::IF)) {
      auto ifToken = advance();
      consume(TokenType::LEFT_PAREN, "Condition for 'if' statement needs to be surrounded by parentheses '('");
      auto condition = expression();
      consume(TokenType::RIGHT_PAREN, "Condition for 'if' statement needs to be surrounded by parentheses ')'");
      auto value = expression();

      auto ifNode = Encodings::If {.condition = condition, .value = value};
      OptionalNode elseNode = std::nullopt;
      if (check(TokenType::ELSE)) {
        advance();
        elseNode = expression();
      }
      ifNode.elseValue = elseNode;
      return addNode(ifNode, ifToken);
    }

    if (check(TokenType::LEFT_PAREN)) {
      advance();
      NodeIndex grouping = expression();
      consume(TokenType::RIGHT_PAREN, "Expected a closing parenthesis");
      return grouping;
    }

    if (check(TokenType::LEFT_BRACKET)) {
      TokenPointer startToken = advance();

      // Multi-pointer
      if (check(TokenType::POINTER) && check(TokenType::RIGHT_BRACKET, {1})) {
        advance();
        advance();

        NodeIndex elementType = expression();
        return addNode(Encodings::UnaryOp{.operand = elementType, .operation = UnaryOps::MUTLI_POINTER}, startToken);
      }

      // Slice
      if (check(TokenType::RIGHT_BRACKET)) {
        advance();
        NodeIndex elementType = expression();
        return addNode(Encodings::UnaryOp{.operand = elementType, .operation = UnaryOps::SLICE}, startToken);
      }
      
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

        // TODO: update this as new closing tokens get added
        std::vector<TokenType> closingTokens = {
          TokenType::STATEMENT_BREAK,
          TokenType::COMMA,
          TokenType::RIGHT_CURLY_BRACE,
          TokenType::RIGHT_BRACKET,
          TokenType::RIGHT_PAREN,
        };
        // Sized array
        if (items.size() == 1 && check(TokenType::RIGHT_BRACKET) && !check(std::span(closingTokens), {1})) {
          advance();
          NodeIndex elementType = expression();
          return addNode(Encodings::BinaryOp{.left=items[0], .right=elementType, .operation=startToken});
        }
      }

      consume(TokenType::RIGHT_BRACKET, "Unclosed array literal; Expected ']'");
      auto node = Encodings::ArrayLiteral{.elements = ChildSpan(items)};
      return addNode(node, toIndex(startToken));
    }

    log << fmt::format("Last token: '{}'", latestToken().lexeme) << std::endl;
    throw std::invalid_argument("Unable to parse");
  }

  NodeIndex function() {
    if (check(TokenType::STRUCT)) {
      return structDefinition();
    } 

    consume(TokenType::FUNCTION, "Expected 'fn' keyword");
    auto keyword = previous();
    // Params
    consume(TokenType::LEFT_PAREN, "Expected '(' for parameter declaration");

    NodeIndex parameters = inputList(Encodings::InputList::InputType::Parameter);

    std::optional<NodeIndex> returnType = std::nullopt;
    if (check(TokenType::COLON)) {
      advance();
      // TODO: make sure this can be evaluated at comptime
      returnType = expression();
    }
    
    OptionalNode body = std::nullopt;
    // Forward declaration
    if (check(TokenType::STATEMENT_BREAK)) {
      advance();
    } else {
      body = block();
    }
    auto node = Encodings::FunctionLiteral {.parameters = parameters, .returnType = returnType, .body = body};
    return addNode(node, toIndex(keyword));
  }

  NodeIndex block() {
    consume(TokenType::LEFT_CURLY_BRACE, "Expected '{'");
    auto startToken = previous();
    std::vector<NodeIndex> statements;
    std::vector<TokenType> types = {TokenType::RIGHT_CURLY_BRACE};
    acceptN(TokenType::STATEMENT_BREAK);
    while (!match(types)) {
      log << "looping\n";
      // peek().print();
      // std::cout << "Finding statements\n";
      statements.push_back(statement());
      acceptN(TokenType::STATEMENT_BREAK);
    }

    auto node = Encodings::Block {.statements = ChildSpan(statements)};
    return addNode(node, toIndex(startToken));
  }

  NodeIndex structDefinition() {
    consume(TokenType::STRUCT, "Expected 'struct' token at the beginning of struct definition");
    auto token = previous();

    // TODO: distinct types
    std::vector<NodeIndex> definitions;
    consume(TokenType::LEFT_CURLY_BRACE, "Struct field definitions must be declared between {}");
    acceptN(TokenType::STATEMENT_BREAK);
    while (!check(TokenType::RIGHT_CURLY_BRACE)) {
      definitions.push_back(declaration());
      if(!check(TokenType::RIGHT_CURLY_BRACE)) {
        consumeAtLeast1(TokenType::STATEMENT_BREAK, "Statement break needed after field definition");
      }
    }
    consume(TokenType::RIGHT_CURLY_BRACE, "Struct field definitions must be declared between {}");

    return addNode(Encodings::Struct {.children = ChildSpan(definitions)}, token);
  }

  NodeIndex whileLoop() {
    TokenPointer token = consume(TokenType::WHILE, "'while' loop requires 'while' keyword");
    NodeIndex condition = expression();
    NodeIndex loopBody = block();
    Encodings::BinaryOp loop = {.left = condition, .right = loopBody, .operation = token};
    return addNode(loop);
  }

  NodeIndex import() {
    // TODO: allow chaining
    auto token = consume(TokenType::IMPORT, "import token required for import statement");
    auto fileNode = consume(TokenType::STRING, "import must be followed by a filename");
    return addNode(Encodings::UnaryOp({.operand = {toIndex(fileNode).value}, .operation = UnaryOps::IMPORT}), token);
  }
};

