#pragma once
#include "common.h"
#include "fmt/base.h"
#include "tokenizer.h"
#include <bit>
#include <fmt/core.h>
#include <iostream>
#include <span>
#include <stack>
#include <vector>

enum class NodeType {
  Declaration,
  Unary,
  Literal,
  FunctionLiteral,
  Block,
  BinaryOp,
  Definition,
  If,
  Struct,
  DotAccess,
  Enum,
  MultiLineString,
  ArgumentList,
  ParameterList,
  Assignment,
  Apply,
};

enum class UnaryOps {
  Not,
  Reference,
  Dereference,
  SliceType,
  MakeSlice,
  MultiPointerTo,
  MultiPointerFrom,
  CompilerBuiltin,
  Import,
  CudaImport,
  Minus,
  BitNot,
  Return,
  Using,
  BuiltinName,
  CudaPtx,
  /* TODO: make these builtins their own nodes
    Link,
    LinkDir,
    Type,
    Include,
    */
  SizeOf,
  AlignOf,
  BitSize,
  Type,
  PtrCast,
  BInclude,
  Raw,
};

struct NodeIndex {
  u32 value;
  static inline NodeIndex null() {
    return {0};
  }

  explicit operator bool() const {
    return value != 0;
  }
};

struct DataIndex {
  u32 value;
};

struct TokenIndex {
  u32 value;
};

struct ASTNode {
  u32 left;
  u32 right;
  TokenIndex token;
  NodeType nodeType;
};

using DataSpan = std::span<u32>;
using ChildSpan = std::span<NodeIndex>;
using TokenSpan = std::span<TokenIndex>;
using OptionalNode = NodeIndex;

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
  OptionalNode object;
  Token fieldName;
};

struct BinaryOp {
  NodeIndex left;
  NodeIndex right;
  Token operation;
};

struct Literal {
  Token token;
};

struct Identifier {
  Token token;
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
  Token name;
  OptionalNode type;
};

struct If {
  NodeIndex condition;
  NodeIndex ifClause;
  // Don't need a separate node type for else because else only stores a single
  // expression
  // TODO: consider adding back else node for clearer messages?
  OptionalNode elseClause;
};

struct Struct {
  ChildSpan children;
  OptionalNode implBlock;
};

struct NamedValue {
  TokenIndex token;
  NodeIndex value;
};
using NamedValues = std::span<NamedValue>;

struct Enum {
  OptionalNode rawType;
  NamedValues entries;
};

struct ForLoop {
  Definition capture;
  NodeIndex iterator;
  NodeIndex body;
};

struct ArgumentList {
  std::span<NodeIndex> positional;
  Encodings::NamedValues named;
};

struct ParameterList {
  std::span<NodeIndex> requiredParameters;
  std::span<NodeIndex> optionalParameters;
};
}; // namespace Encodings

class Parser {

public:
  static std::unordered_map<TokenType, UnaryOps> builtinUnary;
  Parser(Tokenizer& tokenizer)
      : tokenizer(tokenizer), tokens(tokenizer.tokens), log(LogLevel::Parsing) {
  }
  const Tokenizer& tokenizer;
  const std::vector<Token>& tokens;
  std::vector<u32> extraData;
  std::vector<ASTNode> nodes;
  TokenIndex current = {0};
  Logger log;

  Token peek(TokenIndex ahead = {0}) const {
    auto index = current.value + ahead.value;
    if (index >= tokens.size()) {
      crash(
        latestToken(),
        "Peeking too far ahead: {} > {}",
        index,
        tokens.size()
      );
    }
    return tokens[index];
  }

  Token advance() {
    if (isAtEnd()) {
      crash(previous(), "Failed parsing by reaching end of file");
    }
    current.value++;
    return previous();
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

  Token acceptUntil(TokenType filler, TokenType expected) {
    u32 ahead = 0;
    while (!isAtEnd()) {
      auto c = peek({ahead});
      if (c.type == filler) {
        ahead++;
      } else if (c.type == expected) {
        current.value += ahead;
        return advance();
      } else {
        return {};
      }
    }
    crash(
      advance(),
      "End of file while searching for token type {}",
      static_cast<u32>(expected)
    );
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
    return current.value + ahead.value >= tokens.size() ||
           peek(ahead).type == TokenType::EndOfFile;
  }

  Token previous(u32 behind = 1) const {
    return tokens[current.value - behind];
  }

  Token getToken(TokenIndex token) const {
    return tokens[token.value];
  }

  TokenIndex getTokenIndex(NodeIndex node) const {
    return getNode(node).token;
  }

  Token getToken(NodeIndex node) const {
    return getToken(getNode(node).token);
  }

  Token match(TokenType type) {
    if (check(type)) {
      return advance();
    }
    return {};
  }

  Token match(std::vector<TokenType>& types) {
    for (auto type : types) {
      if (check(type)) {
        return advance();
      }
    }
    return {};
  }

  Token consume(TokenType type, std::string message) {
    if (check(type)) return advance();
    crash(advance(), "{}", message);
  }

  Token consume(span<TokenType> type, std::string message) {
    if (check(type)) return advance();
    crash(advance(), "{}", message);
  }

  void consumeN(TokenType needed, std::string message) {
    consume(needed, message);
    while (check(needed))
      advance();
  }

  Token latestToken() const {
    auto last = tokens.size() - 1;
    return tokens[last > current.value ? current.value : last];
  }

  const DataIndex dataIndex() {
    return {(u32)extraData.size() - 1};
  }

  NodeIndex addNode(ASTNode node) {
    nodes.push_back(node);
    return {(u32)nodes.size()};
  }

  ASTNode getNode(NodeIndex index) const {
    assert(index.value != 0);
    return nodes[index.value - 1];
  }

  NodeType nodeType(NodeIndex index) const {
    return getNode(index).nodeType;
  }

  ASTNode getNode(NodeIndex index, NodeType type) const {
    auto encoded = getNode(index);
    assert(encoded.nodeType == type);
    return encoded;
  }

  ChildSpan getChildren() {
    return std::bit_cast<ChildSpan>(std::span<u32>(extraData));
  }

  DataIndex addData(NodeIndex node) {
    return addData(node.value);
  }

  DataIndex addData(u32 data) {
    DataIndex start = {(u32)extraData.size()};
    extraData.push_back(data);
    return start;
  }

  DataIndex addData(DataSpan newData) {
    DataIndex start = {(u32)extraData.size()};
    extraData.insert(extraData.end(), newData.begin(), newData.end());
    return start;
  }

  DataIndex addData(Encodings::NamedValues data) {
    DataIndex start = {(u32)extraData.size()};
    for (auto [name, value] : data) {
      addData(name.value);
      addData(value.value);
    }
    return start;
  }

  DataIndex addData(ChildSpan newData) {
    return addData(std::bit_cast<DataSpan>(newData));
  }

  TokenIndex toIndex(Token token) const {
    for (u32 i = 0; i < tokens.size(); i++) {
      if (
        tokens[i].location == token.location && tokens[i].type == token.type &&
        tokens[i].identifier == token.identifier
      ) {
        return {i};
      }
    }
    TODO("Token does not belong to this parser");
  }

  NodeIndex addNode(Encodings::Declaration node, TokenIndex token) {
    return addNode(
      ASTNode{
        .left = node.definition.value,
        .right = node.value.value,
        .token = token,
        .nodeType = NodeType::Declaration,
      }
    );
  }

  Encodings::Declaration getDeclaration(NodeIndex index) {
    auto encoded = getNode(index, NodeType::Declaration);
    return {.definition = {encoded.left}, .value = {encoded.right}};
  }

  NodeIndex addNode(Encodings::UnaryOp node, Token token) {
    return addNode(
      ASTNode{
        .left = static_cast<u32>(node.operation),
        .right = node.operand.value,
        .token = toIndex(token),
        .nodeType = NodeType::Unary
      }
    );
  }

  Encodings::UnaryOp getUnary(NodeIndex index) {
    auto encoded = getNode(index, NodeType::Unary);
    return {
      .operand = {encoded.right},
      .operation = static_cast<UnaryOps>(encoded.left)
    };
  }

  NodeIndex addNode(Encodings::Literal node) {
    auto token = toIndex(node.token);
    return addNode(
      ASTNode{
        .left = token.value,
        .token = token,
        .nodeType = NodeType::Literal
      }
    );
  }

  Encodings::Literal getLiteral(NodeIndex node) {
    auto encoded = getNode(node, NodeType::Literal);
    return {.token = getToken(encoded.token)};
  }

  NodeIndex addNode(Encodings::FunctionLiteral node, TokenIndex token) {
    // Block stored directed after args
    auto dataIndex = addData(node.returnType);
    addData(node.body);
    return addNode(
      ASTNode{
        .left = dataIndex.value,
        .right = node.parameters.value,
        .token = token,
        .nodeType = NodeType::FunctionLiteral
      }
    );
  }

  Encodings::FunctionLiteral getFunctionLiteral(NodeIndex node) const {
    auto encoded = getNode(node, NodeType::FunctionLiteral);
    auto& parameters = extraData;

    auto startIndex = encoded.left;
    return {
      .parameters = {encoded.right},
      .returnType = {parameters[startIndex]},
      .body = {parameters[startIndex + 1]}
    };
  }

  NodeIndex addNode(Encodings::Block node, TokenIndex token) {
    auto index = addData(node.elements);

    return addNode(
      {.left = index.value,
       .right = (u32)node.elements.size(),
       .token = token,
       .nodeType = NodeType::Block}
    );
  }

  Encodings::Block getBlock(NodeIndex node) {
    auto encoded = getNode(node, NodeType::Block);

    return {.elements = getChildren().subspan(encoded.left, encoded.right)};
  }

  NodeIndex addNode(Encodings::BinaryOp node) {
    return addNode(
      ASTNode{
        .left = node.left.value,
        .right = node.right.value,
        .token = toIndex(node.operation),
        .nodeType = NodeType::BinaryOp
      }
    );
  }

  Encodings::BinaryOp getBinaryOp(NodeIndex node) {
    auto encoded = getNode(node, NodeType::BinaryOp);
    return {
      .left = {encoded.left},
      .right = {encoded.right},
      .operation = getToken(encoded.token)
    };
  }

  NodeIndex addNode(Encodings::Definition node, Token token) {
    // Type is guaranteed to not have the same index as the definition
    return addNode(
      ASTNode{
        .left = toIndex(node.name).value,
        .right = node.type.value,
        .token = toIndex(token),
        .nodeType = NodeType::Definition
      }
    );
  }

  Encodings::Definition getDefinition(NodeIndex node) {
    auto encoded = getNode(node, NodeType::Definition);
    return {
      .name = getToken(TokenIndex{encoded.left}),
      .type = {encoded.right}
    };
  }

  NodeIndex addNode(Encodings::If node, Token token) {
    std::vector<NodeIndex> children = {node.condition, node.ifClause};
    auto dataIndex = addData(ChildSpan(children));

    return addNode(
      ASTNode{
        .left = dataIndex.value,
        .right = node.elseClause.value,
        .token = toIndex(token),
        .nodeType = NodeType::If
      }
    );
  }

  Encodings::If getIf(NodeIndex node) {
    auto encoded = getNode(node, NodeType::If);
    return {
      .condition = {extraData[encoded.left]},
      .ifClause = {extraData[encoded.left + 1]},
      .elseClause = {encoded.right}
    };
  }

  NodeIndex addNode(Encodings::Struct node, Token token) {
    auto dataIndex = addData(node.children);
    addData(node.implBlock);
    return addNode(
      ASTNode{
        .left = dataIndex.value,
        .right = (u32)node.children.size(),
        .token = toIndex(token),
        .nodeType = NodeType::Struct
      }
    );
  }

  Encodings::Struct getStruct(NodeIndex node) {
    auto encoded = getNode(node, NodeType::Struct);

    auto children = getChildren();
    return {
      .children = children.subspan(encoded.left, encoded.right),
      .implBlock = {children[encoded.left + encoded.right].value}
    };
  }

  NodeIndex addNode(Encodings::DotAccessor node, Token token) {
    return addNode(
      ASTNode{
        .left = node.object.value,
        .right = toIndex(node.fieldName).value,
        .token = toIndex(token),
        .nodeType = NodeType::DotAccess
      }
    );
  }

  Encodings::DotAccessor getDotAccess(NodeIndex node) {
    auto encoded = getNode(node, NodeType::DotAccess);
    return {
      .object = {encoded.left},
      .fieldName = getToken(TokenIndex{encoded.right})
    };
  }

  NodeIndex addNode(Encodings::Enum node, Token token) {
    auto dataIndex = addData(node.rawType);
    addData(node.entries);
    return addNode(
      ASTNode{
        .left = dataIndex.value,
        .right = (u32)node.entries.size(),
        .token = toIndex(token),
        .nodeType = NodeType::Enum
      }
    );
  }

  Encodings::Enum getEnumDefinition(NodeIndex node) {
    auto encoded = getNode(node, NodeType::Enum);
    auto rawType = extraData[encoded.left];
    auto entries = std::bit_cast<Encodings::NamedValues>(
      std::span(extraData).subspan(encoded.left + 1, encoded.right)
    );

    return {.rawType = {rawType}, .entries = entries};
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
    acceptN(TokenType::StatementBreak);

    NodeIndex node;

    if (auto storage = match(TokenType::BUILTIN_Shared)) {
      auto name = consume(
        TokenType::Identifier,
        "Expected an identifier after '@shared'"
      );
      consume(TokenType::Colon, "Expected ':' after shared identifier");
      auto type = expression();
      node =
        addNode(Encodings::Definition{.name = name, .type = type}, storage);
    } else {
      bool isDeclaration =
        check(TokenType::Identifier) && check(TokenType::Colon, {1});
      node = isDeclaration ? declaration() : assignment();
    }

    if (!(isAtEnd() || check(TokenType::RightCurlyBrace))) {
      consumeN(TokenType::StatementBreak, "Expected a breaking statement");
    }
    return node;
  }

  NodeIndex definition() {
    Token name =
      consume(TokenType::Identifier, "Expected an identifier for a definition");
    consume(TokenType::Colon, "Expected a ':' for type declaration");
    Token token = previous();
    bool infer = check(TokenType::Colon) || check(TokenType::Assign);
    if (infer) {
      return addNode(
        Encodings::Definition{.name = name, .type = NodeIndex::null()},
        token
      );
    }

    NodeIndex type = expression();
    return addNode(Encodings::Definition{.name = name, .type = type}, token);
  }

  NodeIndex declaration() {
    NodeIndex name = definition();
    if (check(TokenType::Assign) || check(TokenType::Colon)) {
      Token token = advance();
      NodeIndex value = expression();
      log("Assigning {}", tokenizer.lexeme(getDefinition(name).name));
      return addNode(
        Encodings::Declaration{.definition = name, .value = value},
        toIndex(token)
      );
    }

    return name;
  }

  NodeIndex addAssignment(NodeIndex left, NodeIndex right, Token token) {
    return addNode(
      ASTNode{
        .left = left.value,
        .right = right.value,
        .token = toIndex(token),
        .nodeType = NodeType::Assignment
      }
    );
  }

  NodeIndex assignment() {
    NodeIndex expr = expression();

    if (auto token = match(TokenType::Assign)) {
      log("Regular assignment");
      auto value = expression();
      return addAssignment(expr, value, token);
    }
    if (Token::binopFromCompoundAssignment(peek().type).has_value()) {
      log("Binop assignment");
      auto token = advance();
      auto value = expression();
      return addAssignment(expr, value, token);
    }

    return expr;
  }

  NodeIndex expression() {
    auto expr = logicalOr();
    return expr;
  }

  NodeIndex logicalOr() {
    NodeIndex expr;
    if (auto op = match(TokenType::ExclusiveRange)) {
      auto node = Encodings::BinaryOp{
        .left = NodeIndex::null(),
        .right = logicalAnd(),
        .operation = op
      };
      expr = addNode(node);
    } else {
      expr = logicalAnd();
    }
    while (true) {
      if (auto op = match(TokenType::LogicOr)) {
        auto node = Encodings::BinaryOp{
          .left = expr,
          .right = logicalAnd(),
          .operation = op
        };
        expr = addNode(node);
      } else if (auto op = match(TokenType::ExclusiveRange)) {
        if (peek().isClosingToken()) {
          auto node = Encodings::BinaryOp{
            .left = expr,
            .right = NodeIndex::null(),
            .operation = op
          };
          expr = addNode(node);
        } else {
          auto node = Encodings::BinaryOp{
            .left = expr,
            .right = logicalAnd(),
            .operation = op
          };
          expr = addNode(node);
        }
      } else break;
    }
    return expr;
  }

  NodeIndex logicalAnd() {
    NodeIndex expr = equality();
    while (auto op = match(TokenType::LogicAnd)) {
      auto node =
        Encodings::BinaryOp{.left = expr, .right = equality(), .operation = op};
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex equality() {
    static std::vector<TokenType> types = {
      TokenType::DoubleEqual,
      TokenType::NotEqual
    };

    NodeIndex expr = comparison();
    while (auto op = match(types)) {
      expr = addNode(
        Encodings::BinaryOp{
          .left = expr,
          .right = comparison(),
          .operation = op
        }
      );
    }
    return expr;
  }

  NodeIndex comparison() {
    static std::vector<TokenType> types =
      {TokenType::Lt, TokenType::Leq, TokenType::Gt, TokenType::Geq};
    auto expr = bitwiseOr();
    while (auto op = match(types)) {
      acceptN(TokenType::StatementBreak);
      auto node = Encodings::BinaryOp{
        .left = expr,
        .right = bitwiseOr(),
        .operation = op
      };
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex bitwiseOr() {
    auto expr = bitwiseXor();
    while (auto op = match(TokenType::BitOr)) {
      acceptN(TokenType::StatementBreak);
      auto node = Encodings::BinaryOp{
        .left = expr,
        .right = bitwiseXor(),
        .operation = op
      };
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex bitwiseXor() {
    auto expr = bitwiseAnd();
    while (auto op = match(TokenType::Xor)) {
      acceptN(TokenType::StatementBreak);
      auto node = Encodings::BinaryOp{
        .left = expr,
        .right = bitwiseAnd(),
        .operation = op
      };
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex bitwiseAnd() {
    auto expr = shift();
    while (auto op = match(TokenType::BitAnd)) {
      acceptN(TokenType::StatementBreak);
      auto node =
        Encodings::BinaryOp{.left = expr, .right = shift(), .operation = op};
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex shift() {
    static std::vector<TokenType> types = {
      TokenType::ShiftRight,
      TokenType::ShiftLeft
    };
    auto expr = addition();
    while (match(types)) {
      Token op = previous();
      acceptN(TokenType::StatementBreak);
      auto node =
        Encodings::BinaryOp{.left = expr, .right = addition(), .operation = op};
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex addition() {
    static std::vector<TokenType> types = {TokenType::Plus, TokenType::Minus};
    auto expr = multiplication();
    while (auto op = match(types)) {
      acceptN(TokenType::StatementBreak);
      auto node = Encodings::BinaryOp{
        .left = expr,
        .right = multiplication(),
        .operation = op
      };
      expr = addNode(node);
    }
    return expr;
  }

  NodeIndex multiplication() {
    static std::vector<TokenType> productOps =
      {TokenType::Mult, TokenType::Div, TokenType::Remainder};
    auto expr = unary();
    while (true) {
      if (auto op = match(productOps)) {
        acceptN(TokenType::StatementBreak);
        auto node =
          Encodings::BinaryOp{.left = expr, .right = unary(), .operation = op};
        expr = addNode(node);
      } else if (auto token = match(TokenType::Impl)) {
        consume(TokenType::LeftCurlyBrace, "'impl' block must start with '{'");
        vector<NodeIndex> declarations;
        while (!match(TokenType::RightCurlyBrace)) {
          acceptN(TokenType::StatementBreak);
          declarations.push_back(declaration());
          acceptN(TokenType::StatementBreak);
        }
        auto block =
          addNode(Encodings::Block{.elements = declarations}, toIndex(token));
        expr = addNode(
          Encodings::BinaryOp{.left = expr, .right = block, .operation = token}
        );
      } else if (peek().canApply()) {
        // Right associative
        auto token = current;
        auto applicant = multiplication();
        expr = addNode(
          ASTNode{
            .left = expr.value,
            .right = applicant.value,
            .token = token,
            .nodeType = NodeType::Apply
          }
        );
      } else break;
    }
    return expr;
  }

  NodeIndex power() {
    auto expr = call();
    if (auto op = match(TokenType::Power)) {
      acceptN(TokenType::StatementBreak);
      expr = addNode(
        Encodings::BinaryOp{
          .left = expr,
          .right = unary(),
          .operation = op,
        }
      );
    }
    return expr;
  }

  NodeIndex unary() {
    NodeIndex expr;
    std::stack<pair<Token, UnaryOps>> stack;
    // Prefix
    while (true) {
      UnaryOps op;
      Token token;
      if ((token = match(TokenType::Not))) {
        op = UnaryOps::Not;
      } else if ((token = match(TokenType::Pointer))) {
        op = UnaryOps::Reference;
      } else if ((token = match(TokenType::Minus))) {
        op = UnaryOps::Minus;
      } else if ((token = match(TokenType::Xor))) {
        op = UnaryOps::BitNot;
      } else if ((token = match(TokenType::MultiPointer))) {
        op = UnaryOps::MultiPointerTo;
      } else {
        expr = power();
        break;
      }
      stack.push({token, op});
    }

    while (!stack.empty()) {
      auto [token, op] = stack.top();
      expr =
        addNode(Encodings::UnaryOp{.operand = expr, .operation = op}, token);
      stack.pop();
    }
    return expr;
  }

  NodeIndex call() {
    NodeIndex expr = primary();
    while (true) {
      if (auto token = match(TokenType::LeftParen)) {
        auto node = Encodings::BinaryOp{
          .left = expr,
          .right = argumentList(),
          .operation = token
        };
        expr = addNode(node);
      } else if (auto token = match(TokenType::LeftSquareBracket)) {
        if (match(TokenType::RightSquareBracket)) {
          log("We making a slice");
          if (log.canLog())
            tokenizer.locationOf(getToken(expr)).underline(std::cout);
          return addNode(
            Encodings::UnaryOp{
              .operand = expr,
              .operation = UnaryOps::MakeSlice
            },
            token
          );
        }
        auto index = argumentList(TokenType::RightSquareBracket);
        expr = addNode(
          Encodings::BinaryOp{.left = expr, .right = index, .operation = token}
        );
      } else if (match(TokenType::Pointer)) {
        auto node = Encodings::UnaryOp{
          .operand = expr,
          .operation = UnaryOps::Dereference
        };
        expr = addNode(node, previous());
      } else if (auto token = match(TokenType::Dot)) {
        auto node = Encodings::DotAccessor(
          {.object = expr,
           .fieldName = consume(
             TokenType::Identifier,
             "Expected identifier after '.' accessor"
           )}
        );
        expr = addNode(node, token);
      } else if (auto token = match(TokenType::MultiPointer)) {
        auto node = Encodings::UnaryOp{
          .operand = expr,
          .operation = UnaryOps::MultiPointerFrom
        };
        expr = addNode(node, token);
      } else {
        break;
      }
    }

    return expr;
  }

  NodeIndex argumentList(
    TokenType closingBracket = TokenType::RightParen,
    bool canBeUsedAsGrouping = false
  ) {
    bool hasMultiple = false;
    std::vector<NodeIndex> requiredInputs;
    std::vector<Encodings::NamedValue> optionalInputs;

    // allow combined length to fit in signed 8
    static const u32 MAX_ARGUMENTS = 127;
    auto token = toIndex(previous());
    while (!match(closingBracket)) {
      acceptN(TokenType::StatementBreak);
      if (hasMultiple) {
        consume(
          TokenType::Comma,
          "Expected separating comma between parameters"
        );
        acceptN(TokenType::StatementBreak);
        canBeUsedAsGrouping = false;
      }

      if (match(closingBracket)) {
        break;
      }

      if ((requiredInputs.size() + optionalInputs.size()) > MAX_ARGUMENTS) {
        if (optionalInputs.empty()) {
          crash(requiredInputs.back(), "Maximum number of arguments exceeded");
        }

        crash(
          getToken(optionalInputs.back().token),
          "Maximum number of arguments exceeded"
        );
      }
      if (check(TokenType::Identifier) && check(TokenType::Assign, {1})) {
        auto name = advance();
        advance();
        optionalInputs.push_back({toIndex(name), assignment()});
      } else if (!optionalInputs.empty()) {
        crash(
          advance(),
          "All arguments following a named argument must be named"
        );
      } else {
        requiredInputs.push_back(expression());
      }
      hasMultiple = true;
      acceptN(TokenType::StatementBreak);
    }

    if (
      canBeUsedAsGrouping && requiredInputs.size() == 1 &&
      optionalInputs.empty()
    ) {
      return requiredInputs[0];
    }

    auto dataIndex = addData(requiredInputs);
    for (auto [name, value] : optionalInputs) {
      addData(name.value);
      addData(value);
    }
    u32 packedInputLength =
      packInt(requiredInputs.size(), optionalInputs.size(), 0, 0);
    return addNode(
      ASTNode{
        .left = dataIndex.value,
        .right = packedInputLength,
        .token = token,
        .nodeType = NodeType::ArgumentList
      }
    );
  }

  Encodings::ArgumentList getArgumentList(NodeIndex node) {
    auto encoded = getNode(node, NodeType::ArgumentList);
    auto [requiredLength, optionalLength, unused, _] = unpackInt(encoded.right);
    auto children = getChildren();
    return {
      .positional = children.subspan(encoded.left, requiredLength),
      .named = std::bit_cast<Encodings::NamedValues>(
        children.subspan(encoded.left + requiredLength, optionalLength)
      ),
    };
  }

  Encodings::ParameterList getParameterList(NodeIndex node) {
    auto encoded = getNode(node, NodeType::ParameterList);
    auto [requiredLength, optionalLength, inputType, _] =
      unpackInt(encoded.right);
    auto children = getChildren();
    return {
      .requiredParameters = children.subspan(encoded.left, requiredLength),
      .optionalParameters =
        children.subspan(encoded.left + requiredLength, optionalLength),
    };
  }

  NodeIndex parameterList(TokenType closingBracket = TokenType::RightParen) {
    bool hasMultiple = false;
    std::vector<NodeIndex> requiredInputs;
    std::vector<NodeIndex> optionalInputs;

    auto token = toIndex(previous());

    // allow combined length to fit in signed 8
    static const u32 MAX_ARGUMENTS = 127;
    while (!check(closingBracket)) {
      acceptN(TokenType::StatementBreak);
      if (hasMultiple) {
        consume(
          TokenType::Comma,
          "Expected separating comma between parameters"
        );
        acceptN(TokenType::StatementBreak);
      }

      // Allow trailing comma
      if (check(closingBracket)) {
        break;
      }

      // TODO: consider how to clean up this code
      if ((requiredInputs.size() + optionalInputs.size()) > MAX_ARGUMENTS) {
        auto nodeIndex = optionalInputs.empty() ? requiredInputs.back()
                                                : optionalInputs.back();
        crash(
          nodeIndex,
          "All arguments following a named argument must be named"
        );
      }
      NodeIndex parameter = declaration();
      NodeType parameterType = nodeType(parameter);

      bool isOptional = parameterType == NodeType::Declaration;
      if (isOptional) {
        optionalInputs.push_back(parameter);
      } else if (!optionalInputs.empty()) {
        crash(
          parameter,
          "All arguments following a named argument must be named"
        );
      } else {
        requiredInputs.push_back(parameter);
      }
      hasMultiple = true;
      acceptN(TokenType::StatementBreak);
    }

    consume(closingBracket, "Expected ')' for declaration");

    auto dataIndex = addData(requiredInputs);
    addData(optionalInputs);
    u32 packedInputLength =
      packInt(requiredInputs.size(), optionalInputs.size(), 0, 0);
    return addNode(
      ASTNode{
        .left = dataIndex.value,
        .right = packedInputLength,
        .token = token,
        .nodeType = NodeType::ParameterList
      }
    );
  }

  NodeIndex primary() {
    if (auto token = match(TokenType::When)) {
      consume(
        TokenType::LeftParen,
        "Condition for switch statement must be preceeded by a '('"
      );
      std::vector<NodeIndex> cases;
      cases.push_back(expression());
      consume(
        TokenType::RightParen,
        "Condition for switch statement must be followed by a ')'"
      );
      consume(
        TokenType::LeftCurlyBrace,
        "Expected '{' before cases for 'switch'"
      );
      bool hasMultiple = false;
      while (
        !acceptUntil(TokenType::StatementBreak, TokenType::RightCurlyBrace)) {
        if (hasMultiple) {
          consumeN(
            TokenType::StatementBreak,
            "Expected newline between cases for 'switch'"
          );
        } else {
          hasMultiple = true;
          acceptN(TokenType::StatementBreak);
        }
        auto caseCondition = expression();
        consume(
          TokenType::ThinArrow,
          "Expected '=>' between case condition and body"
        );
        auto caseBody = assignment();
        cases.push_back(caseCondition);
        cases.push_back(caseBody);

        if (
          auto elseToken =
            acceptUntil(TokenType::StatementBreak, TokenType::Else)
        ) {
          // Use thin arrow to diambiguate against if/else from previous case
          consume(
            TokenType::ThinArrow,
            "Expected '->' between else case and body"
          );
          auto caseBlock = assignment();
          cases.push_back(NodeIndex::null());
          cases.push_back(caseBlock);
          acceptN(TokenType::StatementBreak);
          consume(
            TokenType::RightCurlyBrace,
            "Expected closing '}' after default 'else' case in switch statement"
          );
          break;
        }
      }
      return addNode(Encodings::Block{.elements = cases}, toIndex(token));
    }

    if (check(TokenType::While)) {
      return whileLoop();
    }

    if (check(TokenType::LeftCurlyBrace)) {
      return block();
    }

    if (auto token = match(TokenType::Using)) {
      return addNode(
        Encodings::UnaryOp{
          .operand = expression(),
          .operation = UnaryOps::Using
        },
        token
      );
    }

    if (peek().isBuiltin()) {
      auto builtin = advance();
      consume(
        TokenType::LeftParen,
        "Compiler builtins must be called like functions"
      );
      auto args = argumentList();
      return addNode(
        Encodings::UnaryOp{
          .operand = args,
          .operation = UnaryOps::CompilerBuiltin
        },
        builtin
      );
    }

    if (peek().isLiteral()) {
      auto node = Encodings::Literal{.token = advance()};
      return addNode(node);
    }

    if (auto token = match(TokenType::Dot)) {
      auto name = consume(
        TokenType::Identifier,
        "Prefix operator '.' must be followed by an identifier"
      );
      auto node =
        Encodings::DotAccessor{.object = NodeIndex::null(), .fieldName = name};
      return addNode(node, token);
    }

    if (auto token = match(TokenType::MultiLineString)) {
      u32 numTokens = 1;
      TokenIndex startToken = toIndex(token);
      while (check(TokenType::StatementBreak)) {
        if (check(TokenType::MultiLineString, {1})) {
          advance();
          advance();
          numTokens++;
        } else {
          break;
        }
      }
      for (int i = 0; i < numTokens; i++) {
        fmt::println(
          "String tokens: {}",
          tokenizer.lexeme(tokens[startToken.value + i * 2])
        );
      }
      return addNode(
        ASTNode{
          .left = toIndex(token).value,
          .right = numTokens,
          .token = toIndex(token),
          .nodeType = NodeType::MultiLineString
        }
      );
    }

    if (check(TokenType::Function) || check(TokenType::Kernel)) {
      return function();
    }

    if (check(TokenType::Generic)) {
      return generic();
    }

    if (auto token = match(TokenType::For)) {
      consume(
        TokenType::LeftParen,
        "'for' loop must provide capture/iterator information within "
        "parentheses"
      );
      auto iteration = declaration();
      {
        auto type = nodeType(iteration);
        if (type == NodeType::Definition) {
          crash(
            iteration,
            "Iterator required for 'for' loop (append '=' and an expression)"
          );
        } else if (type != NodeType::Declaration) {
          crash(iteration, "Expected a declaration in 'for' loop capture");
        }
      }
      consume(TokenType::RightParen, "Expected closing ')'");
      auto body = expression();
      return addNode(
        Encodings::BinaryOp{
          .left = iteration,
          .right = body,
          .operation = token,
        }
      );
    }

    if (check(TokenType::Struct)) {
      return structDefinition();
    }

    if (check(TokenType::Union)) {
      return unionDefinition();
    }

    if (check(TokenType::Enum)) {
      return enumLiteral();
    }

    if (auto token = match(TokenType::Return)) {
      if (peek().isClosingToken()) {
        return addNode(
          Encodings::UnaryOp{
            .operand = NodeIndex::null(),
            .operation = UnaryOps::Return
          },
          token
        );
      }
      return addNode(
        Encodings::UnaryOp{
          .operand = expression(),
          .operation = UnaryOps::Return
        },
        token
      );
    }

    if (check(TokenType::If)) {
      auto ifToken = advance();
      consume(
        TokenType::LeftParen,
        "Condition for 'if' statement needs to be "
        "surrounded by parentheses '('"
      );
      auto condition = expression();
      consume(
        TokenType::RightParen,
        "Condition for 'if' statement needs to be "
        "surrounded by parentheses ')'"
      );
      auto value = assignment();

      auto ifNode = Encodings::If{.condition = condition, .ifClause = value};
      OptionalNode elseNode = NodeIndex::null();

      // Disambiguate when/else
      auto lastToken = current;
      if (
        auto elseToken =
          acceptUntil(TokenType::StatementBreak, TokenType::Else);
        elseToken && !check(TokenType::FatArrow)
      ) {
        elseNode = assignment();
      } else {
        current = lastToken;
      }
      ifNode.elseClause = elseNode;
      return addNode(ifNode, ifToken);
    }

    if (auto token = match(TokenType::LeftParen)) {
      return argumentList(TokenType::RightParen, true);
    }

    if (auto token = match(TokenType::Import)) {
      auto fileNode = consume(
        TokenType::String,
        "import must be followed by a file path string"
      );
      log("Import file: {}", tokenizer.lexeme(fileNode));
      return addNode(
        Encodings::UnaryOp(
          {.operand = {toIndex(fileNode).value}, .operation = UnaryOps::Import}
        ),
        token
      );
    }

    if (auto token = match(TokenType::CudaImport)) {
      auto fileNode = consume(
        TokenType::String,
        "@cudaImport must be followed by a file path string"
      );
      return addNode(
        Encodings::UnaryOp({
          .operand = {toIndex(fileNode).value},
          .operation = UnaryOps::CudaImport,
        }),
        token
      );
    }

    if (auto op = builtinUnary.find(peek().type); op != builtinUnary.end()) {
      auto token = advance();
      return addNode(
        Encodings::UnaryOp{.operand = expression(), .operation = op->second},
        token
      );
    }

    if (auto token = match(TokenType::BUILTIN_Binclude)) {
      auto file = consume(
        TokenType::String,
        "@bInclude must be followed by a file path string"
      );
      return addNode(
        Encodings::UnaryOp{
          .operand = {toIndex(file).value},
          .operation = UnaryOps::BInclude
        },
        token
      );
    }

    if (auto token = match(TokenType::BUILTIN_CudaPtx)) {
      return addNode(
        Encodings::UnaryOp({
          .operand = expression(),
          .operation = UnaryOps::CudaPtx,
        }),
        token
      );
    }

    if (auto token = match(TokenType::MultiPointer)) {
      NodeIndex elementType = expression();
      return addNode(
        Encodings::UnaryOp{
          .operand = elementType,
          .operation = UnaryOps::MultiPointerTo
        },
        token
      );
    }

    if (auto startToken = match(TokenType::LeftSquareBracket)) {

      // Slice
      if (check(TokenType::RightSquareBracket)) {
        advance();
        NodeIndex elementType = expression();
        return addNode(
          Encodings::UnaryOp{
            .operand = elementType,
            .operation = UnaryOps::SliceType
          },
          startToken
        );
      } else {
        std::vector<NodeIndex> items;

        // Array literal
        while (!match(TokenType::RightSquareBracket)) {
          if (items.size() > 0) {
            consume(
              TokenType::Comma,
              "Expected separating comma between elements of array literal"
            );
          }
          acceptN(TokenType::StatementBreak);

          // Allow trailing comma
          if (match(TokenType::RightSquareBracket)) {
            break;
          }

          items.push_back(expression());

          // Sized array
          if (
            items.size() == 1 && check(TokenType::RightSquareBracket) &&
            !peek({1}).isClosingToken()
          ) {
            advance();
            NodeIndex elementType = expression();
            return addNode(
              Encodings::BinaryOp{
                .left = items[0],
                .right = elementType,
                .operation = startToken
              }
            );
          }
          acceptN(TokenType::StatementBreak);
        }

        if (peek().isClosingToken()) {
          items.push_back(NodeIndex::null());
        } else {
          items.push_back(expression());
        }

        auto node = Encodings::Block{.elements = ChildSpan(items)};
        return addNode(node, toIndex(startToken));
      }
    }

    if (auto token = match(TokenType::BUILTIN_Align)) {
      consume(
        TokenType::LeftParen,
        "Builtin '@align' must be called like a function with 1 argument"
      );
      auto alignmentValue = expression();
      consume(
        TokenType::RightParen,
        "Builtin '@align' must be called like a function with 1 argument"
      );
      auto type = expression();

      return addNode(
        Encodings::BinaryOp{
          .left = alignmentValue,
          .right = type,
          .operation = token
        }
      );
    }

    if (auto token = match(TokenType::BUILTIN_Name)) {
      return addNode(
        Encodings::UnaryOp{
          .operand = expression(),
          .operation = UnaryOps::BuiltinName
        },
        token
      );
    }

    crash(latestToken(), "Unable to parse; ending");
  }

  NodeIndex function() {
    static vector<TokenType> types{TokenType::Function, TokenType::Kernel};
    auto keyword = consume(types, "Expected 'fn'/'kernel' keyword");
    accept(TokenType::String);
    // Params
    consume(TokenType::LeftParen, "Expected '(' for parameter declaration");

    NodeIndex parameters = parameterList();

    OptionalNode returnType = NodeIndex::null();
    if (check(TokenType::ThinArrow)) {
      advance();
      returnType = expression();
    }

    OptionalNode body = NodeIndex::null();
    // Forward declaration
    if (check(TokenType::LeftCurlyBrace)) {
      body = block();
    }
    auto node = Encodings::FunctionLiteral{
      .parameters = parameters,
      .returnType = returnType,
      .body = body
    };
    return addNode(node, toIndex(keyword));
  }

  NodeIndex block() {
    consume(TokenType::LeftCurlyBrace, "Expected '{'");
    auto startToken = previous();
    std::vector<NodeIndex> statements;
    acceptN(TokenType::StatementBreak);
    while (!match(TokenType::RightCurlyBrace)) {
      statements.push_back(statement());
    }

    auto node = Encodings::Block{.elements = ChildSpan(statements)};
    return addNode(node, toIndex(startToken));
  }

  NodeIndex structDefinition() {
    consume(
      TokenType::Struct,
      "Expected 'struct' token at the beginning of struct definition"
    );
    auto token = previous();

    // TODO: distinct types
    std::vector<NodeIndex> definitions;
    OptionalNode impl;
    consume(
      TokenType::LeftCurlyBrace,
      "Struct field definitions must be declared between {}"
    );
    while (true) {
      acceptN(TokenType::StatementBreak);
      auto fieldMember = declaration();

      if (nodeType(fieldMember) == NodeType::Declaration) {
        TODO("Default field member values");
      }
      if (nodeType(fieldMember) != NodeType::Definition) {
        crash(fieldMember, "Field member for struct should be a definition");
      }
      definitions.push_back(fieldMember);

      acceptN(TokenType::StatementBreak);
      if (match(TokenType::RightCurlyBrace)) {
        break;
      } else if (match(TokenType::Impl)) {
        // TODO Next
        impl = block();
        acceptN(TokenType::StatementBreak);
        consume(
          TokenType::RightCurlyBrace,
          "Expected a closing '}' after impl block for struct"
        );
        break;
      }
    }

    return addNode(
      Encodings::Struct{.children = ChildSpan(definitions), .implBlock = impl},
      token
    );
  }

  NodeIndex unionDefinition() {
    consume(
      TokenType::Union,
      "Expected 'union' token at the beginning of union definition"
    );
    auto token = previous();

    std::vector<NodeIndex> definitions;
    consume(
      TokenType::LeftCurlyBrace,
      "Union field definitions must be declared between {}"
    );
    while (true) {
      acceptN(TokenType::StatementBreak);
      auto fieldMember = declaration();

      if (nodeType(fieldMember) == NodeType::Declaration) {
        TODO("Default union member values");
      }
      if (nodeType(fieldMember) != NodeType::Definition) {
        crash(fieldMember, "Field member for union should be a definition");
      }
      definitions.push_back(fieldMember);

      acceptN(TokenType::StatementBreak);
      if (match(TokenType::RightCurlyBrace)) {
        break;
      }
    }

    return addNode(
      Encodings::Struct{
        .children = ChildSpan(definitions),
        .implBlock = NodeIndex::null(),
      },
      token
    );
  }

  NodeIndex whileLoop() {
    Token token =
      consume(TokenType::While, "'while' loop requires 'while' keyword");
    NodeIndex condition = expression();
    NodeIndex loopBody = assignment();
    Encodings::BinaryOp loop =
      {.left = condition, .right = loopBody, .operation = token};
    return addNode(loop);
  }

  NodeIndex enumLiteral() {
    auto token =
      consume(TokenType::Enum, "enum token required for enum literal");
    OptionalNode rawType = NodeIndex::null();
    if (check(TokenType::LeftParen)) {
      advance();
      rawType = expression();
      consume(
        TokenType::RightParen,
        "Expected closing parenthesis after raw value for enum"
      );
    }
    std::vector<Encodings::NamedValue> enumEntries;
    consume(
      TokenType::LeftCurlyBrace,
      "enum literals require values to be specified between '{' and '}'"
    );
    acceptN(TokenType::StatementBreak);
    while (!match(TokenType::RightCurlyBrace)) {
      auto name =
        consume(TokenType::Identifier, "enum values must be valid identifiers");
      if (match(TokenType::Assign)) {
        enumEntries.push_back({toIndex(name), expression()});
      } else {
        enumEntries.push_back({toIndex(name), NodeIndex::null()});
      }
      if (match(TokenType::RightCurlyBrace)) break;
      consumeN(
        TokenType::StatementBreak,
        "enum entries must be separated by at least 1 newline"
      );
    }
    return addNode(
      Encodings::Enum{.rawType = rawType, .entries = std::span(enumEntries)},
      token
    );
  }

  NodeIndex generic() {
    auto token = consume(TokenType::Generic, "Expected 'gn' keyword");

    // Params
    consume(
      TokenType::LeftSquareBracket,
      "Expected '[' for parameter declaration"
    );

    auto parameters = parameterList(TokenType::RightSquareBracket);
    auto value = expression();
    return addNode(
      Encodings::BinaryOp{
        .left = parameters,
        .right = value,
        .operation = token
      }
    );
  }

  Tokenizer::TokenLocation locationOf(NodeIndex node) const {
    auto token = getToken(node);
    return tokenizer.locationOf(token);
  }

  template <typename... Args>
  [[noreturn]] void crash(
    NodeIndex node,
    fmt::format_string<Args...> fmt,
    Args&&... args
  ) const {
    crash(getToken(node), fmt, std::forward<Args>(args)...);
  }

  [[clang::noinline]] void underline(NodeIndex node) const {
    underline(getToken(node));
  }

  [[clang::noinline]] void underline(Token token) const {
    tokenizer.locationOf(token).underline(std::cerr);
  }

  [[clang::noinline]] void underline(Tokenizer::TokenLocation loc) const {
    loc.underline(std::cerr);
  }

  template <typename... Args>
  [[noreturn]] void crash(
    Token token,
    fmt::format_string<Args...> fmt,
    Args&&... args
  ) const {
    auto& out = std::cerr;
    auto location = tokenizer.locationOf(token);
    fmt::println(
      out,
      "Parser error in file {} at line {}:{}",
      tokenizer.inputFilePath.string(),
      location.line,
      location.column
    );
    underline(location);
    fmt::println(out, fmt, std::forward<Args>(args)...);
    dumpNodes();
    abort();
  }

  void dumpNodes() const {
    if (!(Logger::globalLevels & LogLevel::Parsing)) return;
    for (u32 i = 1; i <= nodes.size(); i++) {
      log("Node type: {}", (u32)nodeType({i}));
      locationOf({i}).underline(std::cout);
    }
  }
};
