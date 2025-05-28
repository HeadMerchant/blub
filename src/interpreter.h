#pragma once
#include <cassert>
#include <cstdlib>
#include <charconv>
#include "parser/parser.h"
#include "environment.h"
#include <span>
#include <stdexcept>
#include <string>
#include "interpreter/value.h"
#include "tokenizer.h"


class Interpreter {
    public:
    ASTNode* main;
    Environment fileEnvironment;
    Parser& parser;
    std::span<NodeIndex> program;

    Interpreter(Parser& parser, std::span<NodeIndex> program): parser(parser), fileEnvironment({.parent = &Environment::baseEnvironment}), program(program) {}

    InterpreterValue* interpret(NodeIndex nodeIndex) {
        return interpret(nodeIndex, fileEnvironment);
    }
    
    InterpreterValue* interpret(NodeIndex nodeIndex, Environment& environment, int depth = 0) {
        auto encoded = parser.getNode(nodeIndex);
        // std::cout << "\n\nInterpreting node at token:";
        // node->token.print();
        std::string indent(depth, '\t');
        depth += 1;
        std::cout << indent << "Nodetype: " << static_cast<int>(encoded.nodeType) << "\n"; 
        switch(encoded.nodeType) {
            case NodeType::BLOCK: {
                Environment blockEnv{.parent = &environment};
                auto node = parser.getBlock(nodeIndex);
                for (auto child : node.statements) {
                    interpret(child, blockEnv, depth);
                }
                return nullptr;
            }
            case NodeType::DECLARATION: {
                auto node = parser.getDeclaration(nodeIndex);
                // TODO: support using non-identifiers?
                auto name = parser.getIdentifier(node.identifier);
                auto value = interpret(node.value, environment, depth);
                environment.defs[name.token->lexeme] = value;
                return nullptr;
            }
            case NodeType::FUNCTION_CALL: {
                auto node = parser.getFunctionCall(nodeIndex);
                auto function = interpret(node.functionValue, environment, depth);
                std::vector<InterpreterValue*> arguments;
                for (auto arg : node.arguments) {
                    arguments.push_back(interpret(arg, environment, depth));
                    std::cout << "arg: " << *arguments.back()->toString()->string() << "\n";
                }
                if (function->type == ValueType::NATIVE_FUNCTION) {
                    auto nativeFunction = *function->nativeFunction();
                    return nativeFunction(std::span<InterpreterValue *>(arguments));
                }
                if (function->type == ValueType::FUNCTION) {
                    throw std::invalid_argument("TODO: implement user-defined functions");
                }
                return nullptr;
            }
            case NodeType::LITERAL: {
                auto node = parser.getLiteral(nodeIndex);
                auto token = *node.token;
                if (token.type == TokenType::STRING) {
                    return new InterpreterValue(new std::string(token.lexeme));
                }
                if (token.type == TokenType::DECIMAL) {
                    float result;
                    std::from_chars(token.lexeme.data(), token.lexeme.data() + token.lexeme.size(), result);
                    return new InterpreterValue(result); 
                }
                if (token.type == TokenType::INT) {
                    int result;
                    std::from_chars(token.lexeme.data(), token.lexeme.data() + token.lexeme.size(), result);
                    return new InterpreterValue(result);
                }
                throw std::invalid_argument("Unable to create literal from value");
            }
            case NodeType::ARRAY_LITERAL: {
                auto node = parser.getArrayLiteral(nodeIndex);
                ArrayType* values = new ArrayType();
                for (auto element : node.elements) {
                    values->push_back(interpret(element, environment, depth));
                }
                return new InterpreterValue(values);
            }
            case NodeType::BINARY_OP: {
                auto node = parser.getBinaryOp(nodeIndex);
                auto opType = node.token->type;
                auto leftVal = interpret(node.left, environment, depth);
                auto rightVal = interpret(node.right, environment, depth);
                switch (opType) {
                    case TokenType::PLUS:
                        return leftVal->add(rightVal);
                    case TokenType::MINUS:
                        return leftVal->sub(rightVal);
                    case TokenType::DIV:
                        return leftVal->div(rightVal);
                    case TokenType::MULT:
                        return leftVal->mul(rightVal);
                    default:
                        throw std::invalid_argument("Unknown binary operation");
                }
            }
            case NodeType::IDENTIFIER: {
                auto node = parser.getIdentifier(nodeIndex);
                return environment.find(node.token->lexeme);
            }
            case NodeType::FUNCTION_LITERAL: {
                auto node = parser.getFunctionLiteral(nodeIndex);
                auto function = FunctionLiteral {.parser = parser, .function = node};
                return new InterpreterValue(new FunctionLiteral(function));
            }
            case NodeType::UNARY:
            case NodeType::PARAMETER_LIST:
            case NodeType::CALL:
            case NodeType::ARGS_LIST: {
              break;
            }
        }
        throw std::invalid_argument("Bruh im crashing tf out");
    }

    void run() {
        for (auto node : program) {
            interpret(node);
        }
        // std::cout << "\n\ndone interpretting\n\n";
        auto main = fileEnvironment.find("main");
        if (main == nullptr) {
           throw std::invalid_argument("Unable to find definition for function 'main'");
        }

        auto mainFunction = main->function();
        // TODO: add way to interpret this
        auto mainBlock = mainFunction->parser.getBlock(mainFunction->function.body);
        // Encodings::FunctionCall mainCall {.functionValue = , .arguments = std::span<NodeIndex>()};
        for (auto statement : mainBlock.statements) {
            interpret(statement, fileEnvironment, 1);
        }
    }
};
