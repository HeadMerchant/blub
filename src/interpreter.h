#pragma once
#include <cassert>
#include <cstdlib>
#include <charconv>
#include "parser.h"
#include "environment.h"
#include <iostream>
#include <sstream>
#include <span>
#include <stdexcept>
#include <string>
#include <vector>
#include "interpreter/value.h"
#include "tokenizer.h"

class Interpreter {
    public:
    std::vector<ASTNode*> program;
    ASTNode* main;
    Environment* environment;

    Interpreter(std::vector<ASTNode*> program): program(program), environment(new Environment()) {}

    InterpreterValue* interpret(ASTNode* node) {
        // std::cout << "\n\nInterpreting node at token:";
        // node->token.print();
        // std::cout << "Nodetype: " << static_cast<int>(node->type) << "\n"; 
        // std::cout << "\n\n";
        switch(node->type) {
            case NodeType::BLOCK: {
                // std::cout << node->children.size() << "\n";
                for (auto child : node->children) {
                    interpret(node);
                }
                return nullptr;
            }
            case NodeType::DECLARATION: {
                auto name = node->children[0]->token.lexeme;
                auto value = node->children[1];
                environment->defs[name] = value;
                return nullptr;
            }
            case NodeType::FUNCTION_CALL: {
                auto function = node->children[0];
                std::span<ASTNode*> args(node->children);
                args = args.subspan(1, args.size()-1);
                if (function->token.lexeme == "print") {
                    if (args.size() != 1) {
                        std::stringstream ss;
                        ss << "Needed 1 arg; found: " << args.size();
                        throw std::invalid_argument(ss.str());
                    }
                    StringType text = *interpret(args[0])->toString()->string();
                    std::cout << text << "\n";
                    return nullptr;
                }
                return nullptr;
            }
            case NodeType::LITERAL: {
                auto token = node->token;
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
                ArrayType* values = new ArrayType();
                for (auto element : node->children) {
                    values->push_back(interpret(element));
                }
                return new InterpreterValue(values);
            }
            case NodeType::BINARY: {
                auto opType = node->token.type;
                auto leftVal = interpret(node->children[0]);
                auto rightVal = interpret(node->children[1]);
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
            case NodeType::UNARY:
            case NodeType::IDENTIFIER:
            case NodeType::FUNCTION_LITERAL:
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
        auto main = environment->find("main");
        assert(main->type == NodeType::FUNCTION_LITERAL);
        if (main == nullptr) {
           throw std::invalid_argument("Unable to find definition for function 'main'");
        }

        for (auto statement : main->children[1]->children) {
            interpret(statement);
        }
    }
};