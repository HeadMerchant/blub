#pragma once
#include <cassert>
#include <cstdlib>
#include <charconv>
#include "parser/parser.h"
#include "environment.h"
#include <span>
#include <sstream>
#include <stdexcept>
#include <string>
#include "interpreter/value.h"
#include "tokenizer.h"
#include "logging.h"

class Interpreter {
    public:
    ASTNode* main;
    Environment fileEnvironment;
    Parser& parser;
    std::span<NodeIndex> program;
    std::ostream& log;

    Interpreter(Parser& parser, std::span<NodeIndex> program): parser(parser), fileEnvironment(Environment::baseEnvironment), program(program), log(logger(LogLevel::DEBUG)) {}

    Reference* interpret(NodeIndex nodeIndex) {
        return interpret(nodeIndex, fileEnvironment);
    }
    
    Reference* interpret(NodeIndex nodeIndex, Environment& environment, int depth = 0) {
        auto encoded = parser.getNode(nodeIndex);
        // std::cout << "\n\nInterpreting node at token:";
        // node->token.print();
        std::string indent(depth, '\t');
        depth += 1;
        log << indent << "Nodetype: " << static_cast<int>(encoded.nodeType) << "\n"; 
        switch(encoded.nodeType) {
            case NodeType::BLOCK: {
                Environment blockEnv(environment);
                auto node = parser.getBlock(nodeIndex);
                for (auto child : node.statements) {
                    interpret(child, blockEnv, depth);
                }
                return nullptr;
            }
            case NodeType::DECLARATION: {
                auto node = parser.getDeclaration(nodeIndex);
                // TODO: support using non-identifiers?
                auto value = interpret(node.value, environment);

                // TODO: unused rn
                bool compileTime = parser.getToken(encoded.token).type == TokenType::COLON;

                auto reference = interpret(node.identifier);
                log << parser.getToken(encoded.token).line << "\n";
                reference->assign(value);

                // TODO: support assignment as expression???
                return nullptr;
            }
            case NodeType::DEFINITION: {
                auto node = parser.getDefinition(nodeIndex);
                // TODO(types): type inference
                if (node.inferType) {
                    Reference* ref = new Reference(ValueType::INFER);
                    environment.define(node.name->lexeme, ref);
                    return ref;
                } else {
                    Reference* type = interpret(node.type, environment);
                    if (!type->isType()) {
                        std::stringstream message;
                        message << "Type for identifier " << node.name->lexeme << " is not a type";
                        throw std::invalid_argument(message.str());
                    }
                    throw std::invalid_argument("TODO: Add explicit typing");
                }
            }
            case NodeType::ASSIGNMENT: {
                auto node = parser.getAssignment(nodeIndex);
                auto assigneeEncoded = parser.getNode(node.assignee);
                auto value = interpret(node.value, environment, depth+1);

                auto assignee = interpret(node.assignee);
                assignee->assign(value);
                // TODO: consider value
                return nullptr;
                // switch (assigneeEncoded.nodeType) {
                //     case NodeType::IDENTIFIER: {
                //         auto assignee = parser.getIdentifier(node.assignee);
                //         environment.assign(assignee.token->lexeme, value);
                //         return value;
                //     }
                //     default: {
                //         std::stringstream ss;
                //         ss << "Unable to assign to node of type: " << (i32) assigneeEncoded.nodeType;
                //         throw std::invalid_argument(ss.str());
                //     }
                // }
            }
            case NodeType::FUNCTION_CALL: {
                auto node = parser.getFunctionCall(nodeIndex);
                auto function = interpret(node.functionValue, environment, depth);
                std::vector<Reference*> arguments;
                for (auto arg : node.arguments) {
                    arguments.push_back(interpret(arg, environment, depth));
                    // std::cout << "arg: " << *arguments.back()->toString()->string() << "\n";
                }
                if (function->type == ValueType::NATIVE_FUNCTION) {
                    auto nativeFunction = *function->nativeFunction();
                    return nativeFunction(std::span<Reference *>(arguments));
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
                    return new Reference(new std::string(token.lexeme));
                }
                if (token.type == TokenType::DECIMAL) {
                    float result;
                    std::from_chars(token.lexeme.data(), token.lexeme.data() + token.lexeme.size(), result);
                    return new Reference(result); 
                }
                if (token.type == TokenType::INT) {
                    int result;
                    std::from_chars(token.lexeme.data(), token.lexeme.data() + token.lexeme.size(), result);
                    return new Reference(result);
                }
                throw std::invalid_argument("Unable to create literal from value");
            }
            case NodeType::ARRAY_LITERAL: {
                auto node = parser.getArrayLiteral(nodeIndex);
                ArrayType values = new std::vector<Reference*>();
                for (auto element : node.elements) {
                    values->push_back(interpret(element, environment, depth));
                }
                return new Reference(values);
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
                auto function = new FunctionLiteral(FunctionLiteral {.parser = parser, .function = node});
                auto value = new Reference(function);
                log << "Literal address: " << function << "\nValue address: " << value << "\nActual value address: " << value->function() << "\n";
                return value;
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

        log << "Type of main: " << (i32) main->type << "\n";
        auto mainFunction = main->function();
        log << "Address of main: " << mainFunction << "\n";
        // auto parser = mainFunction.parser;
        // TODO: add way to interpret this
        auto mainBlock = mainFunction->parser.getBlock(mainFunction->function.body);
        // Encodings::FunctionCall mainCall {.functionValue = , .arguments = std::span<NodeIndex>()};
        for (auto statement : mainBlock.statements) {
            interpret(statement, fileEnvironment, 1);
        }
    }
};
