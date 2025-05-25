#pragma once
#include <cassert>
#include "parser.h"
#include "environment.h"
#include <iostream>
#include <sstream>
#include <span>

class Interpreter {
    public:
    std::vector<ASTNode*> program;
    ASTNode* main;
    Environment* environment;

    Interpreter(std::vector<ASTNode*> program): program(program), environment(new Environment()) {}

    void interpret(ASTNode* node) {
        std::cout << "\n\nInterpreting node at token:";
        node->token.print();
        std::cout << "Nodetype: " << static_cast<int>(node->type) << "\n"; 
        std::cout << "\n\n";
        switch(node->type) {
            case NodeType::BLOCK: {
                std::cout << node->children.size() << "\n";
                for (auto child : node->children) {
                    interpret(node);
                }
                break;
            }
            case NodeType::DECLARATION: {
                auto name = node->children[0]->token.lexeme;
                auto value = node->children[1];
                std::cout << "Bruh we in here\n";
                environment->defs[name] = value;
                break;
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
                    std::cout << args[0]->token.lexeme << "\n";
                }
                break;
            }
        }
    }

    void run() {
        for (auto node : program) {
            interpret(node);
        }
        std::cout << "\n\ndone interpretting\n\n";
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