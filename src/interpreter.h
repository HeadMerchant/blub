#pragma once
#include <algorithm>
#include <cassert>
#include <charconv>
#include "parser.h"
#include "environment.h"
#include <span>
#include <sstream>
#include <stdexcept>
#include <string>
#include "interpreter/value.h"
#include "tokenizer.h"
#include "logging.h"
#include "types.h"

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
                if (compileTime) {
                    // Name type
                    if (value->isType() && parser.getNode(node.identifier).nodeType == NodeType::IDENTIFIER) {
                        Types::Pool.setTypeName(
                            std::get<Types::TypeIndex>(value->value),
                            std::move(
                              std::string(parser.getIdentifier(node.identifier).token->lexeme)
                            )
                        );
                    }
                }

                auto reference = interpret(node.identifier);
                reference->assign(value);
                reference->isMutable = !compileTime;

                // TODO: support assignment as expression???
                return nullptr;
            }
            case NodeType::DEFINITION: {
                auto node = parser.getDefinition(nodeIndex);
                Reference* ref;
                if (node.inferType) {
                    ref = new Reference(Types::Intrinsic::INFER);
                } else {
                    Reference* type = interpret(node.type, environment);
                    if (!type->isType()) {
                        std::stringstream message;
                        message << "Type for identifier " << node.name->lexeme << " is not a type";
                        throw std::invalid_argument(message.str());
                    }
                    Types::TypeIndex typeIndex = std::get<Types::TypeIndex>(type->value);
                    ref = Reference::ofType(type);

                    // Initialize struct
                    if (Types::Pool.isStruct(typeIndex)) {
                        Types::Struct& structDefinition = Types::Pool.getStruct(typeIndex);
                        std::vector<Reference*> structValue(structDefinition.numFields());
                        log << "Number of fields in struct" << Types::Pool.typeName(typeIndex) << ": " << structDefinition.fields.symbolValues.size() << "\n";
                        // TODO: NEXT
                        for (auto [fieldName, index] : structDefinition.fields.symbolIndex) {
                            log << "Field: " << fieldName << ", index: " << index << "\n";
                            structValue[index] = new Reference(*structDefinition.fields.symbolValues[fieldName], true);
                        }
                        ref->value = structValue;
                        // TODO: initialize field values; not just containers

                    }
                }
                environment.define(node.name->lexeme, ref);
                return ref;
            }
            case NodeType::ASSIGNMENT: {
                auto node = parser.getAssignment(nodeIndex);
                auto assigneeEncoded = parser.getNode(node.assignee);
                auto value = interpret(node.value, environment, depth+1);

                auto assignee = interpret(node.assignee);
                assignee->assign(value);
              
                // TODO: consider value
                return nullptr;
            }
            case NodeType::FUNCTION_CALL: {
                auto node = parser.getFunctionCall(nodeIndex);
                auto function = interpret(node.functionValue, environment, depth);
                std::vector<Reference*> arguments;
                for (auto arg : node.arguments) {
                    arguments.push_back(interpret(arg, environment, depth));
                }
                if (function->type == Types::indexOf(Types::Intrinsic::NATIVE_FUNCTION)) {
                    auto nativeFunction = *std::get<NativeFunction>(function->value);
                    return nativeFunction(std::span<Reference *>(arguments));
                }
                if (function->type == Types::indexOf(Types::Intrinsic::FUNCTION)) {
                    return callUserFunction(&std::get<FunctionType>(function->value), std::span<Reference *>(arguments));
                }
                throw std::invalid_argument("Unknown function type");
            }
            case NodeType::LITERAL: {
                auto node = parser.getLiteral(nodeIndex);
                auto token = *node.token;
                if (token.type == TokenType::STRING) {
                    return new Reference(std::move(std::string(token.lexeme)));
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
                ArrayType values = std::vector<Reference*>();
                for (auto element : node.elements) {
                    values.push_back(interpret(element, environment, depth));
                }
                return new Reference(std::move(values));
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
                auto function = FunctionLiteral(FunctionLiteral {.parser = &parser, .declarationEnvironment = &environment, .function = node});
                auto value = new Reference(std::move(function));
                return value;
            }
            case NodeType::POINTER_OP: {
                auto node = parser.getPointerOp(nodeIndex);
                auto value = interpret(node.operand);
                switch(node.opType) {
                    case Encodings::PointerOpType::DEREFERENCE: {
                        // TODO: better errors
                        assert(Types::Pool.isPointer(value->type));
                        return std::get<Reference*>(value->value);
                    }
                    case Encodings::PointerOpType::REFERENCE: {
                        if (value->isType()) {
                            return Reference::pointerTo(std::get<Types::TypeIndex>(value->value));
                        } else {
                            return Reference::pointerTo(value);
                        }
                    }
                }
            }
            case NodeType::UNARY:
            case NodeType::PARAMETER_LIST:
            case NodeType::CALL:
            case NodeType::ARGS_LIST: {
              break;
            }
            case NodeType::IF: {
                auto node = parser.getIf(nodeIndex);
                auto condition = interpret(node.condition);
                if (condition->type != Types::indexOf(Types::Intrinsic::BOOL)) {
                    throw std::invalid_argument("Condition of an 'if' statement needs to be of type 'bool', but was type '"+Types::Pool.typeName(condition->type)+"'");
                }
                if (std::get<bool>(condition->value)) {
                    return interpret(node.value);
                } else if (node.hasElse) {
                    return interpret(node.elseValue);
                }

                return nullptr;
            }
            case NodeType::BOOLEAN_LITERAL:
                return Reference::of(parser.getBooleanLiteral(nodeIndex));
            case NodeType::STRUCT: {
                auto node = parser.getStruct(nodeIndex);
                auto structDefinition = Types::Struct();
                auto definitionEnv = new Environment(&environment);
                for (auto fieldIndex : node.children) {
                    interpret(fieldIndex, *definitionEnv);
                }

                std::vector<Identifier> fields;
                std::transform(definitionEnv->defs.symbolValues.begin(), definitionEnv->defs.symbolValues.end(), std::back_inserter(fields), [](std::pair<Identifier, Reference*> a){return a.first;});
                std::sort(fields.begin(), fields.end(), [definitionEnv](Identifier a, Identifier b) { return definitionEnv->defs.indexOf(a) < definitionEnv->defs.indexOf(b);});
                for (auto field : fields) {
                    auto value = definitionEnv->defs.symbolValues[field];
                    // Copy fields into either fields or statics
                    if (value->isMutable) {
                        structDefinition.fields.define(field, value);
                    } else {
                        structDefinition.statics.define(field, value);
                    }
                }
                free(definitionEnv);
                Types::TypeIndex structTypeIndex = Types::Pool.addStruct(structDefinition, "");
                return Reference::toType(structTypeIndex);
            }
            case NodeType::DOT_ACCESS: {
                auto node = parser.getDotAccess(nodeIndex);
                Reference* object = interpret(node.object);
                return object->getField(node.fieldName->lexeme);
            }
            }
        throw std::invalid_argument("Bruh im crashing tf out");
    }

    Reference* callUserFunction(FunctionLiteral* definition, std::span<Reference*> arguments) {
        // TODO: consider heap allocating for closures
        Environment callEnv = Environment(definition->declarationEnvironment);

        if (arguments.size() > definition->function.parameters.size()) {
            throw std::invalid_argument("Too many arguments to function call");
        }

        if (arguments.size() < definition->function.parameters.size()) {
            // TODO: default arguments
            throw std::invalid_argument("Too few arguments to function call");
        }

        for (i32 i = 0; i < arguments.size(); i++) {
            auto param = definition->function.parameters[i];
            auto arg = arguments[i];
            // TODO: use parser from function definition
            assert(definition->parser->getNode(param).nodeType == NodeType::DEFINITION);
            interpret(param, callEnv)->assign(arg);
        }

        auto returnVal = interpret(definition->function.body, callEnv);
        return returnVal;
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

        log << "Type of main: " << main->type.value << "\n";
        auto mainFunction = std::get<FunctionType>(main->value);
        log << "Address of main: " << &mainFunction << "\n";
        callUserFunction(&mainFunction, std::span<Reference*>());
        // auto parser = mainFunction.parser;
        // TODO: add way to interpret this
        // auto mainBlock = mainFunction->parser.getBlock(mainFunction->function.body);
        // // Encodings::FunctionCall mainCall {.functionValue = , .arguments = std::span<NodeIndex>()};
        // for (auto statement : mainBlock.statements) {
        //     interpret(statement, fileEnvironment, 1);
        // }
    }
};
