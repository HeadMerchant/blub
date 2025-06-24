#pragma once
#include <algorithm>
#include <cassert>
#include "parser.h"
#include "environment.h"
#include <ostream>
#include <queue>
#include <span>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include "interpreter/value.h"
#include "tokenizer.h"
#include "logging.h"
#include "types.h"
#include "common.h"

class LLVMCompiler {
    public:
    ASTNode* main;
    Environment fileEnvironment;
    Parser& parser;
    std::span<NodeIndex> program;
    std::ostream& log;
    std::queue<std::stringstream> globalsStack;

    LLVMCompiler(Parser& parser, std::span<NodeIndex> program): parser(parser), fileEnvironment(), program(program), log(logger(LogLevel::DEBUG)) {}
    

    std::string toLiteral(Reference* value, std::ostream& outputFile, Environment& environment) {
        if (value->storageType == StorageType::VARIABLE) {
            std::string valueName = std::move(environment.addTemporary());
            outputFile
                << valueName << " = load " << Types::Pool().getLLVMType(value->type) << ", ptr " << value->llvmName() << "\n";
            return std::move(valueName);
        }
        return std::move(std::string(value->llvmName()));
    }

    Reference* interpret(NodeIndex nodeIndex, Environment& environment, std::ostream& outputFile) {
        auto encoded = parser.getNode(nodeIndex);
        auto prefix = environment.prefix;
        switch(encoded.nodeType) {
            case NodeType::BLOCK: {
                TODO("Block declarations");
                // Environment blockEnv(environment);
                // auto node = parser.getBlock(nodeIndex);
                // for (auto child : node.statements) {
                //     interpret(child, blockEnv, depth);
                // }
                // return nullptr;
            }
            case NodeType::DECLARATION: {
                auto node = parser.getDeclaration(nodeIndex);
                // TODO: support using non-identifiers?
                auto value = interpret(node.value, environment, outputFile);

                bool compileTime = parser.getToken(encoded.token).type == TokenType::COLON;
                auto definitionName = parser.getDefinition(node.definition).name->lexeme;
                if (compileTime) {
                    std::stringstream instruction;
                    // Name type
                    if (value->isType() && parser.getNode(node.definition).nodeType == NodeType::IDENTIFIER) {
                        bool isDefined = Types::Pool().setTypeName(
                            std::get<Types::TypeIndex>(value->value),
                            std::move(
                              std::string(parser.getIdentifier(node.definition).token->lexeme)
                            )
                        );
                        if (!isDefined) {
                            TODO("Anonymous structs");
                        }
                    } else if (value->isFunction()) {
                        std::cout << "Type of identifier: " << static_cast<i32>(parser.getNode(node.definition).nodeType) << std::endl;
                        std::stringstream nameStream;
                        nameStream << "@" << environment.prefix << definitionName;
                        std::string name = nameStream.str();
                        // TODO parameters
                        instruction << "define void " << name << " (){\n";
                        // TODO function body
                        auto functionLiteral = std::get<FunctionType>(value->value);
                        auto body = parser.getBlock(functionLiteral.function.body);

                        std::stringstream envPrefixBuilder;
                        // envPrefixBuilder << environment.prefix << definitionName << ".";
                        Environment functionEnv(environment);
                        for (auto statement : body.statements) {
                            interpret(statement, functionEnv, instruction);
                        }

                        instruction << "ret void\n}\n";
                    }
                    
                    globalsStack.push(std::move(instruction));
                }

                auto reference = interpret(node.definition, environment, outputFile);
                reference->assign(value);
                reference->isMutable = !compileTime;
                if (!compileTime) {
                    outputFile
                        << reference->llvmName() << " = alloca " << Types::Pool().getLLVMType(reference->type) << "\n"
                        << "store " << Types::Pool().getLLVMType(value->type) << ' ' << toLiteral(value, outputFile, environment) << ", ptr " << reference->llvmName() << '\n';
                }

                // TODO: support assignment as expression???
                return Reference::Void();
            }
            case NodeType::DEFINITION: {
                auto node = parser.getDefinition(nodeIndex);
                Reference* ref;
                std::stringstream llvmName;
                llvmName << "%" << node.name->lexeme;
                if (!node.type.has_value()) {
                    ref = Reference::variable(Types::indexOf(Types::Intrinsic::INFER), std::move(llvmName.str()));
                } else {
                    Reference* type = interpret(node.type.value(), environment, outputFile);
                    if (!type->isType()) {
                        std::stringstream message;
                        message << "Type for identifier " << node.name->lexeme << " is not a type";
                        throw std::invalid_argument(message.str());
                    }
                    Types::TypeIndex typeIndex = std::get<Types::TypeIndex>(type->value);
                    ref = Reference::variable(typeIndex, std::move(llvmName.str()));

                    // Initialize struct
                    if (Types::Pool().isStruct(typeIndex)) {
                        TODO("Initialize struct");
                        // Types::Struct& structDefinition = Types::Pool.getStruct(typeIndex);
                        // std::vector<Reference*> structValue(structDefinition.numFields());
                        // log << "Number of fields in struct" << Types::Pool.typeName(typeIndex) << ": " << structDefinition.fields.symbolValues.size() << "\n";
                        // // TODO: NEXT
                        // for (auto [fieldName, index] : structDefinition.fields.symbolIndex) {
                        //     log << "Field: " << fieldName << ", index: " << index << "\n";
                        //     structValue[index] = new Reference(*structDefinition.fields.symbolValues[fieldName], true);
                        // }
                        // ref->value = structValue;
                        // TODO: initialize field values; not just containers
                    }
                }
                environment.define(node.name->lexeme, ref);
                return ref;
            }
            case NodeType::ASSIGNMENT: {
                auto node = parser.getAssignment(nodeIndex);
                auto assigneeEncoded = parser.getNode(node.assignee);
                auto value = interpret(node.value, environment, outputFile);

                auto assignee = interpret(node.assignee, environment, outputFile);
                if (assignee->storageType != StorageType::VARIABLE) {
                    throw std::invalid_argument("Can't assign to literal");
                }
                assignee->assign(value);
                std::string_view type = Types::Pool().getLLVMType(value->type);
                std::string valueName = toLiteral(value, outputFile, environment);
                outputFile << "store " << type << " " << valueName << ", ptr " << assignee->llvmName() << "\n";
              
                // TODO: consider value
                return Reference::Void();
            }
            case NodeType::FUNCTION_CALL: {
                auto node = parser.getFunctionCall(nodeIndex);
                auto function = interpret(node.functionValue, environment, outputFile);
                
                std::vector<Reference*> arguments;
                for (auto arg : node.arguments) {
                    arguments.push_back(interpret(arg, environment, outputFile));
                }
                if (function->type == Types::indexOf(Types::Intrinsic::FUNCTION)) {
                    TODO("Calling for user functions");
                }
                if (function->type == Types::indexOf(Types::Intrinsic::LLVM_FUNCTION)) {
                    // TODO: return value
                    auto nativeCall = std::get<LLVMFunction>(function->value);
                    outputFile << "call " << nativeCall.usage << "(";
                    bool hasMultipleArgs = false;
                    for (Reference* arg : arguments) {
                        // TODO: different arg types
                        if (hasMultipleArgs) {
                            outputFile << ", ";
                        }
                        outputFile << Types::Pool().getLLVMType(arg->type) << " " << std::get<LLVMName>(arg->value);
                    }
                    outputFile << ")\n";

                    if (!function->isInitialized) {
                        function->isInitialized = true;
                        std::stringstream ss;
                        ss << nativeCall.definition << "\n";
                        globalsStack.push(std::move(ss));
                    }
                    
                    return Reference::Void();
                }
                std::cout << "Unknown function type: " << Types::Pool().typeName(function->type) << "\n";
                throw std::invalid_argument("Unknown function type");
            }
            case NodeType::LITERAL: {
                auto node = parser.getLiteral(nodeIndex);
                auto token = *node.token;
                if (token.type == TokenType::STRING) {
                    TODO("Length-based strings");
                    auto name = environment.addConstant();
                    auto stringValue = token.lexeme;
                    static std::string nullByte = "\\00";
                    // TODO: use string types instead of C strings
                    std::stringstream instruction;
                    instruction << name << " = global [" << stringValue.length()+1 << " x i8] c\"" << stringValue << nullByte << "\", align 1\n";
                    globalsStack.push(std::move(instruction));
                    Reference* ref = Reference::literal(Types::indexOf(Types::Intrinsic::STRING), std::move(name));
                    return ref;
                }
                if (token.type == TokenType::NULL_TERMINATED_STRING) {
                    auto name = environment.addConstant();
                    auto stringValue = token.lexeme;
                    static std::string nullByte = "\\00";
                    std::stringstream instruction;
                    instruction << name << " = global [" << stringValue.length()+1 << " x i8] c\"" << stringValue << nullByte << "\", align 1\n";
                    globalsStack.push(std::move(instruction));
                    Reference* ref = Reference::literal(Types::Pool().pointerTo(Types::Pool().u8), std::move(name));
                    return ref;
                }
                if (token.type == TokenType::DECIMAL) {
                    std::string name(token.lexeme);
                    return Reference::literal(Types::indexOf(Types::Intrinsic::UNTYPED_FLOAT), std::move(name));
                }
                if (token.type == TokenType::INT) {
                    std::string name(token.lexeme);
                    return Reference::literal(Types::indexOf(Types::Intrinsic::UNTYPED_INT), std::move(name));
                }
                throw std::invalid_argument("Unable to create literal from value");
            }
            case NodeType::ARRAY_LITERAL: {
                TODO("Array literals");
                // auto node = parser.getArrayLiteral(nodeIndex);
                // ArrayType values = std::vector<Reference*>();
                // for (auto element : node.elements) {
                //     values.push_back(interpret(element, environment, outputFile));
                // }
                // return new Reference(std::move(values));
            }
            case NodeType::BINARY_OP: {
                auto node = parser.getBinaryOp(nodeIndex);
                auto opType = node.token->type;
                auto leftVal = interpret(node.left, environment, outputFile);
                auto rightVal = interpret(node.right, environment, outputFile);
                TODO("Binary ops");
                // switch (opType) {
                //     case TokenType::PLUS:
                //         return leftVal->add(rightVal);
                //     case TokenType::MINUS:
                //         return leftVal->sub(rightVal);
                //     case TokenType::DIV:
                //         return leftVal->div(rightVal);
                //     case TokenType::MULT:
                //         return leftVal->mul(rightVal);
                //     default:
                //         throw std::invalid_argument("Unknown binary operation");
                // }
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
                auto value = interpret(node.operand, environment, outputFile);
                TODO("Dereferencing");
                // switch(node.opType) {
                //     case Encodings::PointerOpType::DEREFERENCE: {
                //         // TODO: better errors
                //         assert(Types::Pool.isPointer(value->type));
                //         return std::get<Reference*>(value->value);
                //     }
                //     case Encodings::PointerOpType::REFERENCE: {
                //         if (value->isType()) {
                //             return Reference::pointerTo(std::get<Types::TypeIndex>(value->value));
                //         } else {
                //             return Reference::pointerTo(value);
                //         }
                //     }
                // }
            }
            case NodeType::UNARY:
            case NodeType::PARAMETER_LIST:
            case NodeType::CALL:
            case NodeType::ARGS_LIST: {
              break;
            }
            case NodeType::IF: {
                auto node = parser.getIf(nodeIndex);
                // TODO: add br instruction
                auto condition = interpret(node.condition, environment, outputFile);
                if (condition->type != Types::Pool().boolean) {
                    std::stringstream ss;
                    ss << "Condition of an 'if' statement needs to be of type 'bool', but was type '" << Types::Pool().typeName(condition->type) << "' " << condition->llvmName();
                    throw std::invalid_argument(ss.str());
                }

                auto blockIndex = environment.getNextTemporary();
                auto resultVariable = environment.addTemporary(blockIndex);
                auto ifLabel = environment.addLabel("if", blockIndex);
                auto endLabel = environment.addLabel("endif", blockIndex);
                auto elseLabel = environment.addLabel("else", blockIndex);

                // If
                std::stringstream ifInstruction;
                ifInstruction << ifLabel << ":\n";
                auto ifResult = interpret(node.value, environment, ifInstruction);
                std::optional<Types::TypeIndex> resultType = ifResult->type;

                // Else
                Reference* elseResult;
                std::stringstream elseInstruction;
                if (node.elseValue.has_value()) {
                    elseInstruction << elseLabel << ":\n";
                    elseResult = interpret(node.elseValue.value(), environment, elseInstruction);
                    resultType = Types::Pool().isAssignable(resultType.value(), elseResult->type);
                }

                // Results
                auto voidType = Types::indexOf(Types::Intrinsic::VOID);
                auto hasReturnType = (resultType.value_or(voidType) != voidType) && node.elseValue.has_value();
                if (hasReturnType) {
                    outputFile << resultVariable << " = alloca " << Types::Pool().getLLVMType(resultType.value()) << "\n";
                }
                std::string conditionName = toLiteral(condition, outputFile, environment);

                if (node.elseValue.has_value()) {
                    outputFile << "br i1 " << conditionName << ", label %" << ifLabel << ", label %" << elseLabel << "\n";
                    outputFile << ifInstruction.str();
                    outputFile << "br label %" << endLabel << "\n";
                    outputFile << elseInstruction.str();
                    outputFile << endLabel << ":\n";
                } else {
                    outputFile << "br i1 " << conditionName << ", label %" << ifLabel << ", label %" << endLabel << "\n";
                    outputFile << ifInstruction.str();
                    outputFile << "br label %" << endLabel << "\n";
                    outputFile << endLabel << ":\n";
                }

                if (hasReturnType) {
                    // TODO: figure out i1, i16, etc... vs struct storage
                    auto phiResult = environment.addTemporary();
                    auto resultTypeName = Types::Pool().getLLVMType(resultType.value());
                    outputFile << phiResult << " = phi " << resultTypeName << " [" << ifResult->llvmName() << ", %" << ifLabel << "], [" << elseResult->llvmName() << ", %" << elseLabel << "]\n";
                    outputFile << "store " << resultTypeName << phiResult << ", " << resultTypeName << "* " << resultVariable << "\n";
                    // TODO: figure out storage type and assignment based on if and else branches
                    return Reference::variable(resultType.value(), std::move(resultVariable));
                }

                return Reference::Void();
            }
            case NodeType::BOOLEAN_LITERAL:
                std::cout << "Boolean literal" << std::endl;
                return Reference::of(parser.getBooleanLiteral(nodeIndex));
            case NodeType::STRUCT: {
                auto node = parser.getStruct(nodeIndex);
                auto structDefinition = Types::Struct();
                auto definitionEnv = new Environment(environment);
                for (auto fieldIndex : node.children) {
                    interpret(fieldIndex, *definitionEnv, outputFile);
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
                Types::TypeIndex structTypeIndex = Types::Pool().addStruct(structDefinition, "");
                return Reference::typeReference(structTypeIndex);
            }
            case NodeType::DOT_ACCESS: {
                auto node = parser.getDotAccess(nodeIndex);
                Reference* object = interpret(node.object, environment, outputFile);
                return object->getField(node.fieldName->lexeme);
            }
            }
        throw std::invalid_argument("Bruh im crashing tf out");
    }

    
    void run(std::ostream& outputFile) {
        for (auto node : program) {
            interpret(node, fileEnvironment, outputFile);
            while (!globalsStack.empty()) {
                outputFile << globalsStack.front().str() << "\n";
                globalsStack.pop();
            }
        }
        // // std::cout << "\n\ndone interpretting\n\n";
        // auto main = fileEnvironment.find("main");
        // if (main == nullptr) {
        //    throw std::invalid_argument("Unable to find definition for function 'main'");
        // }

        // log << "Type of main: " << main->type.value << "\n";
        // auto mainFunction = std::get<FunctionType>(main->value);
        // log << "Address of main: " << &mainFunction << "\n";
        // callUserFunction(&mainFunction, std::span<Reference*>());
        // // auto parser = mainFunction.parser;
        // // TODO: add way to interpret this
        // // auto mainBlock = mainFunction->parser.getBlock(mainFunction->function.body);
        // // // Encodings::FunctionCall mainCall {.functionValue = , .arguments = std::span<NodeIndex>()};
        // // for (auto statement : mainBlock.statements) {
        // //     interpret(statement, fileEnvironment, 1);
        // // }
    }
};
