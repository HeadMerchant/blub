#pragma once
#include <cassert>
#include "fmt/core.h"
#include <fmt/args.h>
#include <fmt/ranges.h>
#include "parser.h"
#include "environment.h"
#include <charconv>
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
#include <fmt/format.h>

struct CompilerContext {
    // NodeIndex index;
    // std::ostream& outputFile;
    // Environment& environment;
    
    std::optional<std::string_view> name;

    Types::OptionalType expectedType;
    Types::OptionalType returnType;
};

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
            std::string valueName = environment.addTemporary();
            outputFile
                << fmt::format("{} = load {}, ptr {}\n", valueName, Types::Pool().getLLVMType(value->type), value->llvmName());
            return valueName;
        }
        return std::string(value->llvmName());
    }

    std::string toVariable(Reference* value, std::ostream& outputFile, Environment& environment) {
        if (value->storageType == StorageType::LITERAL) {
            auto typeName = Types::Pool().getLLVMType(value->type);
            std::string valueName = environment.addTemporary();
            outputFile
                << fmt::format("{} = alloca {}, align {}\nstore {} {}, ptr {}\n", valueName, typeName, Types::Pool()[value->type].alignment, typeName, value->llvmName(), valueName);
            return valueName;
        }
        return std::string(value->llvmName());
    }

    Reference* dereference(Reference* value, std::ostream& outputFile, Environment& environment) {
        Types::OptionalType dereferencedType = Types::Pool().dereference(value->type);
        if (!dereferencedType.has_value()) {
            std::stringstream message;
            message << "Unable to dereference non-pointer type " << Types::Pool().typeName(value->type);
            throw std::invalid_argument(std::move(message.str()));
        }
        
        auto type = dereferencedType.value();
        auto llvmType = Types::Pool().getLLVMType(type);
        switch (value->storageType) {
            case StorageType::VARIABLE: {
                LLVMName dereferencedVariable = environment.addTemporary();
                outputFile
                    << dereferencedVariable << " = load ptr, ptr " << value->llvmName() << "\n";
                return Reference::variable(dereferencedType.value(), std::move(dereferencedVariable));
            }
            case StorageType::LITERAL: {
                return Reference::variable(dereferencedType.value(), LLVMName(value->llvmName()));
            }
        }
    }
    
    Reference* interpret(NodeIndex nodeIndex, Environment& environment, std::ostream& outputFile, CompilerContext& context) {
        auto encoded = parser.getNode(nodeIndex);
        auto prefix = environment.prefix;
        switch(encoded.nodeType) {
            case NodeType::BLOCK: {
                auto node = parser.getBlock(nodeIndex);
                Environment blockEnv(environment);
                for (auto child : node.statements) {
                    interpret(child, blockEnv, outputFile, context);
                }
                return Reference::Void();
            }
            case NodeType::DECLARATION: {
                auto node = parser.getDeclaration(nodeIndex);
                // NOTE: don't support using non-identifiers
                auto definitionName = parser.getDefinition(node.definition).name->lexeme;
                auto valueContext = CompilerContext(context);
                valueContext.name = definitionName;
                auto value = interpret(node.value, environment, outputFile, valueContext);

                bool compileTime = parser.getToken(encoded.token)->type == TokenType::COLON;

                auto reference = interpret(node.definition, environment, outputFile, context);
                Types::OptionalType valueType = reference->assign(value);
                if (!valueType) {
                    throw std::invalid_argument(fmt::format("Unable to assign value of type {} to declaration of type {}", Types::Pool().typeName(value->type), Types::Pool().typeName(reference->type)));
                }
                reference->isMutable = !compileTime;
                if (!compileTime) {
                    outputFile << fmt::format("{} = alloca {}\n", reference->llvmName(), Types::Pool().getLLVMType(reference->type));
                    outputFile << fmt::format("store {} {}, ptr {}\n", Types::Pool().getLLVMType(*valueType), toLiteral(value, outputFile, environment), reference->llvmName());
                } else {
                    // TODO: figure out naming for this compile-time
                    reference->value = std::string(value->llvmName());
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
                    Reference* type = interpret(node.type.value(), environment, outputFile, context);
                    if (!type->isType()) {
                        std::stringstream message;
                        message << "Type for identifier " << node.name->lexeme << " is not a type";
                        throw std::invalid_argument(message.str());
                    }
                    Types::TypeIndex typeIndex = std::get<Types::TypeIndex>(type->value);
                    ref = Reference::variable(typeIndex, std::move(llvmName.str()));
                }
                environment.define(node.name->lexeme, ref);
                return ref;
            }
            case NodeType::LITERAL: {
                auto node = parser.getLiteral(nodeIndex);
                auto token = *node.token;
                if (token.type == TokenType::STRING) {
                    TODO("Length-based strings");
                    auto [_, name] = environment.addConstant();
                    auto stringValue = token.lexeme;
                    // TODO: use string types instead of C strings
                    std::stringstream instruction;
                    instruction << fmt::format("{} = global [{} x i8] c\"{}\" align 1\n", name, stringValue.length(), stringValue);
                    globalsStack.push(std::move(instruction));
                    Reference* ref = Reference::literal(Types::indexOf(Types::Intrinsic::STRING), std::move(name));
                    return ref;
                }
                if (token.type == TokenType::NULL_TERMINATED_STRING) {
                    auto [_, name] = environment.addConstant();
                    auto stringValue = token.lexeme;
                    static std::string nullByte = "\\00";
                    std::stringstream instruction;
                    instruction << fmt::format("{} = global [{} x i8] c\"{}{}\" align 1\n", name, stringValue.length()+1, stringValue, nullByte);
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
                if (token.type == TokenType::TRUE) {
                    return Reference::of(true);
                }
                if (token.type == TokenType::FALSE) {
                    return Reference::of(false);
                }
                if (token.type == TokenType::IDENTIFIER) {
                    std::optional<Reference*> value = environment.find(node.token->lexeme);
                    if (!value.has_value()) {
                        std::string message = fmt::format("Identifier \"{}\" not defined", node.token->lexeme);
                        throw std::invalid_argument(std::move(message));
                    }
                    return value.value();
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
                auto opType = node.operation->type;
                auto leftVal = interpret(node.left, environment, outputFile, context);

                if (node.operation->isArithmeticOperation()) {
                    auto rightVal = interpret(node.right, environment, outputFile, context);
                    auto leftLiteral = toLiteral(leftVal, outputFile, environment);
                    auto rightLiteral = toLiteral(rightVal, outputFile, environment);
                    auto resultName = environment.addTemporary();
                    auto resultType = Types::Pool().coerce(leftVal->type, rightVal->type);
                    if (!resultType.has_value()) {
                        throw std::invalid_argument("Unable to perform binary operation on incompatible types");
                    }
                    auto type = resultType.value();
                    std::string binaryOperator;
                    switch (opType) {
                        case TokenType::PLUS: {
                            if (Types::Pool().isInt(type)) binaryOperator = "add";
                            else if (Types::Pool().isFloat(type)) binaryOperator = "fadd";
                            else crashBinOp(node.operation, leftVal, rightVal);
                            break;
                        }
                        case TokenType::MINUS: {
                            if (Types::Pool().isInt(type)) binaryOperator = "sub";
                            else if (Types::Pool().isFloat(type)) binaryOperator = "fsub";
                            else crashBinOp(node.operation, leftVal, rightVal);
                            break;
                        }
                        case TokenType::DIV: {
                            if (Types::Pool().isSignedInt(type)) binaryOperator = "sdiv";
                            else if (Types::Pool().isInt(type)) binaryOperator = "udiv";
                            else if (Types::Pool().isFloat(type)) binaryOperator = "fdiv";
                            else crashBinOp(node.operation, leftVal, rightVal);
                            break;
                        }
                        case TokenType::MULT: {
                            if (Types::Pool().isInt(type)) binaryOperator = "mul";
                            else if (Types::Pool().isFloat(type)) binaryOperator = "fmul";
                            else crashBinOp(node.operation, leftVal, rightVal);
                            break;
                        }
                        case TokenType::LESS_THAN: {
                            if (Types::Pool().isSignedInt(type)) binaryOperator = "icmp slt";
                            else if (Types::Pool().isInt(type)) binaryOperator = "icmp ult";
                            else if (Types::Pool().isFloat(type)) binaryOperator = "fcmp olt";
                            else crashBinOp(node.operation, leftVal, rightVal);
                            break;
                        }
                        case TokenType::GREATER_THAN: {
                            if (Types::Pool().isSignedInt(type)) binaryOperator = "icmp sgt";
                            else if (Types::Pool().isInt(type)) binaryOperator = "icmp ugt";
                            else if (Types::Pool().isFloat(type)) binaryOperator = "fcmp ogt";
                            else crashBinOp(node.operation, leftVal, rightVal);
                            break;
                        }
                        case TokenType::LEQ: {
                            if (Types::Pool().isSignedInt(type)) binaryOperator = "icmp sle";
                            else if (Types::Pool().isInt(type)) binaryOperator = "icmp ule";
                            else if (Types::Pool().isFloat(type)) binaryOperator = "fcmp ole";
                            else crashBinOp(node.operation, leftVal, rightVal);
                            break;
                        }
                        case TokenType::GEQ: {
                            if (Types::Pool().isSignedInt(type)) binaryOperator = "icmp sge";
                            else if (Types::Pool().isInt(type)) binaryOperator = "icmp uge";
                            else if (Types::Pool().isFloat(type)) binaryOperator = "fcmp oge";
                            else crashBinOp(node.operation, leftVal, rightVal);
                            break;
                        }
                        case TokenType::DOUBLE_EQUAL: {
                            if (Types::Pool().isInt(type) || Types::Pool().isPointer(type)) binaryOperator = "icmp eq";
                            else if (Types::Pool().isFloat(type)) binaryOperator = "fcmp eq";
                            else crashBinOp(node.operation, leftVal, rightVal);
                            break;
                        }
                        case TokenType::NOT_EQUAL: {
                            if (Types::Pool().isInt(type) || Types::Pool().isPointer(type)) binaryOperator = "icmp ne";
                            else if (Types::Pool().isFloat(type)) binaryOperator = "fcmp ne";
                            else crashBinOp(node.operation, leftVal, rightVal);
                            break;
                        }
                        default:
                            throw std::invalid_argument("Unknown binary operation");
                    }
                    auto llvmTypeName = Types::Pool().getLLVMType(type);
                    outputFile << fmt::format("{} = {} {} {}, {}\n", resultName, binaryOperator, llvmTypeName, leftLiteral, rightLiteral);
                    return Reference::literal(type, std::move(resultName));
                }

                auto leftType = leftVal->type;

                switch(opType) {
                    case TokenType::WHILE: {
                        auto rightVal = interpret(node.right, environment, outputFile, context);
                        auto rightType = rightVal->type;
                        auto loopId = environment.getNextTemporary();
                        auto loopHeader = environment.addLabel("while", loopId);
                        auto loopBody = loopHeader + ".continue";
                        auto endLabel = loopHeader + ".break";
                        auto condition = interpret(node.left, environment, outputFile, context);
                        if (condition->type != Types::Pool().boolean) {
                            std::stringstream ss;
                            ss << "Condition for while loop must be of type 'bool', but was of type '" << Types::Pool().typeName(condition->type) << "'";
                            throw std::invalid_argument(ss.str());
                        }

                        outputFile << fmt::format("{}:\n", loopHeader);
                        auto conditionLiteral = toLiteral(condition, outputFile, environment);

                        outputFile << fmt::format("br i1 {}, label %{}, label %{}\n", conditionLiteral, loopBody, endLabel);
                        outputFile << loopBody << ":\n";

                        interpret(node.right, environment, outputFile, context);

                        outputFile << fmt::format("br label %{}\n", loopHeader);
                        outputFile << endLabel << ":\n";
                        // TODO: consider value expression (see https://ziglang.org/documentation/master/#while)
                        return Reference::Void();
                    }
                    case TokenType::ASSIGNMENT: {
                        auto value = interpret(node.right, environment, outputFile, context);
                        auto assignee = interpret(node.left, environment, outputFile, context);

                        if (assignee->storageType != StorageType::VARIABLE) {
                            throw std::invalid_argument("Can't assign to literal");
                        }
                        Types::OptionalType valueType = assignee->assign(value);
                        if (!valueType) {
                            throw std::invalid_argument(fmt::format("Can't assign value of type {} to symbol of type {}", Types::Pool().typeName(value->type), Types::Pool().typeName(assignee->type)));
                        }
                        std::string valueName = toLiteral(value, outputFile, environment);
                        outputFile << fmt::format("store {} {}, ptr {}\n", Types::Pool().getLLVMType(*valueType), valueName, assignee->llvmName());
          
                        // TODO: consider value
                        return Reference::Void();
                    }
                    // TODO: slicing operations
                    case TokenType::LEFT_BRACKET: {
                        // [left]right
                        // ^
                        if (parser.nodeTokenPrecedes(nodeIndex, node.left)) {
                            // TODO: comptime evaluation
                            auto sizeNode = parser.getNode(node.left);
                            auto sizeToken = parser.getToken(sizeNode.token);
                            if (sizeNode.nodeType != NodeType::LITERAL || sizeToken->type != TokenType::INT) {
                                throw std::invalid_argument("Array size must be a non-negative int");
                            }
                            i32 arrayLength;
                            std::from_chars(sizeToken->lexeme.data(), sizeToken->lexeme.data() + sizeToken->lexeme.length(), arrayLength);

                            if (auto elementType = interpret(node.right, environment, outputFile, context)->unboxType()) {
                                return Reference::typeReference(Types::Pool().sizedArrayOf(*elementType, arrayLength));
                            }

                            throw std::invalid_argument("Array element type must be a type");
                        }

                        auto leftLiteral = toLiteral(leftVal, outputFile, environment);
                        auto rightVal = interpret(node.right, environment, outputFile, context);
                        auto rightLiteral = toLiteral(rightVal, outputFile, environment);
                        auto rightType = rightVal->type;
                        Types::OptionalType arrayElement;
                        
                        // left[right]
                        //     ^
                        if (auto sliceElement = Types::Pool().sliceElementType(leftType)) {
                            Types::TypeIndex elementType = *sliceElement;
                            TODO("Indices for slices");
                        } else if ((arrayElement = Types::Pool().multiPointerElementType(leftType)) || (arrayElement = Types::Pool().sizedArrayElementType(leftType))) {
                            auto resultName = environment.addTemporary();
                            Types::TypeIndex elementType = *arrayElement;
                            if (!Types::Pool().isInt(rightType)) {
                                throw std::invalid_argument("Index must be an integer");
                            }
                            outputFile << fmt::format("{} = getelementptr {}, ptr {}, {} {}\n", resultName, Types::Pool().getLLVMType(elementType), leftLiteral, Types::Pool().getLLVMType(rightType), rightLiteral);
                            return Reference::variable(elementType, std::move(resultName));
                        } else crashBinOp(node.operation, leftVal, rightVal);
                        break;
                    }
                    case TokenType::LEFT_PAREN: {
                        auto function = interpret(node.left, environment, outputFile, context);
                        auto args = parser.getInputList(node.right);

                        std::vector<Reference*> arguments;
                        for (auto arg : args.requiredInputs) {
                            arguments.push_back(interpret(arg, environment, outputFile, context));
                        }
                        for (auto arg : args.optionalInputs) {
                            TODO("Passing in named arguments");
                        }

                        if (function->type == Types::indexOf(Types::Intrinsic::LLVM_FUNCTION)) {
                            // TODO: return value
                            auto nativeCall = std::get<LLVMFunction>(function->value);
                            std::vector<std::string> args;
                            for (Reference* arg : arguments) {
                                args.push_back(toLiteral(arg, outputFile, environment));
                            }

                            outputFile << fmt::format("call {}(", nativeCall.usage);
                            bool hasMultipleArgs = false;
                            for (int i = 0; i < args.size(); i++) {
                                if (hasMultipleArgs) {
                                    outputFile << ", ";
                                }
                                outputFile << Types::Pool().getLLVMType(arguments[i]->type) << " " << args[i];
                                hasMultipleArgs = true;
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

                        if (auto functionType = Types::Pool().functionType(function->type)) {
                            std::vector<std::string> parameters;

                            auto returnType = functionType->returnType;
                            auto parameterTypes = Types::Pool().tupleElements(functionType->parameters);

                            std::string resultName;
                            bool literalReturn = Types::Pool().isLlvmLiteralType(returnType);
                            bool hasReturn = !Types::Pool().isVoid(returnType);
                            if (hasReturn) {
                                resultName = environment.addTemporary();
                            }

                            if (hasReturn) {
                                outputFile << fmt::format("{} = alloca {}, align {}\n", resultName, Types::Pool().getLLVMType(returnType), Types::Pool()[returnType].alignment);
                                parameters.push_back(fmt::format("{}* sret {}", Types::Pool().getLLVMType(returnType), resultName));
                            }

                            // TODO
                            for (i32 i = 0; i < arguments.size(); i++) {
                                if (auto argType = Types::Pool().coerce(parameterTypes[i], arguments[i]->type)) {
                                    if (Types::Pool().isLlvmLiteralType(*argType)) {
                                        parameters.push_back(fmt::format("{} {}", Types::Pool().getLLVMType(*argType), toLiteral(arguments[i], outputFile, environment)));
                                    } else {
                                        parameters.push_back(fmt::format("ptr byval({}) {}", Types::Pool().getLLVMType(*argType), toVariable(arguments[i], outputFile, environment)));
                                    }
                                } else {
                                    throw std::invalid_argument(fmt::format("Invalid argument in function call (can't pass argument of type {} to parameter of type {})", Types::Pool().typeName(arguments[i]->type), Types::Pool().typeName(parameterTypes[i])));
                                }
                            }

                            if (literalReturn) {
                                outputFile << fmt::format("{} = ", resultName);
                            }
                            outputFile << fmt::format("call {} {}({})\n", literalReturn ? Types::Pool().getLLVMType(returnType) : "void", function->llvmName(), fmt::join(parameters, ", "));
                            
                            if (Types::Pool().isVoid(returnType)) {
                                return Reference::Void();
                            }
                            if (literalReturn) {
                                return Reference::literal(returnType, resultName);
                            }
                            return Reference::variable(returnType, resultName);
                            // TODO: optional arguments
                        }

                        std::cout << "Unknown function type: " << Types::Pool().typeName(function->type) << std::endl;
                        throw std::invalid_argument("Unknown function type");
                    }
                    default:
                        throw std::invalid_argument("Unknown binary operation");
                }

            }
            case NodeType::FUNCTION_LITERAL: {
                auto node = parser.getFunctionLiteral(nodeIndex);
                Types::TypeIndex returnType = Types::indexOf(Types::Intrinsic::VOID);
                if (node.returnType.has_value()) {
                    auto boxedReturnType = interpret(node.returnType.value(), environment, outputFile, context);
                    if (!boxedReturnType->isType()) {
                        throw std::invalid_argument("Return type of function must be a compile-time known type");
                    }
                    returnType = std::get<Types::TypeIndex>(boxedReturnType->value);
                }

                std::stringstream instruction;
                std::string llvmName;
                std::string _prefix;
                std::string_view prefix;
                bool forwardDeclare = !node.body.has_value();
                if (forwardDeclare) {
                    fmt::println("Forward declaring function {}", context.name.value_or("missing name"));
                } else {
                    fmt::println("Not forward declaring");
                }
                if(context.name.has_value()) {
                    prefix = context.name.value();
                    llvmName = forwardDeclare ? fmt::format("@{}", prefix) : environment.addConstant(prefix);
                } else {
                    auto [prefixNum, name] = environment.addConstant();
                    llvmName = std::move(name);
                    _prefix = std::to_string(prefixNum);
                    prefix = std::string_view(_prefix);
                }

                Environment functionEnvironment = Environment(&environment, prefix);
                CompilerContext functionContext(context);
                functionContext.returnType = returnType;
                
                Types::LLVMStorage returnStorage = Types::Pool().storageType(returnType);
                // TODO: NEXT
                instruction << fmt::format("{} {} {}(", (forwardDeclare ? "declare" : "define"), (returnStorage == Types::LLVMStorage::LITERAL ? Types::Pool().getLLVMType(returnType) : "void"), llvmName);
                bool hasParameters = false;
                if (returnStorage == Types::LLVMStorage::VARIABLE) {
                    // TODO: factor out %return register
                    instruction << Types::Pool().getLLVMType(returnType) << "* noalias sret %return";
                    hasParameters = true;
                }
                auto parameters = parser.getInputList(node.parameters);
                std::vector<Types::TypeIndex> parameterTypes;
                for (NodeIndex parameterIndex : parameters.requiredInputs) {
                    if (hasParameters) {
                        instruction << ", ";
                    }
                    hasParameters = true;

                    auto parameterDefinition = parser.getDefinition(parameterIndex);
                    if (!parameterDefinition.type.has_value()) {
                        throw std::invalid_argument("Parameters must have a type");
                    }

                    auto parameterType = interpret(*parameterDefinition.type, functionEnvironment, instruction, context)->unboxType();
                    if (!parameterType) {
                        throw std::invalid_argument("Parameter type must be a compile time-known type");
                    }

                    fmt::println("Parameter type: {} ({})", Types::Pool().typeName(*parameterType), parameterType->value);
                    parameterTypes.push_back(*parameterType);
                    bool isLiteralParameter = Types::Pool().isLlvmLiteralType(*parameterType);
                    
                    std::string_view paramName = parameterDefinition.name->lexeme;
                    std::string parameterLlvmName = "%" + std::string(paramName);

                    auto typeName = Types::Pool().getLLVMType(*parameterType);
                    if (isLiteralParameter) {
                        instruction << fmt::format("{} {}", typeName, parameterLlvmName);
                    } else {
                        // TODO: type alignment; for now align to s64
                        instruction << fmt::format("{}* byval align 8 {}", typeName, parameterLlvmName);
                    }

                    functionEnvironment.define(paramName, isLiteralParameter ? Reference::literal(*parameterType, std::move(parameterLlvmName)) : Reference::variable(*parameterType, std::move(parameterLlvmName)));
                }

                for (NodeIndex parameterIndex : parameters.optionalInputs) {
                    TODO("Named parameters/default values");
                }

                if (!forwardDeclare) {
                    // TODO: attributes https://llvm.org/docs/LangRef.html#function-attributes
                    instruction << ") {\n";


                    auto body = parser.getBlock(node.body.value());
                    std::vector<Types::TypeIndex> parameterTypes;
                    for (auto statement : body.statements) {
                        interpret(statement, functionEnvironment, instruction, functionContext);
                    }

                    // TODO: ensure return value
                    if (returnType == indexOf(Types::Intrinsic::VOID)) {
                        instruction << "ret void\n";
                    }

                    instruction << "}\n\n";
                }

                globalsStack.push(std::move(instruction));
                Types::TypeIndex parameterTuple = Types::Pool().tupleOf(std::move(parameterTypes));
                Types::TupleIndex tupleType = Types::Pool().tupleIndex(parameterTuple);
                Types::TypeIndex functionType = Types::Pool().addFunction(Types::FunctionType(tupleType, returnType));
                std::cout << fmt::format("Making function of type {}", Types::Pool().typeName(functionType)) << std::endl;
                return Reference::literal(functionType, std::move(llvmName));
            }
            case NodeType::UNARY: {
                auto node = parser.getUnary(nodeIndex);
                auto value = interpret(node.operand, environment, outputFile, context);
                switch(node.operation) {
                    case UnaryOps::DEREFERENCE: return dereference(value, outputFile, environment);
                    case UnaryOps::REFERENCE: {
                        if (value->isType()) {
                            return Reference::pointerTo(std::get<Types::TypeIndex>(value->value));
                        } else {
                            return Reference::pointerTo(value);
                        }
                    }
                    case UnaryOps::NOT: {
                        auto type = value->type;
                        if (!(Types::Pool().isInt(type) || value->type == Types::Pool().boolean)) {
                            throw std::invalid_argument("Unary not operator '!' can only be used on integer and boolean types");
                        }
                        auto valueName = toLiteral(value, outputFile, environment);
                        auto resultName = environment.addTemporary();
                        outputFile << fmt::format("{} = not {} {}\n", resultName, Types::Pool().getLLVMType(type), valueName);
                        return Reference::literal(type, valueName);
                    }
                    case UnaryOps::SLICE: {
                        auto elementType = value->unboxType();
                        if (!elementType.has_value()) {
                            throw std::invalid_argument("Element type for array type (slice, fixed-size, etc) must be a compile-time known type");
                        }
                        return Reference::typeReference(Types::Pool().multiPointerTo(elementType.value()));
                    }
                    case UnaryOps::MUTLI_POINTER: {
                        auto elementType = value->unboxType();
                        if (!elementType.has_value()) {
                            throw std::invalid_argument("Element type for array type (slice, fixed-size, etc) must be a compile-time known type");
                        }
                        return Reference::typeReference(Types::Pool().sliceOf(elementType.value()));
                    }
                }
            }
            case NodeType::IF: {
                auto node = parser.getIf(nodeIndex);
                // TODO: add br instruction
                auto condition = interpret(node.condition, environment, outputFile, context);
                if (condition->type != Types::Pool().boolean) {
                    std::string message = fmt::format("Condition of an 'if' statement needs to be of type 'bool', but was type '{}' {}", Types::Pool().typeName(condition->type), condition->llvmName());
                    throw std::invalid_argument(std::move(message));
                }

                auto blockIndex = environment.getNextTemporary();
                auto resultVariable = environment.addTemporary(blockIndex);
                auto ifLabel = environment.addLabel("if", blockIndex);
                auto endLabel = environment.addLabel("endif", blockIndex);
                auto elseLabel = environment.addLabel("else", blockIndex);

                // If
                std::stringstream ifInstruction;
                ifInstruction << ifLabel << ":\n";
                auto ifResult = interpret(node.value, environment, ifInstruction, context);
                std::optional<Types::TypeIndex> resultType = ifResult->type;

                // Else
                Reference* elseResult;
                std::stringstream elseInstruction;
                if (node.elseValue.has_value()) {
                    elseInstruction << elseLabel << ":\n";
                    elseResult = interpret(node.elseValue.value(), environment, elseInstruction, context);
                    resultType = Types::Pool().isAssignable(resultType.value(), elseResult->type);
                }

                // Results
                auto voidType = Types::indexOf(Types::Intrinsic::VOID);
                auto hasReturnType = (resultType.value_or(voidType) != voidType) && node.elseValue.has_value();
                if (hasReturnType) {
                    outputFile << fmt::format("{} = alloca {}\n", resultVariable, Types::Pool().getLLVMType(resultType.value()));
                }
                std::string conditionName = toLiteral(condition, outputFile, environment);

                if (node.elseValue.has_value()) {
                    outputFile << fmt::format("br i1 {}, label %{}, label %{}\n", conditionName, ifLabel, elseLabel);
                    outputFile << ifInstruction.str();
                    outputFile << fmt::format("br label %{}\n", endLabel);
                    outputFile << elseInstruction.str();
                    outputFile << endLabel << ":\n";
                } else {
                    outputFile << fmt::format("br i1 {}, label %{}, label %{}\n", conditionName, ifLabel, endLabel);
                    outputFile << ifInstruction.str();
                    outputFile << fmt::format("br label %{}\n", endLabel);
                    outputFile << endLabel << ":\n";
                }

                if (hasReturnType) {
                    // TODO: figure out i1, i16, etc... vs struct storage
                    auto phiResult = environment.addTemporary();
                    auto resultTypeName = Types::Pool().getLLVMType(resultType.value());
                    outputFile << fmt::format("{}  = phi {} [{}, %{}], [{}, %{}]\n", phiResult, resultTypeName, ifResult->llvmName(), ifLabel, elseResult->llvmName(), elseLabel);
                    outputFile << fmt::format("store {} {}, {}* {}\n", resultTypeName, phiResult, resultTypeName, resultVariable);
                    // TODO: figure out storage type and assignment based on if and else branches
                    return Reference::variable(resultType.value(), std::move(resultVariable));
                }

                return Reference::Void();
            }
            case NodeType::STRUCT: {
                auto node = parser.getStruct(nodeIndex);
                // TODO: methods
                auto structDefinition = Types::Struct();

                std::stringstream ss;
                std::string llvmName = context.name.has_value() ? environment.addGlobal(context.name.value()) : environment.addGlobal();
                ss << llvmName << " = type {";
                i32 fieldIndexNumber = 0;
                bool hasFields = false;
                for (auto fieldIndex : node.children) {
                    auto fieldNode = parser.getNode(fieldIndex);
                    auto nodeType = fieldNode.nodeType;
                    switch (nodeType) {
                        case NodeType::DEFINITION: {
                            if (hasFields) ss << ", ";
                            auto definitionNode = parser.getDefinition(fieldIndex);
                            Reference* boxedType = interpret(definitionNode.type.value(), environment, outputFile, context);
                            Types::TypeIndex type = std::get<Types::TypeIndex>(boxedType->value);

                            // TODO: default values
                            structDefinition.fields.define(definitionNode.name->lexeme, Reference::structField(type, fieldIndexNumber));
                            ss << Types::Pool().getLLVMType(type);
                            hasFields = true;
                            fieldIndexNumber++;
                            break;
                        }
                        default: throw std::invalid_argument("TODO: implement struct fields");
                    }
                    fieldIndexNumber++;
                }
                ss << "}\n";
                globalsStack.push(std::move(ss));
            }
            case NodeType::DOT_ACCESS: {
                auto node = parser.getDotAccess(nodeIndex);
                Reference* object = interpret(node.object, environment, outputFile, context);
                if (object->isType()) {
                    throw std::invalid_argument("Unable to get field for non-object");
                }
                Types::TypeIndex type = object->type;
                std::string_view objectLlvmType = Types::Pool().getLLVMType(type);

                // TODO: auto dereference pointers
                Types::OptionalType dereferenced = Types::Pool().dereference(type);
                if (dereferenced.has_value()) {
                    TODO("Implement automatic dereferencing for field access on pointer types");
                }
                std::string_view fieldName = node.fieldName->lexeme;
                std::optional<Reference*> boxedField = Types::Pool().getFieldIndex(type, fieldName);

                if (!boxedField.has_value()) {
                    throw std::invalid_argument(fmt::format("No field '{}' found in struct '{}'", fieldName, Types::Pool().typeName(type)));
                }
                
                // TODO: literals
                Reference* field = boxedField.value();
                std::string fieldPointer = environment.addTemporary();
                Types::TypeIndex fieldType = field->type;
                if (!field->structFieldIndex().has_value()) {
                    throw std::invalid_argument(fmt::format("Internal: malformed field index {}. Non-integer index value", field->llvmName()));
                }

                i32 fieldIndex = *field->structFieldIndex().value();
                if (object->storageType == StorageType::VARIABLE) {
                    outputFile << fmt::format("{} = getelementptr inbounds {}, ptr {}, i32 0, i32 {}\n", fieldPointer, objectLlvmType, object->llvmName(), fieldIndex);
                    return Reference::variable(fieldType, std::move(fieldPointer));
                } else {
                    outputFile << fmt::format("{} = extractvalue {} {}, {}\n", fieldPointer, objectLlvmType, object->llvmName(), fieldIndex);
                    return Reference::literal(fieldType, std::move(fieldPointer));
                }
            }
            case NodeType::INPUT_LIST: {
                throw std::invalid_argument("Input list nodes shouldn't be directly interpreted");
            }
            case NodeType::IMPORT:
              break;
            }
        throw std::invalid_argument("Bruh im crashing tf out");
    }

    std::string escapeSourceString(std::string_view str) {
        
    }
    
    void crashBinOp(TokenPointer token, Reference* leftVal, Reference* rightVal) {
        std::string message = fmt::format("Unable to perform binary operation '{}' on types '{}' and '{}'", token->lexeme, Types::Pool().typeName(leftVal->type), Types::Pool().typeName(rightVal->type));
        throw std::invalid_argument(std::move(message));
    }
    
    void run(std::ostream& outputFile) {
        CompilerContext context;
        for (auto node : program) {
            interpret(node, fileEnvironment, outputFile, context);
            while (!globalsStack.empty()) {
                outputFile << globalsStack.front().str() << "\n";
                globalsStack.pop();
            }
        }
    }
};
