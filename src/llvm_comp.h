#pragma once
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
            std::string valueName = std::move(environment.addTemporary());
            outputFile
                << valueName << " = load " << Types::Pool().getLLVMType(value->type) << ", ptr " << value->llvmName() << "\n";
            return std::move(valueName);
        }
        return std::move(std::string(value->llvmName()));
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
                    << dereferencedVariable << " = load " << llvmType << ", ptr " << value->llvmName() << "\n";
                return Reference::variable(dereferencedType.value(), std::move(dereferencedVariable));
            }
            case StorageType::LITERAL: {
                return Reference::variable(dereferencedType.value(), std::move(LLVMName(value->llvmName())));
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

                bool compileTime = parser.getToken(encoded.token).type == TokenType::COLON;

                auto reference = interpret(node.definition, environment, outputFile, context);
                reference->assign(value);
                reference->isMutable = !compileTime;
                if (!compileTime) {
                    outputFile << fmt::format("{} = alloca {}\n", reference->llvmName(), Types::Pool().getLLVMType(reference->type));
                    outputFile << fmt::format("store {} {}, ptr {}\n", Types::Pool().getLLVMType(value->type), toLiteral(value, outputFile, environment), ", ptr ", reference->llvmName());
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
                auto value = interpret(node.value, environment, outputFile, context);

                auto assignee = interpret(node.assignee, environment, outputFile, context);
                if (assignee->storageType != StorageType::VARIABLE) {
                    throw std::invalid_argument("Can't assign to literal");
                }
                assignee->assign(value);
                std::string_view type = Types::Pool().getLLVMType(value->type);
                std::string valueName = toLiteral(value, outputFile, environment);
                outputFile << fmt::format("store {} {}, ptr {}\n", type, valueName, assignee->llvmName());
              
                // TODO: consider value
                return Reference::Void();
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
                    instruction << fmt::format(" = global [{} x i8] c\"{}\" align 1\n", name, stringValue.length(), stringValue);
                    globalsStack.push(std::move(instruction));
                    Reference* ref = Reference::literal(Types::indexOf(Types::Intrinsic::STRING), std::move(name));
                    return ref;
                }
                if (token.type == TokenType::NULL_TERMINATED_STRING) {
                    auto [_, name] = environment.addConstant();
                    auto stringValue = token.lexeme;
                    static std::string nullByte = "\\00";
                    std::stringstream instruction;
                    instruction << fmt::format(" = global [{} x i8] c\"{}{}\" align 1\n", name, stringValue.length()+1, stringValue, nullByte);
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
                auto rightVal = interpret(node.right, environment, outputFile, context);
                auto leftLiteral = toLiteral(leftVal, outputFile, environment);
                auto rightLiteral = toLiteral(rightVal, outputFile, environment);

                if (bool isArithmetic = (opType == TokenType::PLUS || opType == TokenType::MINUS || opType == TokenType::MULT || opType == TokenType::DIV)) {
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
                        default:
                            throw std::invalid_argument("Unknown binary operation");
                    }
                    auto llvmTypeName = Types::Pool().getLLVMType(type);
                    outputFile << fmt::format("{} = {} {} {}, {}\n", resultName, binaryOperator, llvmTypeName, leftLiteral, rightLiteral);
                    return Reference::literal(type, std::move(resultName));
                }

                auto leftType = leftVal->type;
                auto rightType = rightVal->type;

                switch(opType) {
                    // TODO: slicing operations
                    case TokenType::LEFT_BRACKET: {
                        // [left]right
                        // ^
                        if (parser.nodeTokenPrecedes(nodeIndex, node.left)) {
                            // TODO: comptime evaluation
                            return ;
                        }

                        // left[right]
                        //     ^
                        if (auto sliceElement = Types::Pool().sliceType(leftType)) {
                            Types::TypeIndex elementType = *sliceElement;
                            TODO("Indices for slices");
                        } else if (auto arrayElement = Types::Pool().arrayType(rightType)) {
                            auto resultName = environment.addTemporary();
                            Types::TypeIndex elementType = *arrayElement;
                            if (!Types::Pool().isInt(rightType)) {
                                throw std::invalid_argument("Index must be an integer");
                            }
                            outputFile << fmt::format("{} = getelementptr {}, ptr {}, {} {}", resultName, Types::Pool().getLLVMType(elementType), leftLiteral, Types::Pool().getLLVMType(rightType), rightLiteral) << "\n";
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
                        for (auto arg : args.requiredInputs) {
                            TODO("Passing in named arguments");
                        }
                        if (function->type == Types::indexOf(Types::Intrinsic::FUNCTION)) {
                            TODO("Calling for user functions");
                        }
                        if (function->type == Types::indexOf(Types::Intrinsic::LLVM_FUNCTION)) {
                            // TODO: return value
                            auto nativeCall = std::get<LLVMFunction>(function->value);
                            std::vector<std::string> args(arguments.size());
                            for (Reference* arg : arguments) {
                                args.push_back(toLiteral(arg, outputFile, environment));
                            }

                            outputFile << "call " << nativeCall.usage << "(";
                            bool hasMultipleArgs = false;
                            for (int i; i < args.size(); i++) {
                                if (hasMultipleArgs) {
                                    outputFile << ", ";
                                }
                                outputFile << Types::Pool().getLLVMType(arguments[i]->type) << " " << args[i];
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
                if(context.name.has_value()) {
                    prefix = context.name.value();
                    llvmName = forwardDeclare ? prefix : environment.addConstant(prefix);
                } else {
                    auto [prefixNum, name] = environment.addConstant();
                    llvmName = std::move(name);
                    _prefix = std::to_string(prefixNum);
                    prefix = std::string_view(_prefix);
                }

                Environment functionEnvironment = Environment(&environment, llvmName);
                CompilerContext functionContext(context);
                functionContext.returnType = returnType;
                
                bool canReturn = Types::Pool().isLlvmLiteralType(returnType);
                instruction << fmt::format("{} {} {}(", (forwardDeclare ? "define" : "declare"), (canReturn ? Types::Pool().getLLVMType(returnType) : "void"), llvmName);
                bool hasParameters = false;
                if (!canReturn) {
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

                    Types::TypeIndex parameterType = std::get<Types::TypeIndex>(interpret(parameterDefinition.type.value(), functionEnvironment, instruction, context)->value);
                    parameterTypes.push_back(parameterType);
                    bool isLiteralParameter = Types::Pool().isLlvmLiteralType(parameterType);
                    
                    std::string_view paramName = parameterDefinition.name->lexeme;
                    std::string parameterLlvmName = "%" + std::string(paramName);
                    functionEnvironment.define(paramName, isLiteralParameter ? Reference::literal(parameterType, std::move(parameterLlvmName)) : Reference::variable(parameterType, std::move(llvmName)));

                    instruction << Types::Pool().getLLVMType(parameterType);
                    if (!isLiteralParameter) {
                        // TODO: type alignment; for now align to s64
                        instruction << "* byval align 8 " << parameterLlvmName;
                    }
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
                Types::TypeIndex functionType = Types::Pool().addFunction(parameterTypes, returnType);
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
            case NodeType::WHILE: {
                auto node = parser.getWhileLoop(nodeIndex);
                auto loopId = environment.getNextTemporary();
                auto loopHeader = environment.addLabel("while", loopId);
                auto loopBody = loopHeader + ".continue";
                auto endLabel = loopHeader + ".break";
                auto condition = interpret(node.condition, environment, outputFile, context);
                if (condition->type != Types::Pool().boolean) {
                    std::stringstream ss;
                    ss << "Condition for while loop must be of type 'bool', but was of type '" << Types::Pool().typeName(condition->type) << "'";
                    throw std::invalid_argument(ss.str());
                }

                outputFile << fmt::format("{}:\n", loopHeader);
                auto conditionLiteral = toLiteral(condition, outputFile, environment);

                outputFile << fmt::format("br i1 {}, label %{}, label %{}\n", conditionLiteral, loopBody, endLabel);
                outputFile << loopBody << ":\n";

                interpret(node.loopBody, environment, outputFile, context);

                outputFile << fmt::format("br label %{}\n", loopHeader);
                outputFile << endLabel << ":\n";
                return Reference::Void();
            }
            case NodeType::INPUT_LIST:
            case NodeType::IMPORT:
              break;
            case NodeType::CALL:
              break;
            }
        throw std::invalid_argument("Bruh im crashing tf out");
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
