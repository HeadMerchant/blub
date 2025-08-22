#pragma once
#include <optional>
#include <stdexcept>
#include <vector>
#include "types.h"
#include <cassert>
#include <variant>

struct Reference;

using StringType = std::string;
using ArrayType = std::vector<Reference*>;
using FloatType = float;

class Environment;

using Definition = std::pair<std::string_view, Types::TypeIndex>;
struct FunctionLiteral {
    std::vector<Definition> parameters;
    Types::TypeIndex returnType;
};

using FunctionType = FunctionLiteral;

using IntType = int;

struct ParameterizedType {
    Reference* type;
    std::vector<Reference*> parameters;
};
struct UserTypeValue {
    Reference* type;
    Reference* value;
};

struct LLVMFunction {
    std::string definition;
    std::string usage;
};

enum class StorageType {
    LITERAL,
    VARIABLE
};

using LLVMName = std::string;

using LiteralValue = std::variant<Types::TypeIndex, UserTypeValue, LLVMName, LLVMFunction, FunctionType, i32>;
struct Reference {
    Types::TypeIndex type;
    LiteralValue value;
    bool isMutable = true;
    bool isInitialized = false;
    StorageType storageType;
    
    static Reference* True() {
        static Reference True(true);
        return &True;
    }
    static Reference* False() {
        static Reference False(false);
        return &False;
    }
    static Reference* Void() {
        static Reference Void(Types::indexOf(Types::Intrinsic::VOID));
        return &Void;
    }
    
    Reference(Types::TypeIndex type): type(type), isInitialized(false) {}
    Reference(Types::TypeIndex type, LiteralValue value): type(type), value(std::move(value)) {}
    Reference(Types::TypeIndex type, LLVMName llvmName, StorageType storageType): type(type), value(std::move(llvmName)), storageType(storageType) {}
    Reference(Types::Intrinsic type): type(Types::indexOf(type)), isInitialized(false) {}
    Reference(FunctionType function): type(Types::indexOf(Types::Intrinsic::FUNCTION)), value(std::move(function)) {}
    Reference(LLVMFunction function): type(Types::indexOf(Types::Intrinsic::LLVM_FUNCTION)), value(std::move(function)), storageType(StorageType::LITERAL) {}
    Reference(bool value): type(Types::Pool().boolean), isMutable(false), isInitialized(true), value(value ? "true" : "false"), storageType(StorageType::LITERAL) {}

    public:
    static Reference* literal(Types::TypeIndex type, LLVMName llvmName) {
        return new Reference(type, std::move(llvmName), StorageType::LITERAL);
    }

    static Reference* variable(Types::TypeIndex type, LLVMName llvmName) {
        return new Reference(type, std::move(llvmName), StorageType::VARIABLE);
    }
    
    static Reference* of(bool boolean) {
        return boolean ? True() : False();
    }

    static Reference* ofType(Reference* type) {
        assert(type->isType());
        return new Reference(std::get<Types::TypeIndex>(type->value));
    }

    static Reference* typeReference(Types::TypeIndex index) {
        auto ref = new Reference(Types::Intrinsic::TYPE);
        ref->isInitialized = true;
        ref->isMutable = false;
        ref->value = index;
        return ref;
    }

    static Reference* toType(Types::Intrinsic index) {
        auto ref = new Reference(Types::Intrinsic::TYPE);
        ref->value = Types::indexOf(index);
        ref->isInitialized = true;
        ref->isMutable = false;
        return ref;
    }

    static Reference* pointerTo(Reference* value) {
        if (value->storageType != StorageType::VARIABLE) {
            throw std::invalid_argument("Unable to create reference to temporary value");
        }
        Types::TypeIndex pointerType = Types::Pool().pointerTo(value->type);
        assert(Types::Pool()[pointerType].definition == value->type.value);
        return Reference::variable(pointerType, std::string(value->llvmName()));
    }

    static Reference* pointerTo(Types::TypeIndex type) {
        Types::TypeIndex pointerType = Types::Pool().pointerTo(type);
        return Reference::typeReference(pointerType);
    }

    Types::OptionalType assign(Reference* newValue) {
        if (!isMutable && isInitialized) {
            throw std::invalid_argument("Unable to assign to an initialized immutable reference");
        }

        auto newType = Types::Pool().isAssignable(newValue->type, type);
        if (newType) {
            type = *newType;
        }
        return newType;
        // if (!newType.has_value()) {
        //     std::stringstream message;
        //     message << "Unable to assign value of type " << Types::Pool().typeName(newValue->type) << " to variable of type " << Types::Pool().typeName(type);
        //     throw std::invalid_argument(std::move(message.str()));
        // }

        // if (Types::Pool.isStruct(type) && newValue->type == Types::indexOf(Types::Intrinsic::ARRAY)) {
        //     ArrayType& structValue = std::get<ArrayType>(this->value);
        //     ArrayType& assignValue = std::get<ArrayType>(newValue->value);
        //     if (structValue.size() != assignValue.size()) {
        //         std::stringstream ss;
        //         ss << "Unable to assign array of length " << assignValue.size() << " to struct with " << structValue.size() << " members";
        //         throw std::invalid_argument(ss.str());
        //     }

        //     for (i32 i = 0; i < structValue.size(); i++) {
        //         structValue[i]->assign(assignValue[i]);
        //     }
        //     isInitialized = true;
        //     return;
        // }

        // if ((type != Types::indexOf(Types::Intrinsic::INFER)) && (type != newValue->type)) {
        //     std::stringstream ss;
        //     ss << "Attempted to assign value of type " << Types::Pool.typeName(newValue->type) << " to reference of type " << Types::Pool.typeName(type);
        //     throw std::invalid_argument(ss.str());
        // }

        // type = newValue->type;
        // isInitialized = true;
        // this->value = newValue->value;
    }

    // TODO: add support for printing arity
    static Reference functionString;

    Reference* toString();

    bool isType() {
        return type == Types::indexOf(Types::Intrinsic::TYPE);
    }

    Types::OptionalType unboxType() {
        if (isType()) return std::get<Types::TypeIndex>(value);
        return std::nullopt;
    }

    bool isFunction() {
        return type == Types::indexOf(Types::Intrinsic::FUNCTION);
    }

    std::string_view llvmName() {
        return std::string_view(std::get<LLVMName>(value));
    }

    static Reference* structField(Types::TypeIndex type, i32 fieldIndex) {
        return new Reference(type, LiteralValue(fieldIndex));
    }
    
    std::optional<i32*> structFieldIndex() {
        if (i32* indexPointer = std::get_if<i32>(&value)) {
            return indexPointer;
        } else {
            return std::nullopt;
        }
    }
};

