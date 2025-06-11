#pragma once
#include <functional>
#include <stdexcept>
#include <variant>
#include <vector>
#include "parser.h"
#include <sstream>
#include "types.h"
#include <cassert>

struct Reference;

using StringType = std::string;
using ArrayType = std::vector<Reference*>;
using FloatType = float;

class Environment;
struct FunctionLiteral {
    Parser* parser;
    Environment* declarationEnvironment;
    Encodings::FunctionLiteral function;
};
using FunctionType = FunctionLiteral;

using IntType = int;
using NativeFunction = std::function<Reference*(std::span<Reference*>)>*;

struct ParameterizedType {
    Reference* type;
    std::vector<Reference*> parameters;
};
struct UserTypeValue {
    Reference* type;
    Reference* value;
};

// Pointer/boxed type
struct Reference {
    std::variant<bool, IntType, FloatType, ArrayType, StringType, NativeFunction, FunctionType, Types::TypeIndex, Reference*> value;

    Types::TypeIndex type;
    bool isMutable = true;
    bool isInitialized = false;
    static Reference* True;
    static Reference* False;
    
    Reference(Types::TypeIndex type): type(type), isInitialized(false) {}
    Reference(Types::Intrinsic type): type(Types::indexOf(type)), isInitialized(false) {}
    Reference(StringType string): type(Types::indexOf(Types::Intrinsic::STRING)), value(string) {}
    Reference(ArrayType array): type(Types::indexOf(Types::Intrinsic::ARRAY)), value(array) {}
    Reference(FloatType num): type(Types::indexOf(Types::Intrinsic::FLOAT)), value(num) {}
    Reference(FunctionType func): type(Types::indexOf(Types::Intrinsic::FUNCTION)), value(func) {}
    Reference(IntType num): type(Types::indexOf(Types::Intrinsic::INT)), value(num) {}
    Reference(NativeFunction func): type(Types::indexOf(Types::Intrinsic::NATIVE_FUNCTION)), value(func) {}
    Reference(const Reference& ref, bool isMutable): type(ref.type), value(ref.value), isMutable(true), isInitialized(ref.isInitialized) {}
    private:
    Reference(bool value): type(Types::indexOf(Types::Intrinsic::BOOL)), isMutable(false), isInitialized(true), value(value) {}

    public:
        
    static Reference* of(bool boolean) {
        return boolean ? True : False;
    }

    static Reference* ofType(Reference* type) {
        assert(type->isType());

        return new Reference(std::get<Types::TypeIndex>(type->value));
    }

    static Reference* toType(Types::TypeIndex index) {
        auto ref = new Reference(Types::Intrinsic::TYPE);
        ref->value = index;
        ref->isInitialized = true;
        ref->isMutable = false;
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
        Types::TypeIndex pointerType = Types::Pool.pointerTo(value->type);
        assert(Types::Pool[pointerType].definition == value->type.value);

        auto ref = new Reference(pointerType);
        ref->value = value;

        return ref;
    }

    static Reference* pointerTo(Types::TypeIndex type) {
        Types::TypeIndex pointerType = Types::Pool.pointerTo(type);
        return Reference::toType(pointerType);
        
    }
    
    Reference* add(Reference* right);

    Reference* sub(Reference* right);

    Reference* div(Reference* right);

    Reference* mul(Reference* right);

    Reference* getField(std::string_view fieldName) {
        // TODO: consider limiting depth
        if (Types::Pool.isPointer(type)) {
            return std::get<Reference*>(value)->getField(fieldName);
        }

        Types::Type typeDefinition = Types::Pool.getDefinition(type);
        if (typeDefinition.type == Types::Intrinsic::STRUCT) {
            Types::Struct& structDefinition = Types::Pool.getStruct(type);

            Reference* fieldValue = structDefinition.getField(fieldName);
            if (fieldValue != nullptr) {
                return fieldValue;
            }

            i32 fieldIndex = structDefinition.fields.indexOf(fieldName);
            return std::get<ArrayType>(this->value).at(fieldIndex);
        }

        throw std::invalid_argument("Unable to access fields for type " + Types::Pool.typeName(type));
    }

    void assign(Reference* newValue) {
        assert(newValue->type != Types::indexOf(Types::Intrinsic::INFER));

        if (!isMutable && isInitialized) {
            throw std::invalid_argument("Unable to assign to an initialized immutable reference");
        }

        if (Types::Pool.isStruct(type) && newValue->type == Types::indexOf(Types::Intrinsic::ARRAY)) {
            ArrayType& structValue = std::get<ArrayType>(this->value);
            ArrayType& assignValue = std::get<ArrayType>(newValue->value);
            if (structValue.size() != assignValue.size()) {
                std::stringstream ss;
                ss << "Unable to assign array of length " << assignValue.size() << " to struct with " << structValue.size() << " members";
                throw std::invalid_argument(ss.str());
            }

            for (i32 i = 0; i < structValue.size(); i++) {
                structValue[i]->assign(assignValue[i]);
            }
            isInitialized = true;
            return;
        }

        if ((type != Types::indexOf(Types::Intrinsic::INFER)) && (type != newValue->type)) {
            std::stringstream ss;
            ss << "Attempted to assign value of type " << Types::Pool.typeName(newValue->type) << " to reference of type " << Types::Pool.typeName(type);
            throw std::invalid_argument(ss.str());
        }

        type = newValue->type;
        isInitialized = true;
        this->value = newValue->value;
    }

    // TODO: add support for printing arity
    static Reference functionString;

    Reference* toString();

    bool isType() {
        return type == Types::indexOf(Types::Intrinsic::TYPE);
    }
};
