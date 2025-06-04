#pragma once
#include <cmath>
#include <functional>
#include <stdexcept>
#include <vector>
#include "parser/parser.h"
#include <cassert>

enum class ValueType {
    BOOL,
    INT,
    FLOAT,
    ARRAY,
    STRING,
    NATIVE_FUNCTION,
    REFERENCE,
    FUNCTION,

    // Types
    TRAIT,
    STRUCT,
    ENUM,
    UNION,
    INTRINSIC_TYPE,
    USER_TYPE,
    POINTER_TO,

    // Wait until initialization to set type
    INFER
};

struct Reference;

using StringType = std::string*;
using ArrayType = std::vector<Reference*>*;
using FloatType = float;
struct FunctionLiteral {
    Parser& parser;
    Encodings::FunctionLiteral function;
};
using FunctionType = FunctionLiteral*;
using IntType = int;
using NativeFunction = std::function<Reference*(std::span<Reference*>)>*;
// using NativeFunction = InterpreterValue* (*)(std::span<InterpreterValue*>);

struct Struct {};
struct Enum {};
struct Union {};
struct Trait {};
// TODO: add type interning
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
    union {
        bool _bool;
        IntType _int;
        FloatType _float;
        ArrayType _array;
        StringType _string;
        NativeFunction _nativeFunction;
        Struct* _struct;
        Enum* _enum;
        Union* _union;
        FunctionType _function;
        Trait* _trait;
        UserTypeValue _userType;
        ValueType _intrinsicType;
    } value;

    ValueType type;
    ValueType typeParam;
    bool isMutable = true;
    bool isInitialized = false;
    Reference(ValueType type): type(type), isInitialized(false) {}
    Reference(StringType string): type(ValueType::STRING), value({._string = string}) {}
    Reference(ArrayType array): type(ValueType::ARRAY), value({._array = array}) {}
    Reference(FloatType num): type(ValueType::FLOAT), value({._float = num}) {}
    Reference(FunctionType func): type(ValueType::FUNCTION), value({._function = func}) {}
    Reference(IntType num): type(ValueType::INT), value({._int = num}) {}
    Reference(NativeFunction func): type(ValueType::NATIVE_FUNCTION), value({._nativeFunction = func}) {}

    static Reference* intrinsicType(ValueType type) {
        auto ref = new Reference(ValueType::INTRINSIC_TYPE);
        ref->value._intrinsicType = type;
        ref->isInitialized = true;
        ref->isMutable = false;
        return ref;
    }

    static Reference* ofType(Reference* type) {
        assert(type->isType());
        Reference* ref;
        if (type->type == ValueType::INTRINSIC_TYPE) {
            ref = new Reference(type->type);
        } else {
            ref = new Reference(ValueType::USER_TYPE);
            ref->value._userType = {
                .type = type
            };
        }

        return ref;
    }

    Reference* add(Reference* right);

    Reference* sub(Reference* right);

    Reference* div(Reference* right);

    Reference* mul(Reference* right);

    StringType string();
    ArrayType array();
    FloatType floatVal();
    FunctionType function();
    IntType intVal();
    NativeFunction nativeFunction();

    bool typesEquivalent(Reference* value) {
        if (this->type != value->type) {
            return false;
        }

        if (this->type == ValueType::POINTER_TO) {
            return this->value._userType.type == value->value._userType.type;
        }


    }
    
    void assign(Reference* value) {
        assert(value->type != ValueType::INFER);

        if (!isMutable && isInitialized) {
            throw std::invalid_argument("Unable to assign to an initialized immutable reference");
        }

        if ((type != ValueType::INFER) && (type != value->type)) {
            throw std::invalid_argument("Non-matching types in assignment");
        }

        if () {
            
        }

        type = value->type;
        isInitialized = true;
        this->value = value->value;
    }

    // TODO: add support for printing arity
    static Reference functionString;

    Reference* toString();

    bool isType() {
        switch(type) {
            case ValueType::TRAIT:
            case ValueType::STRUCT:
            case ValueType::ENUM:
            case ValueType::UNION:
            case ValueType::INTRINSIC_TYPE:
            case ValueType::USER_TYPE:
            case ValueType::POINTER_TO:
                return true;
            default:
                return false;
        }
    }
};
