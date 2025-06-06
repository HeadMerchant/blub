#pragma once
#include <functional>
#include <stdexcept>
#include <vector>
#include "parser.h"
#include <sstream>
#include "types.h"
#include <cassert>

struct Reference;

using StringType = std::string*;
using ArrayType = std::vector<Reference*>*;
using FloatType = float;

class Environment;
struct FunctionLiteral {
    Parser& parser;
    Environment* declarationEnvironment;
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
        FunctionType _function;
        Trait* _trait;
        Types::TypeIndex _type;
        Reference* _reference;
    } value;

    Types::TypeIndex type;
    bool isMutable = true;
    bool isInitialized = false;
    static Reference* True;
    static Reference* False;
    
    Reference(Types::TypeIndex type): type(type), isInitialized(false) {}
    Reference(Types::Intrinsic type): type(Types::indexOf(type)), isInitialized(false) {}
    Reference(StringType string): type(Types::indexOf(Types::Intrinsic::STRING)), value({._string = string}) {}
    Reference(ArrayType array): type(Types::indexOf(Types::Intrinsic::ARRAY)), value({._array = array}) {}
    Reference(FloatType num): type(Types::indexOf(Types::Intrinsic::FLOAT)), value({._float = num}) {}
    Reference(FunctionType func): type(Types::indexOf(Types::Intrinsic::FUNCTION)), value({._function = func}) {}
    Reference(IntType num): type(Types::indexOf(Types::Intrinsic::INT)), value({._int = num}) {}
    Reference(NativeFunction func): type(Types::indexOf(Types::Intrinsic::NATIVE_FUNCTION)), value({._nativeFunction = func}) {}
    private:
    Reference(bool value): type(Types::indexOf(Types::Intrinsic::BOOL)), isMutable(false), isInitialized(true), value({._bool = value}) {}

    public:
        
    static Reference* of(bool boolean) {
        return boolean ? True : False;
    }

    static Reference* ofType(Reference* type) {
        assert(type->isType());

        return new Reference(type->value._type);
    }

    static Reference* toType(Types::TypeIndex index) {
        auto ref = new Reference(Types::Intrinsic::TYPE);
        ref->value._type = index;
        ref->isInitialized = true;
        ref->isMutable = false;
        return ref;
    }

    static Reference* toType(Types::Intrinsic index) {
        auto ref = new Reference(Types::Intrinsic::TYPE);
        ref->value._type = Types::indexOf(index);
        ref->isInitialized = true;
        ref->isMutable = false;
        return ref;
    }

    static Reference* pointerTo(Reference* value) {
        Types::TypeIndex pointerType = Types::Pool.pointerTo(value->type);
        assert(Types::Pool[pointerType].definition == value->type);

        auto ref = new Reference(pointerType);
        ref->value._reference = value;

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

    StringType string();
    ArrayType array();
    FloatType floatVal();
    FunctionType function();
    IntType intVal();
    NativeFunction nativeFunction();

    void assign(Reference* value) {
        assert(value->type != Types::indexOf(Types::Intrinsic::INFER));

        if (!isMutable && isInitialized) {
            throw std::invalid_argument("Unable to assign to an initialized immutable reference");
        }

        if ((type != Types::indexOf(Types::Intrinsic::INFER)) && (type != value->type)) {
            std::stringstream ss;
            ss << "Attempted to assign value of type " << Types::Pool.typeName(value->type) << " to reference of type " << Types::Pool.typeName(type);
            throw std::invalid_argument(ss.str());
        }

        type = value->type;
        isInitialized = true;
        this->value = value->value;
    }

    // TODO: add support for printing arity
    static Reference functionString;

    Reference* toString();

    bool isType() {
        return type == Types::indexOf(Types::Intrinsic::TYPE);
    }
};
