#include "value.h"
#include "types.h"
#include <iostream>
#include <cassert>
#include <sstream>
#include <stdexcept>
#include <string>

Reference Reference::functionString(new std::string("<user-defined function>"));
Reference nativeFunctionString(new std::string("<native function>"));
Reference trueString(new std::string("true"));
Reference falseString(new std::string("false"));

StringType Reference::string() {
    assert(type == Types::indexOf(Types::Intrinsic::STRING));
    return value._string;
}

ArrayType Reference::array() {
    assert(type == Types::indexOf(Types::Intrinsic::ARRAY));
    return value._array;
}

FloatType Reference::floatVal() {
    assert(type == Types::indexOf(Types::Intrinsic::FLOAT));
    return value._float;
}

FunctionType Reference::function() {
    assert(type == Types::indexOf(Types::Intrinsic::FUNCTION));
    return value._function;
}

IntType Reference::intVal() {
    assert(type == Types::indexOf(Types::Intrinsic::INT));
    return value._int;
}

NativeFunction Reference::nativeFunction() {
    assert(type == Types::indexOf(Types::Intrinsic::NATIVE_FUNCTION));
    return value._nativeFunction;
}

Reference* Reference::toString() {
    if(type == Types::indexOf(Types::Intrinsic::STRING)) {
        return this;
    }
    if(type == Types::indexOf(Types::Intrinsic::ARRAY)) {
        ArrayType array = this->array();
        std::stringstream ss;
        ss << "[";
        bool separate = false;
        for (Reference* child : *array) {
            if (separate) {
                ss << ", ";
            }
            ss << *(child->toString()->string());
            separate = true;
        }
        ss << "]";

        std::string value = ss.str();
        return new Reference(new std::string(value));
    }
    if(type == Types::indexOf(Types::Intrinsic::FLOAT)) {
        float rawValue = floatVal();
        std::cout << "Float value: " << rawValue;
        return new Reference (new std::string(std::to_string(rawValue)));
    }
    if(type == Types::indexOf(Types::Intrinsic::FUNCTION)) {
       return &Reference::functionString;
    }
    if(type == Types::indexOf(Types::Intrinsic::INT)) {
        auto rawValue = intVal();
        std::cout << "Int value: " << rawValue;
        return new Reference(new std::string(std::to_string(rawValue)));
    }
    if(type == Types::indexOf(Types::Intrinsic::NATIVE_FUNCTION)) {
        return &nativeFunctionString;
    }
    if(type == Types::indexOf(Types::Intrinsic::BOOL)) {
        return value._bool ? &trueString : &falseString;
    }

    std::stringstream ss;
    ss << "Undefined string conversion for value type: " << type.value;
    throw std::invalid_argument(ss.str());
}

std::tuple<float, float> unpackFloat(Reference* left, Reference* right) {
    bool bothFloat = (left->type == Types::indexOf(Types::Intrinsic::FLOAT)) && (right->type == Types::indexOf(Types::Intrinsic::FLOAT));
    if (!bothFloat) throw std::invalid_argument("Unable to perform binary operation on non-float operands");

    return std::make_tuple(left->floatVal(), right->floatVal());
}

Reference* Reference::add(Reference* right) {
    if (this->type == Types::indexOf(Types::Intrinsic::FUNCTION) || right->type == Types::indexOf(Types::Intrinsic::FUNCTION)) {
        throw std::invalid_argument("Can't add functions");
    }

    if (this->type == Types::indexOf(Types::Intrinsic::STRING)) {
        StringType val = new std::string(this->string()->append(*right->toString()->string()));
        return new Reference(val);
    }
 
    if (this->type == Types::indexOf(Types::Intrinsic::ARRAY) || right->type == Types::indexOf(Types::Intrinsic::ARRAY)) {
        if (this->type != right->type) {
            throw std::invalid_argument("Can't add arrays to non-arrays");
        }

        ArrayType newArray = new std::vector<Reference*>();
        for (auto item : *this->value._array) {
            newArray->push_back(item);
        }
        for (auto item : *right->value._array) {
            newArray->push_back(item);
        }

        return new Reference(newArray);
    }

    if (type == Types::indexOf(Types::Intrinsic::INT)) {
        IntType left = intVal();
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left + right->intVal()); 
        }
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left + right->floatVal());
        }
    }

    if (type == Types::indexOf(Types::Intrinsic::FLOAT)) {
        FloatType left = floatVal();
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left + right->floatVal());
        }
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left + right->intVal());
        }
    }
    
    // const auto [a, b] = unpackFloat(this, right);
    // return new InterpreterValue(a + b);
    throw std::invalid_argument("Undefined addition for these two types");
}

Reference* Reference::sub(Reference* right) {
    if (type == Types::indexOf(Types::Intrinsic::INT)) {
        IntType left = intVal();
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left - right->intVal()); 
        }
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left - right->floatVal());
        }
    }

    if (type == Types::indexOf(Types::Intrinsic::FLOAT)) {
        FloatType left = floatVal();
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left - right->floatVal());
        }
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left - right->intVal());
        }
    }
    
    throw std::invalid_argument("Undefined subtraction for these two types");
}

Reference* Reference::div(Reference* right) {
    if (type == Types::indexOf(Types::Intrinsic::INT)) {
        IntType left = intVal();
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left / right->intVal()); 
        }
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left / right->floatVal());
        }
    }

    if (type == Types::indexOf(Types::Intrinsic::FLOAT)) {
        FloatType left = floatVal();
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left / right->floatVal());
        }
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left / right->intVal());
        }
    }
    
    throw std::invalid_argument("Undefined division for these two types");
}

Reference* Reference::mul(Reference* right) {
    if (type == Types::indexOf(Types::Intrinsic::INT)) {
        IntType left = intVal();
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left * right->intVal()); 
        }
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left * right->floatVal());
        }
    }

    if (type == Types::indexOf(Types::Intrinsic::FLOAT)) {
        FloatType left = floatVal();
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left * right->floatVal());
        }
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left * right->intVal());
        }
    }
    
    throw std::invalid_argument("Undefined multiplication for these two types");
}
