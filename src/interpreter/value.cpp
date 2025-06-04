#include "value.h"
#include <iostream>
#include <cassert>
#include <sstream>
#include <stdexcept>
#include <string>

Reference Reference::functionString(new std::string("<user-defined function>"));
Reference nativeFunctionString(new std::string("<native function>"));

StringType Reference::string() {
    assert(type == ValueType::STRING);
    return value._string;
}

ArrayType Reference::array() {
    assert(type == ValueType::ARRAY);
    return value._array;
}

FloatType Reference::floatVal() {
    assert(type == ValueType::FLOAT);
    return value._float;
}

FunctionType Reference::function() {
    assert(type == ValueType::FUNCTION);
    return value._function;
}

IntType Reference::intVal() {
    assert(type == ValueType::INT);
    return value._int;
}

NativeFunction Reference::nativeFunction() {
    assert(type == ValueType::NATIVE_FUNCTION);
    return value._nativeFunction;
}

Reference* Reference::toString() {
    switch (type) {
        case ValueType::STRING: {
            return this;
        }
        case ValueType::ARRAY: {
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
        case ValueType::FLOAT: {
            float rawValue = floatVal();
            std::cout << "Float value: " << rawValue;
            return new Reference (new std::string(std::to_string(rawValue)));
        }
        case ValueType::FUNCTION: {
           return &Reference::functionString;
        }
        case ValueType::INT: {
            auto rawValue = intVal();
            std::cout << "Int value: " << rawValue;
            return new Reference(new std::string(std::to_string(rawValue)));
        }
        case ValueType::NATIVE_FUNCTION: {
            return &nativeFunctionString;
        }
        case ValueType::BOOL:
        case ValueType::STRUCT:
        case ValueType::ENUM:
        case ValueType::UNION:
        case ValueType::REFERENCE:
        case ValueType::INTRINSIC_TYPE:
        case ValueType::USER_TYPE:
        case ValueType::INFER:
        case ValueType::TRAIT: {
            std::stringstream ss;
            ss << "Undefined string conversion for value type: " << (i32) type;
            throw std::invalid_argument(ss.str());
        }
    }
    throw std::invalid_argument("Unable to convert value to string");
}

std::tuple<float, float> unpackFloat(Reference* left, Reference* right) {
    bool bothFloat = (left->type == ValueType::FLOAT) && (right->type == ValueType::FLOAT);
    if (!bothFloat) throw std::invalid_argument("Unable to perform binary operation on non-float operands");

    return std::make_tuple(left->floatVal(), right->floatVal());
}

Reference* Reference::add(Reference* right) {
    if (this->type == ValueType::FUNCTION || right->type == ValueType::FUNCTION) {
        throw std::invalid_argument("Can't add functions");
    }

    if (this->type == ValueType::STRING) {
        StringType val = new std::string(this->string()->append(*right->toString()->string()));
        return new Reference(val);
    }
 
    if (this->type == ValueType::ARRAY || right->type == ValueType::ARRAY) {
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

    if (type == ValueType::INT) {
        IntType left = intVal();
        if (right->type == ValueType::INT) {
            return new Reference(left + right->intVal()); 
        }
        if (right->type == ValueType::FLOAT) {
            return new Reference(left + right->floatVal());
        }
    }

    if (type == ValueType::FLOAT) {
        FloatType left = floatVal();
        if (right->type == ValueType::FLOAT) {
            return new Reference(left + right->floatVal());
        }
        if (right->type == ValueType::INT) {
            return new Reference(left + right->intVal());
        }
    }
    
    // const auto [a, b] = unpackFloat(this, right);
    // return new InterpreterValue(a + b);
    throw std::invalid_argument("Undefined addition for these two types");
}

Reference* Reference::sub(Reference* right) {
    if (type == ValueType::INT) {
        IntType left = intVal();
        if (right->type == ValueType::INT) {
            return new Reference(left - right->intVal()); 
        }
        if (right->type == ValueType::FLOAT) {
            return new Reference(left - right->floatVal());
        }
    }

    if (type == ValueType::FLOAT) {
        FloatType left = floatVal();
        if (right->type == ValueType::FLOAT) {
            return new Reference(left - right->floatVal());
        }
        if (right->type == ValueType::INT) {
            return new Reference(left - right->intVal());
        }
    }
    
    throw std::invalid_argument("Undefined subtraction for these two types");
}

Reference* Reference::div(Reference* right) {
    if (type == ValueType::INT) {
        IntType left = intVal();
        if (right->type == ValueType::INT) {
            return new Reference(left / right->intVal()); 
        }
        if (right->type == ValueType::FLOAT) {
            return new Reference(left / right->floatVal());
        }
    }

    if (type == ValueType::FLOAT) {
        FloatType left = floatVal();
        if (right->type == ValueType::FLOAT) {
            return new Reference(left / right->floatVal());
        }
        if (right->type == ValueType::INT) {
            return new Reference(left / right->intVal());
        }
    }
    
    throw std::invalid_argument("Undefined division for these two types");
}

Reference* Reference::mul(Reference* right) {
    if (type == ValueType::INT) {
        IntType left = intVal();
        if (right->type == ValueType::INT) {
            return new Reference(left * right->intVal()); 
        }
        if (right->type == ValueType::FLOAT) {
            return new Reference(left * right->floatVal());
        }
    }

    if (type == ValueType::FLOAT) {
        FloatType left = floatVal();
        if (right->type == ValueType::FLOAT) {
            return new Reference(left * right->floatVal());
        }
        if (right->type == ValueType::INT) {
            return new Reference(left * right->intVal());
        }
    }
    
    throw std::invalid_argument("Undefined multiplication for these two types");
}
