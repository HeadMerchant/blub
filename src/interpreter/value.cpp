#include "value.h"
#include <cassert>
#include <sstream>
#include <stdexcept>
#include <string>

InterpreterValue InterpreterValue::functionString(new std::string("<user-defined function>"));
InterpreterValue nativeFunctionString(new std::string("<native function>"));

StringType* InterpreterValue::string() {
    assert(type == ValueType::STRING);
    return (StringType*) value;
}

ArrayType* InterpreterValue::array() {
    assert(type == ValueType::ARRAY);
    return (ArrayType*) value;
}

FloatType InterpreterValue::floatVal() {
    assert(type == ValueType::FLOAT);
    return *(FloatType*) value;
}

FunctionType* InterpreterValue::function() {
    assert(type == ValueType::FUNCTION);
    return (FunctionType*) value;
}

IntType InterpreterValue::intVal() {
    assert(type == ValueType::INT);
    return *(IntType*) value;
}

NativeFunction* InterpreterValue::nativeFunction() {
    assert(type == ValueType::NATIVE_FUNCTION);
    return (NativeFunction*) value;
}

InterpreterValue* InterpreterValue::toString() {
    switch (type) {
        case ValueType::STRING: {
            return this;
        }
        case ValueType::ARRAY: {
            ArrayType array = *this->array();
            std::stringstream ss;
            ss << "[";
            bool separate = false;
            for (InterpreterValue* child : array) {
                if (separate) {
                    ss << ", ";
                }
                ss << *(child->toString()->string());
                separate = true;
            }
            ss << "]";

            std::string value = ss.str();
            return new InterpreterValue(new std::string(value));
        } 
        case ValueType::FLOAT: {
            float rawValue = floatVal();
            return new InterpreterValue (new std::string(std::to_string(rawValue)));
        }
        case ValueType::FUNCTION: {
           return &InterpreterValue::functionString;
        }
        case ValueType::INT: {
            auto rawValue = intVal();
            return new InterpreterValue(new std::string(std::to_string(rawValue)));
        }
        case ValueType::NATIVE_FUNCTION: {
            return &nativeFunctionString;
        }
    }
    throw std::invalid_argument("Unable to convert value to string");
}

std::tuple<float, float> unpackFloat(InterpreterValue* left, InterpreterValue* right) {
    bool bothFloat = (left->type == ValueType::FLOAT) && (right->type == ValueType::FLOAT);
    if (!bothFloat) throw std::invalid_argument("Unable to perform binary operation on non-float operands");

    return std::make_tuple(left->floatVal(), right->floatVal());
}

InterpreterValue* InterpreterValue::add(InterpreterValue* right) {
    if (this->type == ValueType::FUNCTION || right->type == ValueType::FUNCTION) {
        throw std::invalid_argument("Can't add functions");
    }

    if (this->type == ValueType::STRING) {
        StringType val = this->string()->append(*right->toString()->string());
        return new InterpreterValue(new StringType(val));
    }
 
    if (this->type == ValueType::ARRAY || right->type == ValueType::ARRAY) {
        if (this->type != right->type) {
            throw std::invalid_argument("Can't add arrays to non-arrays");
        }

        ArrayType* newArray = new ArrayType();
        for (auto item : *(ArrayType*) this->value) {
            newArray->push_back(item);
        }
        for (auto item : *(ArrayType*) right->value) {
            newArray->push_back(item);
        }

        return new InterpreterValue (newArray);
    }

    if (type == ValueType::INT) {
        IntType left = intVal();
        if (right->type == ValueType::INT) {
            return new InterpreterValue(left + right->intVal()); 
        }
        if (right->type == ValueType::FLOAT) {
            return new InterpreterValue(left + right->floatVal());
        }
    }

    if (type == ValueType::FLOAT) {
        FloatType left = floatVal();
        if (right->type == ValueType::FLOAT) {
            return new InterpreterValue(left + right->floatVal());
        }
        if (right->type == ValueType::INT) {
            return new InterpreterValue(left + right->intVal());
        }
    }
    
    // const auto [a, b] = unpackFloat(this, right);
    // return new InterpreterValue(a + b);
    throw std::invalid_argument("Undefined addition for these two types");
}

InterpreterValue* InterpreterValue::sub(InterpreterValue* right) {
    if (type == ValueType::INT) {
        IntType left = intVal();
        if (right->type == ValueType::INT) {
            return new InterpreterValue(left - right->intVal()); 
        }
        if (right->type == ValueType::FLOAT) {
            return new InterpreterValue(left - right->floatVal());
        }
    }

    if (type == ValueType::FLOAT) {
        FloatType left = floatVal();
        if (right->type == ValueType::FLOAT) {
            return new InterpreterValue(left - right->floatVal());
        }
        if (right->type == ValueType::INT) {
            return new InterpreterValue(left - right->intVal());
        }
    }
    
    throw std::invalid_argument("Undefined subtraction for these two types");
}

InterpreterValue* InterpreterValue::div(InterpreterValue* right) {
    if (type == ValueType::INT) {
        IntType left = intVal();
        if (right->type == ValueType::INT) {
            return new InterpreterValue(left / right->intVal()); 
        }
        if (right->type == ValueType::FLOAT) {
            return new InterpreterValue(left / right->floatVal());
        }
    }

    if (type == ValueType::FLOAT) {
        FloatType left = floatVal();
        if (right->type == ValueType::FLOAT) {
            return new InterpreterValue(left / right->floatVal());
        }
        if (right->type == ValueType::INT) {
            return new InterpreterValue(left / right->intVal());
        }
    }
    
    throw std::invalid_argument("Undefined division for these two types");
}

InterpreterValue* InterpreterValue::mul(InterpreterValue* right) {
    if (type == ValueType::INT) {
        IntType left = intVal();
        if (right->type == ValueType::INT) {
            return new InterpreterValue(left * right->intVal()); 
        }
        if (right->type == ValueType::FLOAT) {
            return new InterpreterValue(left * right->floatVal());
        }
    }

    if (type == ValueType::FLOAT) {
        FloatType left = floatVal();
        if (right->type == ValueType::FLOAT) {
            return new InterpreterValue(left * right->floatVal());
        }
        if (right->type == ValueType::INT) {
            return new InterpreterValue(left * right->intVal());
        }
    }
    
    throw std::invalid_argument("Undefined multiplication for these two types");
}
