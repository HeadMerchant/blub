#include "value.h"
#include "types.h"
#include <cassert>
#include <sstream>
#include <stdexcept>
#include <string>

Reference Reference::functionString(new std::string("<user-defined function>"));
Reference nativeFunctionString(std::string("<native function>"));
Reference trueString(std::string("true"));
Reference falseString(std::string("false"));
Reference* Reference::True = new Reference(true);
Reference* Reference::False= new Reference(false);

Reference* Reference::toString() {
    if(type == Types::indexOf(Types::Intrinsic::STRING)) {
        return this;
    }
    if(type == Types::indexOf(Types::Intrinsic::ARRAY)) {
        ArrayType array = std::get<ArrayType>(this->value);
        std::stringstream ss;
        ss << "[";
        bool separate = false;
        for (Reference* child : array) {
            if (separate) {
                ss << ", ";
            }
            ss << std::get<StringType>(child->toString()->value);
            separate = true;
        }
        ss << "]";

        std::string value = ss.str();
        return new Reference(std::move(std::string(value)));
    }
    if(type == Types::indexOf(Types::Intrinsic::FLOAT)) {
        FloatType rawValue = std::get<FloatType>(value);
        std::stringstream ss;
        ss << rawValue;
        return new Reference (std::move(ss.str()));
    }
    if(type == Types::indexOf(Types::Intrinsic::FUNCTION)) {
       return &Reference::functionString;
    }
    if(type == Types::indexOf(Types::Intrinsic::INT)) {
        IntType rawValue = std::get<IntType>(value);
        return new Reference(std::move(std::to_string(rawValue)));
    }
    if(type == Types::indexOf(Types::Intrinsic::NATIVE_FUNCTION)) {
        return &nativeFunctionString;
    }
    if(type == Types::indexOf(Types::Intrinsic::BOOL)) {
        return std::get<bool>(value) ? &trueString : &falseString;
    }

    std::stringstream ss;
    ss << "Undefined string conversion for value type: " << type.value;
    throw std::invalid_argument(ss.str());
}

std::tuple<float, float> unpackFloat(Reference* left, Reference* right) {
    bool bothFloat = (left->type == Types::indexOf(Types::Intrinsic::FLOAT)) && (right->type == Types::indexOf(Types::Intrinsic::FLOAT));
    if (!bothFloat) throw std::invalid_argument("Unable to perform binary operation on non-float operands");

    return std::make_tuple(std::get<FloatType>(left->value), std::get<FloatType>(right->value));
}

Reference* Reference::add(Reference* right) {
    if (this->type == Types::indexOf(Types::Intrinsic::FUNCTION) || right->type == Types::indexOf(Types::Intrinsic::FUNCTION)) {
        throw std::invalid_argument("Can't add functions");
    }

    if (this->type == Types::indexOf(Types::Intrinsic::STRING)) {
        StringType val = std::string(std::get<StringType>(this->value).append(std::get<StringType>(right->toString()->value)));
        return new Reference(std::move(val));
    }
 
    if (this->type == Types::indexOf(Types::Intrinsic::ARRAY) || right->type == Types::indexOf(Types::Intrinsic::ARRAY)) {
        if (this->type != right->type) {
            throw std::invalid_argument("Can't add arrays to non-arrays");
        }

        ArrayType newArray;
        for (auto item : std::get<ArrayType>(this->value)) {
            newArray.push_back(item);
        }
        for (auto item : std::get<ArrayType>(right->value)) {
            newArray.push_back(item);
        }

        return new Reference(std::move(newArray));
    }

    if (type == Types::indexOf(Types::Intrinsic::INT)) {
        IntType left = std::get<IntType>(value);
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left + std::get<IntType>(right->value)); 
        }
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left + std::get<FloatType>(right->value));
        }
    }

    if (type == Types::indexOf(Types::Intrinsic::FLOAT)) {
        FloatType left = std::get<FloatType>(value);
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left + std::get<FloatType>(right->value));
        }
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left + std::get<IntType>(right->value));
        }
    }
    
    // const auto [a, b] = unpackFloat(this, right);
    // return new InterpreterValue(a + b);
    throw std::invalid_argument("Undefined addition for these two types");
}

Reference* Reference::sub(Reference* right) {
    if (type == Types::indexOf(Types::Intrinsic::INT)) {
        IntType left = std::get<IntType>(value);
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left - std::get<IntType>(right->value)); 
        }
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left - std::get<FloatType>(right->value));
        }
    }

    if (type == Types::indexOf(Types::Intrinsic::FLOAT)) {
        FloatType left = std::get<FloatType>(value);
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left - std::get<FloatType>(right->value));
        }
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left - std::get<IntType>(right->value));
        }
    }
    
    throw std::invalid_argument("Undefined subtraction for these two types");
}

Reference* Reference::div(Reference* right) {
    if (type == Types::indexOf(Types::Intrinsic::INT)) {
        IntType left = std::get<IntType>(value);
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left / std::get<IntType>(right->value)); 
        }
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left / std::get<FloatType>(right->value));
        }
    }

    if (type == Types::indexOf(Types::Intrinsic::FLOAT)) {
        FloatType left = std::get<FloatType>(value);
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left / std::get<FloatType>(right->value));
        }
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left / std::get<IntType>(right->value));
        }
    }
    
    throw std::invalid_argument("Undefined division for these two types");
}

Reference* Reference::mul(Reference* right) {
    if (type == Types::indexOf(Types::Intrinsic::INT)) {
        IntType left = std::get<IntType>(value);
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left * std::get<IntType>(right->value)); 
        }
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left * std::get<FloatType>(right->value));
        }
    }

    if (type == Types::indexOf(Types::Intrinsic::FLOAT)) {
        FloatType left = std::get<FloatType>(value);
        if (right->type == Types::indexOf(Types::Intrinsic::FLOAT)) {
            return new Reference(left * std::get<FloatType>(right->value));
        }
        if (right->type == Types::indexOf(Types::Intrinsic::INT)) {
            return new Reference(left * std::get<IntType>(right->value));
        }
    }
    
    throw std::invalid_argument("Undefined multiplication for these two types");
}
