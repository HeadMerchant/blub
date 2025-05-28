#pragma once
#include <functional>
#include <vector>
#include "parser/parser.h"

enum class ValueType {
    FUNCTION,
    STRING,
    FLOAT,
    ARRAY,
    INT,
    NATIVE_FUNCTION
};

struct InterpreterValue;

typedef std::string StringType;
typedef std::vector<InterpreterValue*> ArrayType;
typedef float FloatType;
struct FunctionLiteral {
    Parser& parser;
    Encodings::FunctionLiteral function;
};
typedef FunctionLiteral FunctionType;
typedef int IntType;
using NativeFunction = std::function<InterpreterValue*(std::span<InterpreterValue*>)>;
// using NativeFunction = InterpreterValue* (*)(std::span<InterpreterValue*>);

struct InterpreterValue {
    public:
    ValueType type;
    private:
    void* value;

    public:
    InterpreterValue(StringType* string): type(ValueType::STRING), value(string) {}
    InterpreterValue(ArrayType* array): type(ValueType::ARRAY), value(array) {}
    InterpreterValue(FloatType num): type(ValueType::FLOAT), value(new float(num)) {}
    InterpreterValue(FunctionType* func): type(ValueType::FUNCTION), value(func) {}
    InterpreterValue(IntType num): type(ValueType::INT), value(new int(num)) {}
    // InterpreterValue(void* func): type(ValueType::NATIVE_FUNCTION), value(func) {}
    InterpreterValue(NativeFunction* func): type(ValueType::NATIVE_FUNCTION), value(func) {}

    InterpreterValue* add(InterpreterValue* right);

    InterpreterValue* sub(InterpreterValue* right);

    InterpreterValue* div(InterpreterValue* right);

    InterpreterValue* mul(InterpreterValue* right);

    StringType* string();
    ArrayType* array();
    FloatType floatVal();
    FunctionType* function();
    IntType intVal();
    NativeFunction* nativeFunction();

    // TODO: add support for printing arity
    static InterpreterValue functionString;

    InterpreterValue* toString();
};
