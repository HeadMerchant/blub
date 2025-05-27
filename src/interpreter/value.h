#pragma once
#include <stdexcept>
#include <vector>
#include "parser.h"

enum class ValueType {
    FUNCTION,
    STRING,
    FLOAT,
    ARRAY,
    INT
};

struct InterpreterValue;

typedef std::string StringType;
typedef std::vector<InterpreterValue*> ArrayType;
typedef float FloatType;
typedef ASTNode* FunctionType;
typedef int IntType;

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

    InterpreterValue* add(InterpreterValue* right);

    InterpreterValue* sub(InterpreterValue* right);

    InterpreterValue* div(InterpreterValue* right);

    InterpreterValue* mul(InterpreterValue* right);

    StringType* string();
    ArrayType* array();
    FloatType floatVal();
    FunctionType* function();
    IntType intVal();

    // TODO: add support for printing arity
    static InterpreterValue functionString;

    InterpreterValue* toString();
};
