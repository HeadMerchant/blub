#include "common.h"
#include "llvmcomp.h"

Compiler::ArithmeticOperator
  Compiler::multOp{"mul", "multiply", TokenType::Mult, false, false};

Logger Compiler::log(LogLevel::Compile);

FieldMap Compiler::defaultFields;
