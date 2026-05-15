#include "common.h"
#include "llvmcomp.h"

Compiler::ArithmeticOperator
  Compiler::multOp{TokenType::Mult, "mul", "multiply", false, false};

Logger Compiler::log(LogLevel::Compile);

FieldMap Compiler::defaultFields;
