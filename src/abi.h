#pragma once
#include "compilercontext.h"
#include "types.h"
#include "value.h"

// Returns how many llvm registers are used for parameters
struct DeclarationResult {
  std::string aggregateReturnTypeName;
  u32 lastParameterRegister;
};
DeclarationResult declareParamRegisters(std::ostream& outputFile, Function function);

void loadParameterRegisters(OutContext& ctx, FunctionType function, span<Identifier> paramNames);

// Assumes all args have been loaded into llvm registers (i.e. no lvalues)
u32 callAbiFunctionWithArgs(OutContext& ctx, Function function, span<Reference> args);
