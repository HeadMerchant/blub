#pragma once
#include "compilercontext.h"
#include "types.h"
#include "value.h"

// Returns whether a parameter was added or not
u32 declareParamRegisters(OutContext& ctx, Function function);

void loadParameterRegisters(OutContext& ctx, FunctionType function, span<Identifier> paramNames);

void passArgumentRegisters(OutContext& ctx, Function function);
