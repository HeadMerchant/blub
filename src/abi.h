#pragma once
#include "compilercontext.h"
#include "types.h"
#include "value.h"

enum class AbiPassKind {
  Void,
  Direct,
  Coerce,
  Memory,
};

struct AbiComponent {
  u32 offset = 0;
  u32 byteSize = 0;
  std::string llvmType;
};

struct AbiType {
  TypeIndex type;
  AbiPassKind kind = AbiPassKind::Void;
  std::vector<AbiComponent> components;
  std::string extensionAttribute;

  bool isVoid() const { return kind == AbiPassKind::Void; }
  bool isDirect() const { return kind == AbiPassKind::Direct; }
  bool isCoerce() const { return kind == AbiPassKind::Coerce; }
  bool isMemory() const { return kind == AbiPassKind::Memory; }
  std::string returnTypeName() const;
  u32 parameterCount() const;
};

struct DeclarationResult {
  AbiType returnAbi;
  u32 entryLabel = 0;
};

AbiType classifySystemVAbi(TypeIndex type);

DeclarationResult declareParamRegisters(
  std::ostream& outputFile,
  Function function
);

void loadParameterRegisters(
  OutContext& ctx,
  FunctionType function,
  span<Identifier> paramNames
);

// Assumes all args have been loaded into LLVM registers (i.e. no lvalues).
u32 callAbiFunctionWithArgs(
  OutContext& ctx,
  Function function,
  span<Reference> args
);

void emitSystemVReturn(
  OutContext& ctx,
  const AbiType& returnAbi,
  Reference value
);
