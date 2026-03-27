#include "abi.h"
#include "common.h"
#include "compilercontext.h"
#include "fmt/base.h"
#include "types.h"
#include "value.h"

void declareParameterRegisters(OutContext& ctx, TypeIndex typeIndex, RegisterAssignment registers, u8& byteIndex, u32& registerIndex) {
  auto sizing = Pool().getSizing(typeIndex);

  if (sizing.byteSize == 0) return;
  if (registers.get(byteIndex) == RegisterType::Memory) {
    fmt::print(ctx.outputFile, "ptr noundef byval({}) align {}", LlvmName(typeIndex), sizing.alignment.byteAlignment());
    byteIndex += sizing.byteSize;
    registerIndex++;
    return;
  }
  if (registers.allInt()) {
    ctx.outputFile << TypeName(typeIndex);
    byteIndex += sizing.byteSize;
    registerIndex++;
    return;
  }

  auto type = Pool().getType(typeIndex);
  std::visit(
    overloaded{
      [&]<IntRegister T>(T x) {
        ctx.outputFile << LlvmName(typeIndex);
        byteIndex += Pool().getSizing(typeIndex).byteSize;
        registerIndex++;
      },
      [&](Float x) {
        if (x.precision != Float::f32) {
          TODO("Declaring non-f32 floats in parameters");
        }
        auto byteSize = x.byteSize();
        if (registers.get(byteIndex) == RegisterType::Float) {
          if (byteIndex / 8 == 0) {
            registerIndex++;
            if (registers.get(byteIndex + byteSize) == RegisterType::Float) {
              fmt::print(ctx.outputFile, "<2 x {}>", LlvmName(typeIndex));
            } else {
              ctx.outputFile << LlvmName(typeIndex);
            }
          } else if (byteIndex / 8 != 0 && registers.get(byteIndex - byteSize) != RegisterType::Float) {
            ctx.outputFile << LlvmName(typeIndex);
            registerIndex++;
          }
        } else {
          ctx.outputFile << LlvmName(typeIndex);
          registerIndex++;
        }
        byteIndex += byteSize;
      },
      [&](StructIndex x) {
        Struct& structDef = Pool().getStruct(x);

        auto prevRegister = registerIndex;
        auto fieldIndex = 0;
        ctx.outputFile << "{";
        for (auto fieldType : structDef.fieldTypes) {
          if (prevRegister != registerIndex) {
            ctx.outputFile << ", ";
            fieldIndex++;
          }
          prevRegister = registerIndex;
          declareParameterRegisters(ctx, fieldType, registers, byteIndex, registerIndex);
        }
        ctx.outputFile << "}";
      },
      [&](TupleIndex x) { TODO("Declaring parameters for tuples"); },
      [&](SizedArray x) {
        ctx.outputFile << "{";
        auto prevRegister = registerIndex;
        for (u32 i = 0; i < x.length; i++) {
          if (prevRegister != registerIndex) {
            ctx.outputFile << ", ";
          }
          prevRegister = registerIndex;
          declareParameterRegisters(ctx, x.dereferencedType, registers, byteIndex, registerIndex);
        }
        ctx.outputFile << "}";
      },
      [&](EnumIndex x) { declareParameterRegisters(ctx, Pool().getEnum(x).rawType, registers, byteIndex, registerIndex); },
      [&](auto x) { TODO("Can't create args"); },
    },
    type
  );
}

u32 declareParameterRegisters(OutContext& ctx, TypeIndex typeIndex) {
  RegisterAssignment registers;
  Pool().registerStorage(typeIndex, registers);
  u8 byteIndex = 0;
  u32 registerIndex = 0;
  declareParameterRegisters(ctx, typeIndex, registers, byteIndex, registerIndex);
  return registerIndex;
}

// Assume that the caller has put declare/define first
u32 declareParamRegisters(OutContext& ctx, Function function) {
  auto returnType = function.type.returnType;
  RegisterAssignment returnRegisters;
  Pool().registerStorage(returnType, returnRegisters);

  if (returnRegisters.isMemory() || returnRegisters.isVoid()) {
    ctx.outputFile << "void";
  } else {
    declareParameterRegisters(ctx, returnType);
  }

  auto registersUsed = 0;
  bool needsComma = false;
  fmt::print(ctx.outputFile, " {}(", function.globalName);
  if (returnRegisters.isMemory()) {
    fmt::print(ctx.outputFile, "ptr sret({}) align {}", LlvmName(returnType), Pool().getSizing(returnType).alignment.byteAlignment());
    needsComma = true;
    registersUsed += 1;
  }

  for (auto paramType : Pool().tupleElements(function.type.parameters)) {
    if (needsComma) {
      ctx.outputFile << ", ";
    }
    auto newRegisters = declareParameterRegisters(ctx, paramType);
    needsComma = (bool)newRegisters;
    registersUsed += newRegisters;
  }

  ctx.outputFile << ")";
  return registersUsed;
}

void loadParameterRegisters(OutContext& ctx, TypeIndex typeIndex, RegisterName stackPointer, RegisterAssignment registers, u8& byteIndex, u32& registerIndex) {
  auto sizing = Pool().getSizing(typeIndex);
  if (sizing.byteSize == 0) return;
  if (registers.get(byteIndex) == RegisterType::Memory) {
    fmt::println(ctx.outputFile, "call void @llvm.memcpy.p0.p0.i8(ptr %{}, ptr %{}, i64 {}, i1 false)", stackPointer, sizing.byteSize, registerIndex);
    // High-key byteIndex doesn't matter
    byteIndex += sizing.byteSize;
    registerIndex++;
    return;
  }
  if (registers.allInt()) {
    fmt::println(ctx.outputFile, "store {} %{}, ptr %{}", LlvmName(typeIndex), registerIndex, registerIndex);
    byteIndex += sizing.byteSize;
    registerIndex++;
    return;
  }
  auto type = Pool().getType(typeIndex);
  std::visit(
    overloaded{
      [&]<IntRegister T>(T x) {
        fmt::println(ctx.outputFile, "store {} %{}, ptr %{}", LlvmName(typeIndex), registerIndex, stackPointer);
        byteIndex += Pool().getSizing(typeIndex).byteSize;
        registerIndex++;
      },
      [&](Float x) {
        if (x.precision != Float::f32) {
          TODO("Loading non-f32 floats from parameters");
        }
        auto byteSize = x.byteSize();
        if (registers.get(byteIndex) == RegisterType::Float) {
          if (byteIndex / 8 == 0 && registers.get(byteIndex + byteSize) == RegisterType::Float) {
            fmt::println(ctx.outputFile, "store <2 x {}> %{}, ptr %{}", LlvmName(typeIndex), registerIndex, stackPointer);
            registerIndex++;
          } else if (byteIndex / 8 != 0 && registers.get(byteIndex - byteSize) == RegisterType::Float) {
            fmt::println(ctx.outputFile, "store {} %{}, ptr %{}", LlvmName(typeIndex), registerIndex, stackPointer);
            registerIndex++;
          }
        } else {
          fmt::println(ctx.outputFile, "store {} %{}, ptr %{}", LlvmName(typeIndex), registerIndex, stackPointer);
          registerIndex++;
        }
        byteIndex += byteSize;
      },
      [&](StructIndex x) {
        Struct& structDef = Pool().getStruct(x);
        auto prevRegister = registerIndex;
        auto fieldIndex = 0;
        for (auto fieldType : structDef.fieldTypes) {
          if (prevRegister != registerIndex) {
            ctx.outputFile << ", ";
            fieldIndex++;
          }
          auto fieldPointer = ctx.environment.addTemporary();
          fmt::println(ctx.outputFile, "%{} = getelementptr inbounds {}, ptr %{}, i32 0, i32 {}", fieldPointer, LlvmName(typeIndex), stackPointer, fieldIndex);
          loadParameterRegisters(ctx, fieldType, stackPointer, registers, byteIndex, registerIndex);
        }
      },
      [&](TupleIndex x) { TODO("Loading parameters for tuples"); },
      [&](SizedArray x) {
        for (u32 i = 0; i < x.length; i++) {
          auto fieldPointer = ctx.environment.addTemporary();
          fmt::println(ctx.outputFile, "%{} = getelementptr inbounds {}, ptr %{}, i32 0, i32 {}", fieldPointer, LlvmName(x.dereferencedType), stackPointer, i);
          loadParameterRegisters(ctx, x.dereferencedType, fieldPointer, registers, byteIndex, registerIndex);
        }
      },
      [&](EnumIndex x) { loadParameterRegisters(ctx, Pool().getEnum(x).rawType, stackPointer, registers, byteIndex, registerIndex); },
      [&](auto x) { TODO("Can't create args"); },
    },
    type
  );
}

void loadParameterRegisters(OutContext& ctx, TypeIndex typeIndex, RegisterName stackPointer, u32& registerIndex) {
  RegisterAssignment registers;
  Pool().registerStorage(typeIndex, registers);
  u8 byteIndex = 0;
  loadParameterRegisters(ctx, typeIndex, stackPointer, registers, byteIndex, registerIndex);
}

void loadParameterRegisters(OutContext& ctx, FunctionType function, span<Identifier> paramNames) {
  auto paramTypes = Pool().tupleElements(function.parameters);
  assert(paramTypes.size() == paramNames.size());
  u32 paramIndex = 0;
  u32 registerIndex = 0;
  for (auto name : paramNames) {
    auto typeIndex = paramTypes[paramIndex];
    RegisterAssignment assignment;
    Pool().registerStorage(typeIndex, assignment);
    if (assignment.isVoid()) {
      paramIndex++;
      continue;
    }

    auto sizing = Pool().getSizing(typeIndex);
    fmt::println(ctx.outputFile, "%{} = alloca {}, align {}", name, LlvmName(typeIndex), sizing.alignment.byteAlignment());
    loadParameterRegisters(ctx, typeIndex, name, registerIndex);
    paramIndex++;
  }

  // First numbered register after parameters is entry label
  ctx.environment.addTemporary();
}
