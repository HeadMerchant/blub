#include "abi.h"
#include "common.h"
#include "compilercontext.h"
#include "fmt/base.h"
#include "registers.h"
#include "types.h"
#include "value.h"
#include <sstream>
#include <unistd.h>

void declareParameterRegisters(std::ostream& outputFile, TypeIndex typeIndex, RegisterAssignment& registers, u32& registerIndex) {
  auto sizing = Pool().getSizing(typeIndex);

  if (sizing.byteSize == 0) return;
  if (registers.typeAt(0) == RegisterType::Memory) {
    fmt::print(outputFile, "ptr noundef byval({}) align {}", LlvmName(typeIndex), sizing.alignment.byteAlignment());
    registerIndex++;
    return;
  }
  if (registers.allInt(sizing.byteSize)) {
    outputFile << LlvmName(typeIndex);
    registerIndex++;
    registers.pop(sizing.byteSize);
    return;
  }

  auto type = Pool().getType(typeIndex);
  std::visit(
    overloaded{
      [&]<IntRegister T>(T x) { assert(false); },
      [&](Float x) {
        if (x.precision != Float::f32) {
          TODO("Declaring non-f32 floats in parameters");
        }
        auto byteSize = x.byteSize();
        if (registers.typeAt() == RegisterType::Float) {
          if (registers.readIndex / 8 == 0) {
            registerIndex++;
            if (registers.typeAt(byteSize) == RegisterType::Float) {
              fmt::print(outputFile, "<2 x {}>", LlvmName(typeIndex));
            } else {
              outputFile << LlvmName(typeIndex);
            }
          } else if (registers.readIndex / 8 != 0 && registers.typeAt(-byteSize) != RegisterType::Float) {
            outputFile << LlvmName(typeIndex);
            registerIndex++;
          }
        } else {
          outputFile << LlvmName(typeIndex);
          registerIndex++;
        }
        registers.pop(byteSize);
      },
      [&]<AggregateType T>(T x) {
        auto prevRegister = registerIndex;
        for (auto fieldType : x.fields()) {
          if (prevRegister != registerIndex) {
            outputFile << ", ";
          }
          prevRegister = registerIndex;
          declareParameterRegisters(outputFile, fieldType, registers, registerIndex);
        }
      },
      [&](EnumIndex x) { declareParameterRegisters(outputFile, Pool().getEnum(x).rawType, registers, registerIndex); },
      [&](auto x) { TODO("Can't create args"); },
    },
    type
  );
}

u32 declareParameterRegisters(std::ostream& outputFile, TypeIndex typeIndex) {
  RegisterAssignment registers = Pool().registerStorage(typeIndex);
  u32 registerIndex = 0;
  declareParameterRegisters(outputFile, typeIndex, registers, registerIndex);
  return registerIndex;
}

// Assume that the caller has put declare/define first
DeclarationResult declareParamRegisters(std::ostream& outputFile, Function function) {
  auto returnType = function.type.returnType;
  RegisterAssignment returnRegisters = Pool().registerStorage(returnType);

  std::string returnTypeName;
  if (returnRegisters.isMemory() || returnRegisters.isVoid()) {
    outputFile << "void";
  } else if (returnRegisters.allInt() || !Pool().isAggregate(returnType)) {
    outputFile << TypeName(returnType);
  } else {
    std::stringstream returnStream;
    returnStream << "{";
    declareParameterRegisters(outputFile, returnType);
    returnStream << "}";
    returnTypeName = returnStream.str();
  }

  u32 registersUsed = 0;
  bool needsComma = false;
  fmt::print(outputFile, " {}(", function.globalName);
  if (returnRegisters.isMemory()) {
    fmt::print(outputFile, "ptr sret({}) align {}", LlvmName(returnType), Pool().getSizing(returnType).alignment.byteAlignment());
    needsComma = true;
    registersUsed += 1;
  }

  for (auto paramType : Pool().tupleElements(function.type.parameters)) {
    if (needsComma) {
      outputFile << ", ";
    }
    auto newRegisters = declareParameterRegisters(outputFile, paramType);
    needsComma = (bool)newRegisters;
    registersUsed += newRegisters;
  }

  outputFile << ")";
  return {returnTypeName, registersUsed};
}

void loadParameterRegisters(OutContext& ctx, TypeIndex typeIndex, RegisterName stackPointer, RegisterAssignment registers, u32& registerIndex) {
  auto sizing = Pool().getSizing(typeIndex);
  if (sizing.byteSize == 0) return;
  if (registers.typeAt() == RegisterType::Memory) {
    fmt::println(ctx.outputFile, "call void @llvm.memcpy.p0.p0.i8(ptr %{}, ptr %{}, i64 {}, i1 false)", stackPointer, sizing.byteSize, registerIndex);
    // High-key byteIndex doesn't matter
    registerIndex++;
    return;
  }
  if (registers.allInt(sizing.byteSize)) {
    fmt::println(ctx.outputFile, "store {} %{}, ptr %{}", LlvmName(typeIndex), registerIndex, stackPointer);
    registers.pop(sizing.byteSize);
    registerIndex++;
    return;
  }
  auto type = Pool().getType(typeIndex);
  std::visit(
    overloaded{
      [&]<IntRegister T>(T x) { assert(false); },
      [&](Float x) {
        if (x.precision != Float::f32) {
          TODO("Loading non-f32 floats from parameters");
        }
        auto byteSize = x.byteSize();
        if (registers.typeAt() == RegisterType::Float) {
          if (registers.readIndex / 8 == 0 && registers.typeAt(byteSize) == RegisterType::Float) {
            fmt::println(ctx.outputFile, "store <2 x {}> %{}, ptr %{}", LlvmName(typeIndex), registerIndex, stackPointer);
            registerIndex++;
          } else if (registers.readIndex / 8 != 0 && registers.typeAt(-byteSize) == RegisterType::Float) {
            // fmt::println(ctx.outputFile, "store {} %{}, ptr %{}", LlvmName(typeIndex), registerIndex, stackPointer);
          } else {
            fmt::println(ctx.outputFile, "store {} %{}, ptr %{}", LlvmName(typeIndex), registerIndex, stackPointer);
            registerIndex++;
          }
        } else {
          fmt::println(ctx.outputFile, "store {} %{}, ptr %{}", LlvmName(typeIndex), registerIndex, stackPointer);
          registerIndex++;
        }
        registers.pop(byteSize);
      },
      [&]<AggregateType T>(T x) {
        auto prevRegister = registerIndex;
        auto fieldIndex = 0;
        for (auto fieldType : x.fields()) {
          if (prevRegister != registerIndex) {
            ctx.outputFile << ", ";
            fieldIndex++;
          }
          auto fieldPointer = ctx.environment.addTemporary();
          fmt::println(ctx.outputFile, "%{} = getelementptr inbounds {}, ptr %{}, i32 0, i32 {}", fieldPointer, LlvmName(typeIndex), stackPointer, fieldIndex);
          loadParameterRegisters(ctx, fieldType, stackPointer, registers, registerIndex);
        }
      },
      [&](EnumIndex x) { loadParameterRegisters(ctx, Pool().getEnum(x).rawType, stackPointer, registers, registerIndex); },
      [&](auto x) { TODO("Can't create args"); },
    },
    type
  );
}

void loadParameterRegisters(OutContext& ctx, TypeIndex typeIndex, RegisterName stackPointer, u32& registerIndex) {
  RegisterAssignment registers = Pool().registerStorage(typeIndex);
  loadParameterRegisters(ctx, typeIndex, stackPointer, registers, registerIndex);
}

void loadParameterRegisters(OutContext& ctx, FunctionType function, span<Identifier> paramNames) {
  auto paramTypes = Pool().tupleElements(function.parameters);
  assert(paramTypes.size() == paramNames.size());
  u32 paramIndex = 0;
  u32 registerIndex = 0;
  for (auto name : paramNames) {
    auto typeIndex = paramTypes[paramIndex];
    RegisterAssignment assignment = Pool().registerStorage(typeIndex);
    if (assignment.isVoid()) {
      paramIndex++;
      continue;
    }

    auto sizing = Pool().getSizing(typeIndex);
    fmt::println(ctx.outputFile, "%{} = alloca {}, align {}", name, LlvmName(typeIndex), sizing.alignment.byteAlignment());
    loadParameterRegisters(ctx, typeIndex, name, registerIndex);
    paramIndex++;
  }
}

bool passArg(OutContext& ctx, TypeIndex typeIndex, RegisterAssignment& registers, Reference& arg, std::stringstream& callSite) {
  auto sizing = Pool().getSizing(typeIndex);
  if (sizing.byteSize == 0) return false;
  LlvmName typeName(typeIndex);
  if (registers.typeAt() == RegisterType::Memory) {
    auto ptrRegister = ctx.environment.addTemporary();
    fmt::println(ctx.outputFile, "%{} = alloca {}, align {}", ptrRegister, typeName, sizing.alignment.byteAlignment());
    fmt::println(ctx.outputFile, "store {} {}, ptr %{}", typeName, arg, ptrRegister);
    fmt::print(callSite, "ptr noundef byval({}) align {} %{}", typeName, sizing.alignment.byteAlignment(), ptrRegister);
    registers.pop();
    return true;
  }
  if (registers.allInt(sizing.byteSize)) {
    registers.pop(sizing.byteSize);
    fmt::print(callSite, "{} {}", typeName, arg);
    return true;
  }

  auto type = Pool().getType(typeIndex);
  return std::visit(
    overloaded{
      [&]<IntRegister T>(T x) {
        assert(false);
        return true;
      },
      [&](Float x) {
        if (x.precision != Float::f32) {
          TODO("Loading non-f32 floats from parameters");
        }
        auto byteSize = x.byteSize();
        bool consumedParam = true;
        if (registers.typeAt() == RegisterType::Float) {
          if (registers.readIndex / 8 == 0 && registers.typeAt(byteSize) == RegisterType::Float) {
            fmt::println(ctx.outputFile, "%{} = insertelement <2 x {}> undef, i32 0, {} {}", ctx.environment.addTemporary(), typeName, typeName, arg);
            consumedParam = false;
          } else if (registers.readIndex / 8 != 0 && registers.typeAt(-byteSize) == RegisterType::Float) {
            auto prevRegister = ctx.environment.nextTemporary - 1;
            auto newRegister = ctx.environment.addTemporary();
            fmt::println(ctx.outputFile, "%{} = insertelement <2 x {}> %{}, i32 1, {} {}", newRegister, typeName, prevRegister, typeName, arg);
            fmt::print(callSite, "<2 x {}> %{}", typeName, newRegister);
          } else {
            fmt::print(callSite, "{} {}", typeName, arg);
          }
        } else {
          fmt::print(callSite, "{} {}", typeName, arg);
        }
        registers.pop(byteSize);
        return consumedParam;
      },
      [&]<AggregateType T>(T x) {
        bool needsComma = false;
        auto fields = x.fields();
        for (u32 i = 0; i < fields.size(); i++) {
          // TODO: ZST
          if (needsComma) {
            callSite << ", ";
          }
          auto fieldType = fields[i];
          Reference fieldArg(ctx.environment.makeTemporary(fieldType));
          fmt::println(ctx.outputFile, "{} = extractvalue {} {}, {}", fieldArg, typeName, arg, i);

          needsComma = passArg(ctx, fieldType, registers, fieldArg, callSite);
        }
        return true;
      },
      [&](EnumIndex x) { return passArg(ctx, Pool().getEnum(x).rawType, registers, arg, callSite); },
      [&](auto x) {
        TODO("Can't create args");
        // TODO
        return false;
      },
    },
    type
  );
}

u32 callAbiFunctionWithArgs(OutContext& ctx, Function function, span<Reference> args) {
  auto returnType = function.type.returnType;
  RegisterAssignment returnRegisters = Pool().registerStorage(returnType);

  u32 returnRegister = 0;
  std::stringstream callSite;
  callSite << "call ";
  std::string transmuteReturnType;
  if (returnRegisters.isMemory() || returnRegisters.isVoid()) {
    ctx.outputFile << "void";
  } else if (returnRegisters.allInt() || !Pool().isAggregate(returnType)) {
    callSite << TypeName(returnType);
  } else {
    std::stringstream returnStream;
    returnStream << "{";
    declareParameterRegisters(returnStream, returnType);
    returnStream << "}";
    transmuteReturnType = returnStream.str();
  }

  bool needsComma = false;
  auto returnSizing = Pool().getSizing(returnType);
  fmt::print(callSite, " {}(", function.globalName);
  LlvmName typeName(returnType);
  if (returnRegisters.isMemory()) {
    returnRegister = ctx.environment.addTemporary();
    fmt::println(ctx.outputFile, "%{} = alloca {}, align {}", returnRegister, typeName, returnSizing.alignment.byteAlignment());
    fmt::print(callSite, "ptr sret({}) align {} %{}", typeName, returnSizing.alignment.byteAlignment(), returnRegister);
    needsComma = true;
  }

  auto paramTypes = Pool().tupleElements(function.type.parameters);
  assert(paramTypes.size() == args.size());
  for (u32 i = 0; i < args.size(); i++) {
    if (needsComma) {
      callSite << ", ";
    }
    auto& arg = args[i];
    auto paramType = paramTypes[i];
    auto registers = Pool().registerStorage(paramType);
    needsComma = passArg(ctx, paramType, registers, arg, callSite);
  }

  callSite << ")\n";

  auto call = callSite.str();
  if (returnRegisters.isMemory()) {
    fmt::println(ctx.outputFile, "%{} = load {}, ptr %{}", returnRegister, LlvmName(returnType), returnRegister);
  } else if (returnRegisters.allInt() || !transmuteReturnType.empty()) {
    returnRegister = ctx.environment.addTemporary();
    fmt::print(ctx.outputFile, "%{} = ", returnRegister);
  }
  ctx.outputFile << callSite.str();
  if (!transmuteReturnType.empty()) {
    auto storage = ctx.environment.addTemporary();
    auto transmuted = ctx.environment.addTemporary();
    fmt::println(ctx.outputFile, "%{} = alloca {}, align {}", storage, transmuteReturnType, returnSizing.alignment.byteAlignment());
    fmt::println(ctx.outputFile, "store {} %{}, ptr %{}", transmuteReturnType, returnRegister, storage);
    fmt::println(ctx.outputFile, "%{} = load {}, ptr %{}", transmuted, LlvmName(returnType), storage);
    return transmuted;
  }
  return returnRegister;
}

TEST_CASE("Passing primative args") {
  using std::stringstream;
  TypeIndex f32 = Pool()._f32;
  TypeIndex u32 = Pool()._u32;
  auto [_, paramTuple] = Pool().tupleOf({f32, u32});
  Function function{
    .type = FunctionType{.parameters = paramTuple, .returnType = f32},
    .globalName = "@testFunc"
  };
  SUBCASE("Declaration") {
    stringstream declaration;
    auto declarationResult = declareParamRegisters(declaration, function);
    CHECK_EQ(declaration.str(), "f32 @testFunc(float, i32)");
    CHECK_EQ(declarationResult.lastParameterRegister, 2);
  }
  SUBCASE("Loading") {
    stringstream functionBody;
    Environment env;
    OutContext ctx{.outputFile = functionBody, .environment = env};
    vector<Identifier> paramNames = {"param1", "param2"};
    loadParameterRegisters(ctx, function.type, paramNames);
    string_view loadStr = "%param1 = alloca float, align 4\n"
                          "store float %0, ptr %param1\n"
                          "%param2 = alloca i32, align 4\n"
                          "store i32 %1, ptr %param2\n";
    CHECK_EQ(functionBody.str(), loadStr);
  }
  SUBCASE("Calling") {
    Environment env;
    stringstream callSite;
    vector<Reference> args = {
      Reference(env.makeTemporary(Pool()._f32)),
      Reference(IntLiteral(5)),
    };

    OutContext ctx{.outputFile = callSite, .environment = env};
    auto returnRegister = callAbiFunctionWithArgs(ctx, function, args);
    string_view expectedCallSite = "%2 = call float @testFunc(float %1, i32 5)\n";
    CHECK_EQ(returnRegister, 2);
    CHECK_EQ(callSite.str(), expectedCallSite);
  }
}
