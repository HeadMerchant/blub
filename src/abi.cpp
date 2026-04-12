#include "abi.h"
#include "common.h"
#include "compilercontext.h"
#include "fmt/base.h"
#include "registers.h"
#include "types.h"
#include "value.h"
#include <sstream>
#include <unistd.h>

template <typename T>
concept AbiVisitor = requires(T t, RegisterAssignment& registers) {
  { t.memory(TypeIndex{}, Sizing{}) };
  { t.inRegister(TypeIndex{}, Sizing{}) };
  { t.sseVectorLow(TypeIndex{}, Sizing{}) };
  { t.sseVectorHigh(TypeIndex{}, Sizing{}) };
  { t.template aggregate<TupleIndex>(TypeIndex{}, TupleIndex{}, registers) };
  { t.template aggregate<SizedArray>(TypeIndex{}, SizedArray{}, registers) };
  { t.template aggregate<StructIndex>(TypeIndex{}, StructIndex{}, registers) };
};
template <AbiVisitor T>

void abiVisit(TypeIndex typeIndex, RegisterAssignment& registers, T& visitor) {
  auto sizing = Pool().getSizing(typeIndex);

  if (sizing.byteSize == 0) {
    return;
  }
  if (registers.typeAt(0) == RegisterType::Memory) {
    visitor.memory(typeIndex, sizing);
    return;
  }
  if (registers.allInt(sizing.byteSize)) {
    visitor.inRegister(typeIndex, sizing);
    registers.pop(sizing.byteSize);
    return;
  }
  UnderlyingType& type = Pool().getType(typeIndex);
  std::visit(
    overloaded{
      [&]<IntRegister I>(I x) { assert(false); },
      [&](Float x) {
        if (x.precision == Float::f16) {
          TODO("Declaring f16 floats in parameters");
        }
        if (x.precision == Float::f64) {
          visitor.inRegister(typeIndex, sizing);
          registers.pop(x.byteSize());
          return;
        }
        u32 byteSize = x.byteSize();
        bool isFloat = registers.typeAt() == RegisterType::Float;
        u8 readOffset = registers.readIndex % 8;
        if (
          isFloat && readOffset == 0 &&
          registers.typeAt(byteSize) == RegisterType::Float
        ) {
          visitor.sseVectorLow(typeIndex, sizing);
          // fmt::println("Low; expecting to pop {} bytes", byteSize);
        } else if (
          isFloat && readOffset != 0 &&
          registers.typeAt(-byteSize) == RegisterType::Float
        ) {
          visitor.sseVectorHigh(typeIndex, sizing);
          // fmt::println("High; expecting to pop {} bytes", byteSize);
        } else {
          visitor.inRegister(typeIndex, sizing);
          // fmt::println("Putting float in int register", byteSize);
        }
        registers.pop(byteSize);
      },
      [&]<AggregateType U>(U x) { visitor.aggregate(typeIndex, x, registers); },
      [&]<RecursiveType U>(U x) { abiVisit(x.rawType(), registers, visitor); },
      [&](auto x) { TODO("Can't create args"); },
    },
    type
  );
}

struct AbiDeclaration {
  std::ostream& outputFile;
  u32 registerIndex;

  void memory(TypeIndex typeIndex, Sizing sizing) {
    fmt::print(
      outputFile,
      "ptr noundef byval({}) align {}",
      LlvmName(typeIndex),
      sizing.alignment.byteAlignment()
    );
    registerIndex++;
  }

  void inRegister(TypeIndex typeIndex, Sizing sizing) {
    outputFile << LlvmName(typeIndex);
    registerIndex++;
  }

  void sseVectorLow(TypeIndex typeIndex, Sizing sizing) {
    fmt::print(outputFile, "<2 x {}>", LlvmName(typeIndex));
  }

  void sseVectorHigh(TypeIndex typeIndex, Sizing sizing) {
    registerIndex++;
  }

  template <AggregateType T>
  void aggregate(TypeIndex typeIndex, T t, RegisterAssignment& registers) {
    auto prevRegister = registerIndex;
    // fmt::println("{:#b}", registers.types);
    for (auto fieldType : t.fields()) {
      if (prevRegister != registerIndex) {
        outputFile << ", ";
      }
      prevRegister = registerIndex;
      abiVisit(fieldType, registers, *this);
    }
  }
};

static_assert(
  AbiVisitor<AbiDeclaration>,
  "Declaration doesn't conform to AbiVisitor"
);

u32 declareParameterRegisters(std::ostream& outputFile, TypeIndex typeIndex) {
  RegisterAssignment registers = Pool().registerStorage(typeIndex);
  AbiDeclaration visitor{.outputFile = outputFile, .registerIndex = 0};
  abiVisit(typeIndex, registers, visitor);
  return visitor.registerIndex;
}

std::string aggregateReturnTypeName(TypeIndex returnType) {
  std::stringstream returnStream;
  returnStream << "{";
  declareParameterRegisters(returnStream, returnType);
  returnStream << "}";
  return returnStream.str();
}

// Assume that the caller has put declare/define first
DeclarationResult declareParamRegisters(
  std::ostream& outputFile,
  Function function
) {
  auto returnType = function.type.returnType;
  RegisterAssignment returnRegisters = Pool().registerStorage(returnType);

  std::string returnTypeName;
  if (returnRegisters.isMemory() || returnRegisters.isVoid()) {
    outputFile << "void";
  } else if (returnRegisters.allInt() || !Pool().isAggregate(returnType)) {
    outputFile << LlvmName(returnType);
  } else {
    returnTypeName = aggregateReturnTypeName(returnType);
    outputFile << returnTypeName;
  }

  u32 registersUsed = 0;
  bool needsComma = false;
  fmt::print(outputFile, " {}(", function.globalName);
  if (returnRegisters.isMemory()) {
    fmt::print(
      outputFile,
      "ptr sret({}) align {}",
      LlvmName(returnType),
      Pool().getSizing(returnType).alignment.byteAlignment()
    );
    needsComma = true;
    registersUsed += 1;
  }

  for (auto paramType : Pool().tupleElements(function.type.parameters)) {
    RegisterAssignment registers = Pool().registerStorage(paramType);
    if (registers.isVoid()) {
      continue;
    }
    if (needsComma) {
      outputFile << ", ";
    }
    AbiDeclaration visitor{.outputFile = outputFile, .registerIndex = 0};
    abiVisit(paramType, registers, visitor);
    u32 newRegisters = visitor.registerIndex;
    needsComma = (bool)newRegisters;
    registersUsed += newRegisters;
  }

  outputFile << ")";
  return {returnTypeName, registersUsed};
}

struct LoadParameterVisitor {
  OutContext& ctx;
  RegisterName stackPointer;
  u32& registerIndex;
  bool usedLower;

  void memory(TypeIndex typeIndex, Sizing sizing) {
    fmt::println(
      ctx.outputFile,
      "call void @llvm.memcpy.p0.p0.i8(ptr %{}, ptr %{}, i64 {}, i1 false)",
      stackPointer,
      registerIndex,
      sizing.byteSize
    );
    // High-key byteIndex doesn't matter
    registerIndex++;
  }

  void inRegister(TypeIndex typeIndex, Sizing sizing) {
    fmt::println(
      ctx.outputFile,
      "store {} %{}, ptr %{}",
      LlvmName(typeIndex),
      registerIndex,
      stackPointer
    );
    registerIndex++;
  }

  void sseVectorLow(TypeIndex typeIndex, Sizing sizing) {
    fmt::println(
      ctx.outputFile,
      "store <2 x {}> %{}, ptr %{}",
      LlvmName(typeIndex),
      registerIndex,
      stackPointer
    );
    registerIndex++;
    usedLower = true;
  }

  void sseVectorHigh(TypeIndex typeIndex, Sizing sizing) {
    usedLower = false;
  }

  template <AggregateType T>
  void aggregate(TypeIndex typeIndex, T x, RegisterAssignment& registers) {
    auto prevRegister = registerIndex;
    auto fieldIndex = 0;
    auto structPointer = stackPointer;
    for (auto fieldType : x.fields()) {
      if (prevRegister != registerIndex) {
        fieldIndex++;
      }
      if (!usedLower) {
        stackPointer = ctx.environment.addTemporary();
        fmt::println(
          ctx.outputFile,
          "%{} = getelementptr inbounds {}, ptr %{}, i32 0, i32 {}",
          stackPointer,
          LlvmName(typeIndex),
          structPointer,
          fieldIndex
        );
      }
      abiVisit(fieldType, registers, *this);
    }
  }
};
static_assert(AbiVisitor<LoadParameterVisitor>, "Need to implement");

void loadParameterRegisters(
  OutContext& ctx,
  FunctionType function,
  span<Identifier> paramNames
) {
  auto paramTypes = Pool().tupleElements(function.parameters);
  assert(paramTypes.size() == paramNames.size());
  u32 paramIndex = 0;
  u32 registerIndex = 0;
  if (Pool().registerStorage(function.returnType).isMemory()) {
    registerIndex++;
  }
  for (auto name : paramNames) {
    auto typeIndex = paramTypes[paramIndex];
    RegisterAssignment assignment = Pool().registerStorage(typeIndex);
    if (assignment.isVoid()) {
      paramIndex++;
      continue;
    }

    auto sizing = Pool().getSizing(typeIndex);
    fmt::println(
      ctx.outputFile,
      "%{} = alloca {}, align {}",
      name,
      LlvmName(typeIndex),
      sizing.alignment.byteAlignment()
    );

    RegisterAssignment registers = Pool().registerStorage(typeIndex);
    LoadParameterVisitor visitor{
      .ctx = ctx,
      .stackPointer = name,
      .registerIndex = registerIndex
    };
    abiVisit(typeIndex, registers, visitor);
    paramIndex++;
  }
}

struct ArgumentVisitor {
  OutContext& ctx;
  std::stringstream& callSite;
  Reference& arg;
  bool consumedArg = false;
  u32 lastSseVectorRegister;

  void memory(TypeIndex typeIndex, Sizing sizing) {
    auto ptrRegister = ctx.environment.addTemporary();
    LlvmName typeName(typeIndex);
    fmt::println(
      ctx.outputFile,
      "%{} = alloca {}, align {}",
      ptrRegister,
      typeName,
      sizing.alignment.byteAlignment()
    );
    fmt::println(
      ctx.outputFile,
      "store {} {}, ptr %{}",
      typeName,
      arg,
      ptrRegister
    );
    fmt::print(
      callSite,
      "ptr noundef byval({}) align {} %{}",
      typeName,
      sizing.alignment.byteAlignment(),
      ptrRegister
    );
    consumedArg = true;
  }

  void inRegister(TypeIndex typeIndex, Sizing sizing) {
    fmt::print(callSite, "{} {}", LlvmName(typeIndex), arg);
    consumedArg = true;
  }

  void sseVectorLow(TypeIndex typeIndex, Sizing sizing) {
    auto reg = ctx.environment.addTemporary();
    fmt::println(
      ctx.outputFile,
      "%{} = insertelement <2 x {}> undef, {} {}, i32 0",
      reg,
      LlvmName(typeIndex),
      LlvmName(typeIndex),
      arg
    );
    consumedArg = false;
    lastSseVectorRegister = reg;
  }

  void sseVectorHigh(TypeIndex typeIndex, Sizing sizing) {
    auto prevRegister = ctx.environment.nextTemporary - 2;
    auto newRegister = ctx.environment.addTemporary();
    LlvmName typeName(typeIndex);
    fmt::println(
      ctx.outputFile,
      "%{} = insertelement <2 x {}> %{}, {} {}, i32 1",
      newRegister,
      typeName,
      prevRegister,
      typeName,
      arg
    );
    fmt::print(callSite, "<2 x {}> %{}", typeName, newRegister);
    consumedArg = true;
  }

  template <AggregateType T>
  void aggregate(TypeIndex typeIndex, T x, RegisterAssignment& registers) {
    bool needsComma = false;
    auto fields = x.fields();
    LlvmName typeName(typeIndex);
    Reference aggregate = arg;
    bool anyFields = false;
    for (u32 i = 0; i < fields.size(); i++) {
      // TODO: ZST
      if (needsComma) {
        callSite << ", ";
      }
      auto fieldType = fields[i];
      arg.value = ctx.environment.makeTemporary(fieldType);
      fmt::println(
        ctx.outputFile,
        "{} = extractvalue {} {}, {}",
        arg,
        typeName,
        aggregate,
        i
      );
      consumedArg = false;
      abiVisit(fieldType, registers, *this);
      needsComma = consumedArg;
      if (needsComma) {
        anyFields = true;
      }
    }
    consumedArg = anyFields;
  }
};
static_assert(AbiVisitor<ArgumentVisitor>, "...");

u32 callAbiFunctionWithArgs(
  OutContext& ctx,
  Function function,
  span<Reference> args
) {
  auto returnType = function.type.returnType;
  RegisterAssignment returnRegisters = Pool().registerStorage(returnType);

  u32 returnRegister = 0;
  std::stringstream callSite;
  callSite << "call ";
  std::string transmuteReturnType;
  if (returnRegisters.isMemory() || returnRegisters.isVoid()) {
    callSite << "void";
  } else if (returnRegisters.allInt() || !Pool().isAggregate(returnType)) {
    callSite << LlvmName(returnType);
  } else {
    transmuteReturnType = aggregateReturnTypeName(returnType);
    callSite << transmuteReturnType;
  }

  bool needsComma = false;
  auto returnSizing = Pool().getSizing(returnType);
  fmt::print(callSite, " {}(", Reference(function));
  LlvmName typeName(returnType);
  if (returnRegisters.isMemory()) {
    returnRegister = ctx.environment.addTemporary();
    fmt::println(
      ctx.outputFile,
      "%{} = alloca {}, align {}",
      returnRegister,
      typeName,
      returnSizing.alignment.byteAlignment()
    );
    fmt::print(
      callSite,
      "ptr sret({}) align {} %{}",
      typeName,
      returnSizing.alignment.byteAlignment(),
      returnRegister
    );
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
    ArgumentVisitor visitor{
      .ctx = ctx,
      .callSite = callSite,
      .arg = arg,
      .consumedArg = false
    };
    abiVisit(paramType, registers, visitor);
    needsComma = visitor.consumedArg;
  }

  callSite << ")\n";

  auto call = callSite.str();
  if (returnRegisters.isMemory()) {
    ctx.outputFile << callSite.str();
    auto sret = returnRegister;
    returnRegister = ctx.environment.addTemporary();
    fmt::println(
      ctx.outputFile,
      "%{} = load {}, ptr %{}",
      returnRegister,
      LlvmName(returnType),
      sret
    );
  } else if (
    returnRegisters.allInt() || Pool().isFloat(returnType) ||
    !transmuteReturnType.empty()
  ) {
    returnRegister = ctx.environment.addTemporary();
    fmt::print(ctx.outputFile, "%{} = ", returnRegister);
    ctx.outputFile << callSite.str();
  } else {
    ctx.outputFile << callSite.str();
  }

  if (!transmuteReturnType.empty()) {
    auto storage = ctx.environment.addTemporary();
    auto transmuted = ctx.environment.addTemporary();
    fmt::println(
      ctx.outputFile,
      "%{} = alloca {}, align {}",
      storage,
      transmuteReturnType,
      returnSizing.alignment.byteAlignment()
    );
    fmt::println(
      ctx.outputFile,
      "store {} %{}, ptr %{}",
      transmuteReturnType,
      returnRegister,
      storage
    );
    fmt::println(
      ctx.outputFile,
      "%{} = load {}, ptr %{}",
      transmuted,
      LlvmName(returnType),
      storage
    );
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
    .globalName = "testFunc"
  };
  SUBCASE("Declaration") {
    stringstream declaration;
    auto declarationResult = declareParamRegisters(declaration, function);
    CHECK_EQ(declaration.str(), "float @\"testFunc\"(float, i32)");
    CHECK_EQ(declarationResult.entryLabel, 2);
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
    string_view expectedCallSite =
      "%2 = call float @\"testFunc\"(float %1, i32 5)\n";
    CHECK_EQ(returnRegister, 2);
    CHECK_EQ(callSite.str(), expectedCallSite);
  }
}

TEST_CASE("Struct args and returns") {
  using std::stringstream;
  TypeIndex f32 = Pool()._f32;
  auto vec3 = Pool().sizedArrayOf(f32, 3);
  auto mat4x4 = Pool().sizedArrayOf(Pool().sizedArrayOf(f32, 4), 4);
  TupleIndex paramTuple = Pool().tupleOf({vec3}).second;
  Function function{
    .type = FunctionType{.parameters = paramTuple, .returnType = mat4x4},
    .globalName = "translate"
  };
  SUBCASE("Declaration") {
    stringstream declaration;
    auto declarationResult = declareParamRegisters(declaration, function);
    CHECK_EQ(
      declaration.str(),
      "void @\"translate\"(ptr sret([4 x [4 x float]]) align 4, <2 x float>, "
      "float)"
    );
    CHECK_EQ(declarationResult.entryLabel, 3);
  }
  SUBCASE("Loading") {
    stringstream functionBody;
    Environment env;
    env.envType = EnvType::Function;
    env.nextTemporary = 4;
    OutContext ctx{.outputFile = functionBody, .environment = env};
    vector<Identifier> paramNames = {"vec"};
    loadParameterRegisters(ctx, function.type, paramNames);
    string_view loadStr = "%vec = alloca [3 x float], align 4\n"
                          "%4 = getelementptr inbounds [3 x float], "
                          "ptr %vec, i32 0, i32 0\n"
                          "store <2 x float> %1, ptr %4\n"
                          "%5 = getelementptr inbounds [3 x float], "
                          "ptr %vec, i32 0, i32 2\n"
                          "store float %2, ptr %5\n";
    CHECK_EQ(functionBody.str(), loadStr);
    CHECK_EQ(env.nextTemporary, 6);
  }
  SUBCASE("Calling") {
    Environment env;
    env.envType = EnvType::Function;
    stringstream callSite;
    auto arg1 = env.makeTemporary(vec3);
    vector<Reference> args = {
      Reference(arg1),
    };
    REQUIRE_EQ(std::get<u32>(arg1.name), 1);

    OutContext ctx{.outputFile = callSite, .environment = env};
    auto returnRegister = callAbiFunctionWithArgs(ctx, function, args);
    string_view expectedCallSite =
      "%2 = alloca [4 x [4 x float]], align 4\n"
      "%3 = extractvalue [3 x float] %1, 0\n"
      "%4 = insertelement <2 x float> undef, float %3, i32 0\n"
      "%5 = extractvalue [3 x float] %1, 1\n"
      "%6 = insertelement <2 x float> %4, float %5, i32 1\n"
      "%7 = extractvalue [3 x float] %1, 2\n"
      "call void @\"translate\"(ptr sret([4 x [4 x float]]) align 4 %2, <2 x "
      "float> %6, float %7)\n"
      "%8 = load [4 x [4 x float]], ptr %2\n";
    CHECK_EQ(returnRegister, 8);
    CHECK_EQ(callSite.str(), expectedCallSite);
  }
}

TEST_CASE("Returning SSE") {
  using std::stringstream;
  TypeIndex f32 = Pool()._f32;

  // auto [quat, _] = Pool().tupleOf({f32, f32, f32, f32});
  auto [quat, structIndex] = Pool().makeStruct("quat", "%quat");
  Pool().getStruct(structIndex).fieldTypes = {f32, f32, f32, f32};
  TupleIndex paramTuple = Pool().tupleOf({quat, quat}).second;
  Function function{
    .type = FunctionType{.parameters = paramTuple, .returnType = quat},
    .globalName = "multiply"
  };
  SUBCASE("Declaration") {
    string_view expected =
      "{<2 x float>, <2 x float>} @\"multiply\"(<2 x float>, "
      "<2 x float>, <2 x float>, <2 x float>)";
    stringstream declaration;
    auto declarationResult = declareParamRegisters(declaration, function);
    CHECK_EQ(declaration.str(), expected);
    CHECK_EQ(declarationResult.entryLabel, 4);
  }
  SUBCASE("Loading") {
    stringstream functionBody;
    Environment env;
    env.nextTemporary = 5;
    env.envType = EnvType::Function;
    OutContext ctx{.outputFile = functionBody, .environment = env};
    vector<Identifier> paramNames = {"q1", "q2"};
    loadParameterRegisters(ctx, function.type, paramNames);
    string_view expected = "%q1 = alloca %quat, align 4\n"
                           "%5 = getelementptr inbounds %quat, ptr %q1, i32 "
                           "0, i32 0\n"
                           "store <2 x float> %0, ptr %5\n"
                           "%6 = getelementptr inbounds %quat, ptr %q1, i32 "
                           "0, i32 2\n"
                           "store <2 x float> %1, ptr %6\n"
                           "%q2 = alloca %quat, align 4\n"
                           "%7 = getelementptr inbounds %quat, ptr %q2, i32 "
                           "0, i32 0\n"
                           "store <2 x float> %2, ptr %7\n"
                           "%8 = getelementptr inbounds %quat, ptr %q2, i32 "
                           "0, i32 2\n"
                           "store <2 x float> %3, ptr %8\n";
    CHECK_EQ(functionBody.str(), expected);
  }
  SUBCASE("Calling") {
    Environment env;
    env.envType = EnvType::Function;
    stringstream callSite;
    auto arg1 = env.makeTemporary(quat);
    auto arg2 = env.makeTemporary(quat);
    vector<Reference> args = {
      Reference(arg1),
      Reference(arg2),
    };
    REQUIRE_EQ(std::get<u32>(arg1.name), 1);
    REQUIRE_EQ(std::get<u32>(arg2.name), 2);
    OutContext ctx{.outputFile = callSite, .environment = env};
    auto returnRegister = callAbiFunctionWithArgs(ctx, function, args);
    string_view expectedCallSite =
      "%3 = extractvalue %quat %1, 0\n"
      "%4 = insertelement <2 x float> undef, float %3, i32 0\n"
      "%5 = extractvalue %quat %1, 1\n"
      "%6 = insertelement <2 x float> %4, float %5, i32 1\n"
      "%7 = extractvalue %quat %1, 2\n"
      "%8 = insertelement <2 x float> undef, float %7, i32 0\n"
      "%9 = extractvalue %quat %1, 3\n"
      "%10 = insertelement <2 x float> %8, float %9, i32 1\n"
      "%11 = extractvalue %quat %2, 0\n"
      "%12 = insertelement <2 x float> undef, float %11, i32 0\n"
      "%13 = extractvalue %quat %2, 1\n"
      "%14 = insertelement <2 x float> %12, float %13, i32 1\n"
      "%15 = extractvalue %quat %2, 2\n"
      "%16 = insertelement <2 x float> undef, float %15, i32 0\n"
      "%17 = extractvalue %quat %2, 3\n"
      "%18 = insertelement <2 x float> %16, float %17, i32 1\n"
      "%19 = call {<2 x float>, <2 x float>} @\"multiply\"(<2 x float> %6, <2 "
      "x "
      "float> %10, <2 x float> %14, <2 x float> %18)\n"
      "%20 = alloca {<2 x float>, <2 x float>}, align 4\n"
      "store {<2 x float>, <2 x float>} %19, ptr %20\n"
      "%21 = load %quat, ptr %20\n";
    CHECK_EQ(returnRegister, 21);
    CHECK_EQ(callSite.str(), expectedCallSite);
  }
}

TEST_CASE("Passing in memory") {
  TypeIndex f32 = Pool()._f32;

  // auto matrix = ;
  auto [matrix, structIndex] = Pool().makeStruct("", "%mat");
  Pool().getStruct(structIndex).fieldTypes = {Pool().sizedArrayOf(f32, 16)};
  TupleIndex paramTuple =
    Pool().tupleOf({matrix, Pool().sizedArrayOf(Pool()._s16, 8)}).second;
  Function function{
    .type = FunctionType{.parameters = paramTuple, .returnType = matrix},
    .globalName = "@multiply"
  };
  SUBCASE("Declaration") {
    string_view expected =
      "void @\"multiply\"(ptr sret(%mat) align 4, ptr noundef "
      "byval(%mat) align 4, [8 x i16])";
    stringstream declaration;
    auto declarationResult = declareParamRegisters(declaration, function);
    CHECK_EQ(declaration.str(), expected);
    CHECK_EQ(declarationResult.entryLabel, 3);
  }
  SUBCASE("Loading") {
    stringstream functionBody;
    Environment env;
    u32 nextTemporary = 7;
    env.nextTemporary = nextTemporary;
    env.envType = EnvType::Function;
    OutContext ctx{.outputFile = functionBody, .environment = env};
    vector<Identifier> paramNames = {"q1", "q2"};
    loadParameterRegisters(ctx, function.type, paramNames);
    string_view expected =
      "%q1 = alloca %mat, align 4\n"
      "call void @llvm.memcpy.p0.p0.i8(ptr %q1, ptr %1, i64 64, i1 false)\n"
      "%q2 = alloca [8 x i16], align 2\n"
      "store [8 x i16] %2, ptr %q2\n";
    CHECK_EQ(functionBody.str(), expected);
    CHECK_EQ(env.nextTemporary, nextTemporary);
  }
  SUBCASE("Calling") {
    Environment env;
    env.envType = EnvType::Function;
    stringstream callSite;
    auto arg1 = env.makeTemporary(matrix);
    auto arg2 = env.makeTemporary(matrix);
    vector<Reference> args = {
      Reference(arg1),
      Reference(arg2),
    };
    REQUIRE_EQ(std::get<u32>(arg1.name), 1);
    REQUIRE_EQ(std::get<u32>(arg2.name), 2);
    OutContext ctx{.outputFile = callSite, .environment = env};
    auto returnRegister = callAbiFunctionWithArgs(ctx, function, args);
    string_view expectedCallSite = "%3 = alloca %mat, align 4\n"
                                   "%4 = alloca %mat, align 4\n"
                                   "store %mat %1, ptr %4\n"
                                   "call void @\"multiply\"(ptr sret(%mat) "
                                   "align 4 %3, ptr noundef byval(%mat) "
                                   "align 4 %4, [8 x i16] %2)\n"
                                   "%5 = load %mat, ptr %3\n";
    CHECK_EQ(returnRegister, 5);
    CHECK_EQ(callSite.str(), expectedCallSite);
  }
}

struct TestVisitor {
  u32 calledMemory = 0;
  u32 calledSseLow = 0;
  u32 calledSseHigh = 0;
  u32 calledRegister = 0;
  u32 calledAggregate = 0;

  void memory(TypeIndex t, Sizing size) {
    calledMemory++;
  }

  void sseVectorLow(TypeIndex t, Sizing size) {
    calledSseLow++;
  }

  void sseVectorHigh(TypeIndex t, Sizing size) {
    calledSseHigh++;
  }

  template <AggregateType T>
  void aggregate(TypeIndex typeIndex, T t, RegisterAssignment& registers) {
    calledAggregate++;
    for (auto field : t.fields()) {
      abiVisit(field, registers, *this);
    }
  }

  void inRegister(TypeIndex t, Sizing size) {
    calledRegister++;
  }
};

static_assert(AbiVisitor<TestVisitor>, "...");
TEST_CASE("Visitor is good") {
  TestVisitor visitor;
  auto f32 = Pool()._f32;
  auto [vec3, _] = Pool().tupleOf({f32, f32, f32});
  RegisterAssignment registers = Pool().registerStorage(vec3);
  REQUIRE_EQ(registers.types, 0b010101010101010101010101);
  abiVisit(vec3, registers, visitor);
  CHECK_EQ(visitor.calledAggregate, 1);
  CHECK_EQ(visitor.calledSseLow, 1);
  CHECK_EQ(visitor.calledSseHigh, 1);
  CHECK_EQ(visitor.calledRegister, 1);
  CHECK_EQ(visitor.calledMemory, 0);
}
