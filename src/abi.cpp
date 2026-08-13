#include "abi.h"
#include "common.h"
#include "fmt/base.h"
#include "fmt/format.h"
#include "fmt/ostream.h"
#include "types.h"
#include "value.h"
#include <algorithm>
#include <bit>
#include <sstream>

namespace {

enum class EightbyteClass : u8 {
  NoClass,
  Sse,
  Integer,
  Memory,
};

struct Leaf {
  TypeIndex type;
  u32 offset;
  u32 byteSize;
  EightbyteClass abiClass;
};

struct ClassifiedAggregate {
  std::array<EightbyteClass, 2> classes = {
    EightbyteClass::NoClass,
    EightbyteClass::NoClass,
  };
  std::vector<Leaf> leaves;
  bool memory = false;
};

u32 alignTo(u32 value, u32 alignment) {
  return (value + alignment - 1) & ~(alignment - 1);
}

EightbyteClass mergeClass(EightbyteClass left, EightbyteClass right) {
  if (left == right) return left;
  if (left == EightbyteClass::NoClass) return right;
  if (right == EightbyteClass::NoClass) return left;
  if (left == EightbyteClass::Memory || right == EightbyteClass::Memory) {
    return EightbyteClass::Memory;
  }
  if (left == EightbyteClass::Integer || right == EightbyteClass::Integer) {
    return EightbyteClass::Integer;
  }
  return EightbyteClass::Sse;
}

void addLeaf(
  ClassifiedAggregate& result,
  TypeIndex type,
  u32 offset,
  EightbyteClass abiClass
) {
  auto size = Pool().getSizing(type).byteSize;
  if (size == 0) return;
  if (offset + size > 16) {
    result.memory = true;
    return;
  }

  result.leaves.push_back({type, offset, size, abiClass});
  auto first = offset / 8;
  auto last = (offset + size - 1) / 8;
  for (u32 i = first; i <= last; ++i) {
    result.classes[i] = mergeClass(result.classes[i], abiClass);
  }
}

void classifyAt(ClassifiedAggregate& result, TypeIndex type, u32 offset);

template <TypeRange Range>
void classifyFields(ClassifiedAggregate& result, Range fields, u32 baseOffset) {
  u32 fieldOffset = 0;
  for (auto field : fields) {
    auto sizing = Pool().getSizing(field);
    fieldOffset = alignTo(fieldOffset, sizing.alignment.byteAlignment());
    classifyAt(result, field, baseOffset + fieldOffset);
    fieldOffset += sizing.byteSize;
  }
}

void classifyAt(ClassifiedAggregate& result, TypeIndex type, u32 offset) {
  std::visit(
    overloaded{
      [&](VoidType) {},
      [&](SignedInt) {
        addLeaf(result, type, offset, EightbyteClass::Integer);
      },
      [&](UnsignedInt) {
        addLeaf(result, type, offset, EightbyteClass::Integer);
      },
      [&](Pointer) { addLeaf(result, type, offset, EightbyteClass::Integer); },
      [&](MultiPointer) {
        addLeaf(result, type, offset, EightbyteClass::Integer);
      },
      [&](FunctionType) {
        addLeaf(result, type, offset, EightbyteClass::Integer);
      },
      [&](BoundFunctionType) {
        addLeaf(result, type, offset, EightbyteClass::Integer);
      },
      [&](Float) { addLeaf(result, type, offset, EightbyteClass::Sse); },
      [&](VectorType) { addLeaf(result, type, offset, EightbyteClass::Sse); },
      [&](Slice) {
        addLeaf(
          result,
          Pool().pointerTo(Pool()._void),
          offset,
          EightbyteClass::Integer
        );
        addLeaf(result, Pool()._usize, offset + 8, EightbyteClass::Integer);
      },
      [&](StructIndex x) { classifyFields(result, x.fields(), offset); },
      [&](TupleIndex x) { classifyFields(result, x.fields(), offset); },
      [&](SizedArray x) {
        auto stride = Pool().getSizing(x.dereferencedType).byteSize;
        for (u32 i = 0; i < x.length; ++i) {
          classifyAt(result, x.dereferencedType, offset + i * stride);
        }
      },
      [&](EnumIndex x) { classifyAt(result, x.rawType(), offset); },
      [&](AlignedType x) { classifyAt(result, x.rawType(), offset); },
      [&](Union x) {
        for (auto [variant, _] : x.namedVariants) {
          classifyAt(result, variant, offset);
        }
        for (auto variant : x.anonymousVariants) {
          classifyAt(result, variant, offset);
        }
      },
      [&](auto) {
        throw std::invalid_argument(
          fmt::format(
            "Type '{}' cannot be passed using the x86-64 System V ABI",
            TypeName(type)
          )
        );
      },
    },
    Pool().getType(type)
  );
}

bool requiresAggregateCoercion(TypeIndex type) {
  return std::visit(
    overloaded{
      [](Slice) { return true; },
      [](Union) { return true; },
      []<AggregateType T>(T) { return true; },
      [&](AlignedType x) { return requiresAggregateCoercion(x.rawType()); },
      [](auto) { return false; },
    },
    Pool().getType(type)
  );
}

std::string extensionAttribute(TypeIndex type) {
  return std::visit(
    overloaded{
      [](SignedInt x) {
        return x.bitSize < 32 ? std::string("signext") : std::string();
      },
      [](UnsignedInt x) {
        return x.bitSize < 32 ? std::string("zeroext") : std::string();
      },
      [&](EnumIndex x) { return extensionAttribute(x.rawType()); },
      [&](AlignedType x) { return extensionAttribute(x.rawType()); },
      [](auto) { return std::string(); },
    },
    Pool().getType(type)
  );
}

std::string llvmName(TypeIndex type) {
  return fmt::format("{}", LlvmName(type));
}

std::vector<Leaf> leavesInEightbyte(
  const ClassifiedAggregate& aggregate,
  u32 index
) {
  auto start = index * 8;
  auto end = start + 8;
  std::vector<Leaf> leaves;
  for (auto leaf : aggregate.leaves) {
    if (leaf.offset < end && leaf.offset + leaf.byteSize > start) {
      leaves.push_back(leaf);
    }
  }
  return leaves;
}

AbiComponent integerComponent(
  const ClassifiedAggregate& aggregate,
  u32 index,
  u32 aggregateSize
) {
  auto offset = index * 8;
  auto byteSize = std::min(8u, aggregateSize - offset);
  auto leaves = leavesInEightbyte(aggregate, index);

  if (leaves.size() == 1) {
    auto leaf = leaves.front();
    if (
      leaf.abiClass == EightbyteClass::Integer && leaf.offset == offset &&
      leaf.byteSize <= byteSize
    ) {
      return {offset, leaf.byteSize, llvmName(leaf.type)};
    }
  }

  return {offset, byteSize, fmt::format("i{}", byteSize * 8)};
}

AbiComponent sseComponent(
  const ClassifiedAggregate& aggregate,
  u32 index,
  u32 aggregateSize
) {
  auto offset = index * 8;
  auto byteSize = std::min(8u, aggregateSize - offset);
  auto leaves = leavesInEightbyte(aggregate, index);
  if (leaves.empty()) {
    throw std::invalid_argument("SSE ABI component has no source fields");
  }

  // A union can contribute multiple alternative leaves at the same offset.
  // Prefer a leaf that covers the complete eightbyte, as Clang does for e.g.
  // union { double; float; }, whose coercion type is double.
  for (auto leaf : leaves) {
    if (leaf.offset == offset && leaf.byteSize == byteSize) {
      return {offset, leaf.byteSize, llvmName(leaf.type)};
    }
  }

  if (leaves.size() == 1) {
    auto leaf = leaves.front();
    return {offset, leaf.byteSize, llvmName(leaf.type)};
  }

  TypeIndex smallestType = TypeIndex::null();
  u32 smallestSize = 9;
  for (auto leaf : leaves) {
    auto floatType = Pool().getFloat(leaf.type);
    if (!floatType) {
      throw std::invalid_argument(
        fmt::format(
          "Unsupported SSE aggregate component containing '{}'",
          TypeName(leaf.type)
        )
      );
    }
    if (leaf.byteSize < smallestSize) {
      smallestType = leaf.type;
      smallestSize = leaf.byteSize;
    }
  }

  if (byteSize % smallestSize != 0) {
    throw std::invalid_argument("Unsupported padded SSE aggregate component");
  }
  auto elementCount = byteSize / smallestSize;
  if (elementCount == 1) {
    return {offset, byteSize, llvmName(smallestType)};
  }
  return {
    offset,
    byteSize,
    fmt::format("<{} x {}>", elementCount, LlvmName(smallestType)),
  };
}

u32 pointerAlignment(TypeIndex logicalType, u32 offset) {
  auto baseAlignment = Pool().getSizing(logicalType).alignment.byteAlignment();
  if (offset == 0) return baseAlignment;
  auto offsetAlignment = 1u << std::countr_zero(offset);
  return std::min(baseAlignment, offsetAlignment);
}

std::string pointerAt(
  OutContext& ctx,
  std::string basePointer,
  TypeIndex logicalType,
  u32 offset
) {
  if (offset == 0) return basePointer;
  auto pointer = ctx.environment.addTemporary();
  fmt::println(
    ctx.outputFile,
    "%{} = getelementptr inbounds i8, ptr %{}, i64 {}",
    pointer,
    basePointer,
    offset
  );
  return fmt::format("{}", pointer);
}

void printReturnType(std::ostream& output, const AbiType& abi) {
  if (!abi.extensionAttribute.empty()) {
    output << abi.extensionAttribute << " ";
  }
  output << abi.returnTypeName();
}

void printDirectParameter(
  std::ostream& output,
  const AbiType& abi,
  std::string_view value = {}
) {
  output << LlvmName(abi.type);
  if (!abi.extensionAttribute.empty()) {
    output << " " << abi.extensionAttribute;
  }
  if (!value.empty()) output << " " << value;
}

} // namespace

std::string AbiType::returnTypeName() const {
  switch (kind) {
  case AbiPassKind::Void:
  case AbiPassKind::Memory:
    return "void";
  case AbiPassKind::Direct:
    return llvmName(type);
  case AbiPassKind::Coerce:
    if (components.size() == 1) return components.front().llvmType;
    return fmt::format(
      "{{{}, {}}}",
      components[0].llvmType,
      components[1].llvmType
    );
  }
  throw std::logic_error("Unhandled ABI pass kind");
}

u32 AbiType::parameterCount() const {
  switch (kind) {
  case AbiPassKind::Void:
    return 0;
  case AbiPassKind::Direct:
  case AbiPassKind::Memory:
    return 1;
  case AbiPassKind::Coerce:
    return components.size();
  }
  throw std::logic_error("Unhandled ABI pass kind");
}

AbiType classifySystemVAbi(TypeIndex type) {
  if (Pool().isVoid(type)) return {.type = type, .kind = AbiPassKind::Void};

  if (!requiresAggregateCoercion(type)) {
    return {
      .type = type,
      .kind = AbiPassKind::Direct,
      .extensionAttribute = extensionAttribute(type),
    };
  }

  auto sizing = Pool().getSizing(type);
  if (sizing.byteSize > 16) {
    return {.type = type, .kind = AbiPassKind::Memory};
  }

  ClassifiedAggregate aggregate;
  classifyAt(aggregate, type, 0);
  if (aggregate.memory) return {.type = type, .kind = AbiPassKind::Memory};

  AbiType result{.type = type, .kind = AbiPassKind::Coerce};
  auto count = (sizing.byteSize + 7) / 8;
  for (u32 i = 0; i < count; ++i) {
    switch (aggregate.classes[i]) {
    case EightbyteClass::Integer:
      result.components.push_back(
        integerComponent(aggregate, i, sizing.byteSize)
      );
      break;
    case EightbyteClass::Sse:
      result.components.push_back(sseComponent(aggregate, i, sizing.byteSize));
      break;
    case EightbyteClass::NoClass:
    case EightbyteClass::Memory:
      throw std::invalid_argument(
        fmt::format("Unable to classify ABI eightbyte for '{}'", TypeName(type))
      );
    }
  }
  return result;
}

DeclarationResult declareParamRegisters(
  std::ostream& outputFile,
  Function function
) {
  auto returnAbi = classifySystemVAbi(function.type.returnType);
  printReturnType(outputFile, returnAbi);
  fmt::print(outputFile, " @\"{}\"(", function.globalName);

  u32 parameters = 0;
  bool needsComma = false;
  if (returnAbi.isMemory()) {
    auto sizing = Pool().getSizing(returnAbi.type);
    fmt::print(
      outputFile,
      "ptr sret({}) align {}",
      LlvmName(returnAbi.type),
      sizing.alignment.byteAlignment()
    );
    needsComma = true;
    parameters++;
  }

  for (auto paramType : Pool().tupleElements(function.type.parameters)) {
    auto abi = classifySystemVAbi(paramType);
    if (abi.isVoid()) continue;
    if (needsComma) outputFile << ", ";

    if (abi.isMemory()) {
      auto sizing = Pool().getSizing(paramType);
      fmt::print(
        outputFile,
        "ptr noundef byval({}) align {}",
        LlvmName(paramType),
        sizing.alignment.byteAlignment()
      );
    } else if (abi.isDirect()) {
      printDirectParameter(outputFile, abi);
    } else {
      for (u32 i = 0; i < abi.components.size(); ++i) {
        if (i != 0) outputFile << ", ";
        outputFile << abi.components[i].llvmType;
      }
    }
    parameters += abi.parameterCount();
    needsComma = true;
  }

  outputFile << ")";
  return {.returnAbi = std::move(returnAbi), .entryLabel = parameters};
}

void loadParameterRegisters(
  OutContext& ctx,
  FunctionType function,
  span<Identifier> paramNames
) {
  auto paramTypes = Pool().tupleElements(function.parameters);
  assert(paramTypes.size() == paramNames.size());
  u32 registerIndex =
    classifySystemVAbi(function.returnType).isMemory() ? 1 : 0;

  for (u32 i = 0; i < paramNames.size(); ++i) {
    auto type = paramTypes[i];
    auto abi = classifySystemVAbi(type);
    if (abi.isVoid()) continue;
    auto sizing = Pool().getSizing(type);
    fmt::println(
      ctx.outputFile,
      "%{} = alloca {}, align {}",
      paramNames[i],
      LlvmName(type),
      sizing.alignment.byteAlignment()
    );

    if (abi.isMemory()) {
      fmt::println(
        ctx.outputFile,
        "call void @llvm.memcpy.p0.p0.i8(ptr %{}, ptr %{}, i64 {}, i1 false)",
        paramNames[i],
        registerIndex++,
        sizing.byteSize
      );
    } else if (abi.isDirect()) {
      fmt::println(
        ctx.outputFile,
        "store {} %{}, ptr %{}, align {}",
        LlvmName(type),
        registerIndex++,
        paramNames[i],
        sizing.alignment.byteAlignment()
      );
    } else {
      for (auto& component : abi.components) {
        auto pointer =
          pointerAt(ctx, std::string(paramNames[i]), type, component.offset);
        fmt::println(
          ctx.outputFile,
          "store {} %{}, ptr %{}, align {}",
          component.llvmType,
          registerIndex++,
          pointer,
          pointerAlignment(type, component.offset)
        );
      }
    }
  }
}

u32 callAbiFunctionWithArgs(
  OutContext& ctx,
  Function function,
  span<Reference> args
) {
  auto returnAbi = classifySystemVAbi(function.type.returnType);
  auto returnSizing =
    Pool().isVoid(returnAbi.type) ? Sizing{} : Pool().getSizing(returnAbi.type);
  u32 returnStorage = -1;
  std::stringstream call;
  call << "call ";
  printReturnType(call, returnAbi);
  fmt::print(call, " {}(", Reference(function));

  bool needsComma = false;
  if (returnAbi.isMemory()) {
    returnStorage = ctx.environment.addTemporary();
    fmt::println(
      ctx.outputFile,
      "%{} = alloca {}, align {}",
      returnStorage,
      LlvmName(returnAbi.type),
      returnSizing.alignment.byteAlignment()
    );
    fmt::print(
      call,
      "ptr sret({}) align {} %{}",
      LlvmName(returnAbi.type),
      returnSizing.alignment.byteAlignment(),
      returnStorage
    );
    needsComma = true;
  }

  auto paramTypes = Pool().tupleElements(function.type.parameters);
  assert(paramTypes.size() == args.size());
  for (u32 i = 0; i < args.size(); ++i) {
    auto type = paramTypes[i];
    auto abi = classifySystemVAbi(type);
    if (abi.isVoid()) continue;
    if (needsComma) call << ", ";

    if (abi.isDirect()) {
      std::stringstream value;
      value << args[i];
      printDirectParameter(call, abi, value.str());
    } else {
      auto sizing = Pool().getSizing(type);
      auto storage = ctx.environment.addTemporary();
      fmt::println(
        ctx.outputFile,
        "%{} = alloca {}, align {}",
        storage,
        LlvmName(type),
        sizing.alignment.byteAlignment()
      );
      fmt::println(
        ctx.outputFile,
        "store {} {}, ptr %{}, align {}",
        LlvmName(type),
        args[i],
        storage,
        sizing.alignment.byteAlignment()
      );

      if (abi.isMemory()) {
        fmt::print(
          call,
          "ptr noundef byval({}) align {} %{}",
          LlvmName(type),
          sizing.alignment.byteAlignment(),
          storage
        );
      } else {
        for (u32 componentIndex = 0; componentIndex < abi.components.size();
             ++componentIndex) {
          if (componentIndex != 0) call << ", ";
          auto& component = abi.components[componentIndex];
          auto pointer =
            pointerAt(ctx, fmt::format("{}", storage), type, component.offset);
          auto value = ctx.environment.addTemporary();
          fmt::println(
            ctx.outputFile,
            "%{} = load {}, ptr %{}, align {}",
            value,
            component.llvmType,
            pointer,
            pointerAlignment(type, component.offset)
          );
          fmt::print(call, "{} %{}", component.llvmType, value);
        }
      }
    }
    needsComma = true;
  }
  call << ")\n";

  u32 abiResult = -1;
  if (returnAbi.isVoid() || returnAbi.isMemory()) {
    ctx.outputFile << call.str();
  } else {
    abiResult = ctx.environment.addTemporary();
    fmt::print(ctx.outputFile, "%{} = {}", abiResult, call.str());
  }

  if (returnAbi.isVoid()) return -1;
  if (returnAbi.isDirect()) return abiResult;

  if (!returnAbi.isMemory()) {
    returnStorage = ctx.environment.addTemporary();
    fmt::println(
      ctx.outputFile,
      "%{} = alloca {}, align {}",
      returnStorage,
      LlvmName(returnAbi.type),
      returnSizing.alignment.byteAlignment()
    );
    for (u32 i = 0; i < returnAbi.components.size(); ++i) {
      auto& component = returnAbi.components[i];
      u32 componentValue = abiResult;
      if (returnAbi.components.size() > 1) {
        componentValue = ctx.environment.addTemporary();
        fmt::println(
          ctx.outputFile,
          "%{} = extractvalue {} %{}, {}",
          componentValue,
          returnAbi.returnTypeName(),
          abiResult,
          i
        );
      }
      auto pointer = pointerAt(
        ctx,
        fmt::format("{}", returnStorage),
        returnAbi.type,
        component.offset
      );
      fmt::println(
        ctx.outputFile,
        "store {} %{}, ptr %{}, align {}",
        component.llvmType,
        componentValue,
        pointer,
        pointerAlignment(returnAbi.type, component.offset)
      );
    }
  }

  auto result = ctx.environment.addTemporary();
  fmt::println(
    ctx.outputFile,
    "%{} = load {}, ptr %{}, align {}",
    result,
    LlvmName(returnAbi.type),
    returnStorage,
    returnSizing.alignment.byteAlignment()
  );
  return result;
}

void emitSystemVReturn(
  OutContext& ctx,
  const AbiType& returnAbi,
  Reference value
) {
  auto type = returnAbi.type;
  if (returnAbi.isMemory()) {
    fmt::println(
      ctx.outputFile,
      "store {} {}, ptr %0\nret void",
      LlvmName(type),
      value
    );
    return;
  }
  if (returnAbi.isDirect()) {
    fmt::println(ctx.outputFile, "ret {} {}", LlvmName(type), value);
    return;
  }
  if (!returnAbi.isCoerce()) {
    fmt::println(ctx.outputFile, "ret void");
    return;
  }

  auto sizing = Pool().getSizing(type);
  auto storage = ctx.environment.addTemporary();
  fmt::println(
    ctx.outputFile,
    "%{} = alloca {}, align {}",
    storage,
    LlvmName(type),
    sizing.alignment.byteAlignment()
  );
  fmt::println(
    ctx.outputFile,
    "store {} {}, ptr %{}, align {}",
    LlvmName(type),
    value,
    storage,
    sizing.alignment.byteAlignment()
  );

  if (returnAbi.components.size() == 1) {
    auto& component = returnAbi.components.front();
    auto pointer =
      pointerAt(ctx, fmt::format("{}", storage), type, component.offset);
    auto result = ctx.environment.addTemporary();
    fmt::println(
      ctx.outputFile,
      "%{} = load {}, ptr %{}, align {}",
      result,
      component.llvmType,
      pointer,
      pointerAlignment(type, component.offset)
    );
    fmt::println(ctx.outputFile, "ret {} %{}", component.llvmType, result);
    return;
  }

  u32 aggregate = -1;
  for (u32 i = 0; i < returnAbi.components.size(); ++i) {
    auto& component = returnAbi.components[i];
    auto pointer =
      pointerAt(ctx, fmt::format("{}", storage), type, component.offset);
    auto componentValue = ctx.environment.addTemporary();
    fmt::println(
      ctx.outputFile,
      "%{} = load {}, ptr %{}, align {}",
      componentValue,
      component.llvmType,
      pointer,
      pointerAlignment(type, component.offset)
    );
    auto next = ctx.environment.addTemporary();
    fmt::println(
      ctx.outputFile,
      "%{} = insertvalue {} {}, {} %{}, {}",
      next,
      returnAbi.returnTypeName(),
      i == 0 ? "undef" : fmt::format("%{}", aggregate),
      component.llvmType,
      componentValue,
      i
    );
    aggregate = next;
  }
  fmt::println(
    ctx.outputFile,
    "ret {} %{}",
    returnAbi.returnTypeName(),
    aggregate
  );
}

TEST_CASE("System V aggregate classification") {
  auto [rect, rectIndex] = Pool().makeStruct("abi_rect", "abi_rect");
  Pool().getStruct(rectIndex).fields = {
    {"x", Pool()._s32},
    {"y", Pool()._s32},
    {"w", Pool()._s32},
    {"h", Pool()._s32},
  };
  auto rectAbi = classifySystemVAbi(rect);
  REQUIRE(rectAbi.isCoerce());
  REQUIRE_EQ(rectAbi.components.size(), 2);
  CHECK_EQ(rectAbi.components[0].llvmType, "i64");
  CHECK_EQ(rectAbi.components[1].llvmType, "i64");
  CHECK_EQ(rectAbi.returnTypeName(), "{i64, i64}");

  auto threeBytes = Pool().sizedArrayOf(Pool()._u8, 3);
  auto bytesAbi = classifySystemVAbi(threeBytes);
  REQUIRE_EQ(bytesAbi.components.size(), 1);
  CHECK_EQ(bytesAbi.components[0].llvmType, "i24");

  auto [quat, quatIndex] = Pool().makeStruct("abi_quat", "abi_quat");
  Pool().getStruct(quatIndex).fields = {
    {"x", Pool()._f32},
    {"y", Pool()._f32},
    {"z", Pool()._f32},
    {"w", Pool()._f32},
  };
  auto quatAbi = classifySystemVAbi(quat);
  REQUIRE_EQ(quatAbi.components.size(), 2);
  CHECK_EQ(quatAbi.components[0].llvmType, "<2 x float>");
  CHECK_EQ(quatAbi.components[1].llvmType, "<2 x float>");

  auto floatOrDouble = Pool().addType(
    Union{
      .namedVariants = {
                        {Pool()._f32, "as_float"},
                        {Pool()._f64, "as_double"},
                        },
  }
  );
  auto unionAbi = classifySystemVAbi(floatOrDouble);
  REQUIRE_EQ(unionAbi.components.size(), 1);
  CHECK_EQ(unionAbi.components[0].llvmType, "double");
}

TEST_CASE("System V declaration matches Clang aggregate coercions") {
  auto [rect, rectIndex] = Pool().makeStruct("decl_rect", "decl_rect");
  Pool().getStruct(rectIndex).fields = {
    {"x", Pool()._s32},
    {"y", Pool()._s32},
    {"w", Pool()._s32},
    {"h", Pool()._s32},
  };
  auto params = Pool().tupleOf({rect, Pool()._u8}).second;
  Function function{
    .type = FunctionType{.parameters = params, .returnType = rect},
    .globalName = "rect_roundtrip",
  };
  std::stringstream declaration;
  auto result = declareParamRegisters(declaration, function);
  CHECK_EQ(
    declaration.str(),
    "{i64, i64} @\"rect_roundtrip\"(i64, i64, i8 zeroext)"
  );
  CHECK_EQ(result.entryLabel, 3);
}
