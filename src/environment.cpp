#include "environment.h"
#include "interpreter/value.h"
#include "types.h"

Environment* Environment::baseEnvironment() {
  static Environment baseEnvironment(Map {
    {"printf", new Reference(
      std::move(LLVMFunction{.definition = "declare i32 @printf(ptr noalias nocapture, ...)", .usage = "i32 (i8*, ...) @printf"})
    )},
    {"malloc", new Reference(
      std::move(LLVMFunction{.definition = "declare ptr @malloc(i64)", .usage = "ptr (i64) @malloc"})
    )},
    {"free", new Reference(
      std::move(LLVMFunction{.definition = "declare void @free(ptr)", .usage = "void (ptr) @free"})
    )},
    {"bool", Reference::typeReference(Types::Pool().boolean)},
    {"s8", Reference::typeReference(Types::Pool().s8)},
    {"s16", Reference::typeReference(Types::Pool().s16)},
    {"s32", Reference::typeReference(Types::Pool().s32)},
    {"s64", Reference::typeReference(Types::Pool().s64)},
    {"u8", Reference::typeReference(Types::Pool().u8)},
    {"u16", Reference::typeReference(Types::Pool().u16)},
    {"u32", Reference::typeReference(Types::Pool().u32)},
    {"u64", Reference::typeReference(Types::Pool().u64)},
    {"f16", Reference::typeReference(Types::Pool().f16)},
    {"f32", Reference::typeReference(Types::Pool().f32)},
    {"f64", Reference::typeReference(Types::Pool().f64)},
    {"usize", Reference::typeReference(Types::Pool().usize)},
    {"isize", Reference::typeReference(Types::Pool().isize)},
  });
  auto s32 = baseEnvironment.find("s32").value()->unboxType();
  auto actualS32 = Types::Pool().s32;

  return &baseEnvironment;
}
