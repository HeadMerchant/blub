#include "environment.h"
#include "interpreter/value.h"
#include "types.h"

Environment* Environment::baseEnvironment() {
  static Environment baseEnvironment(
    std::unordered_map<Identifier, Reference>{
      // {"printf", new Reference(
      //   std::move(LLVMFunction{.definition = "declare i32 @printf(ptr noalias
      //   nocapture, ...)", .usage = "i32 (i8*, ...) @printf"})
      // )},
      // {"malloc", new Reference(
      //   std::move(LLVMFunction{.definition = "declare ptr @malloc(i64)",
      //   .usage = "ptr (i64) @malloc"})
      // )},
      // {"free", new Reference(
      //   std::move(LLVMFunction{.definition = "declare void @free(ptr)",
      //   .usage = "void (ptr) @free"})
      // )},
      {"bool",  Reference(Types::Pool()._bool)},
      {"s8",    Reference(Types::Pool().s8)     },
      {"s16",   Reference(Types::Pool().s16)    },
      {"s32",   Reference(Types::Pool().s32)    },
      {"s64",   Reference(Types::Pool().s64)    },
      {"u8",    Reference(Types::Pool().u8)     },
      {"u16",   Reference(Types::Pool().u16)    },
      {"u32",   Reference(Types::Pool().u32)    },
      {"u64",   Reference(Types::Pool().u64)    },
      {"f16",   Reference(Types::Pool().f16)    },
      {"f32",   Reference(Types::Pool().f32)    },
      {"f64",   Reference(Types::Pool().f64)    },
      {"usize", Reference(Types::Pool().usize)  },
      {"isize", Reference(Types::Pool().isize)  },
  });
  // auto s32 = baseEnvironment.find("s32").value()->unboxType();
  // auto actualS32 = Types::Pool().s32;

  return &baseEnvironment;
}
