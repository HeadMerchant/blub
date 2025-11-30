#include "types.h"
#include "value.h"

Environment* Environment::baseEnvironment() {
  static Types::TypePool& typePool = Types::Pool();
  // typePool.debugTypes();
  static Environment baseEnvironment(
    std::unordered_map<Identifier, Reference>{
      {"bool",  Reference(Types::Pool()._bool)},
      {"s8",    Reference(Types::Pool().s8)   },
      {"s16",   Reference(Types::Pool().s16)  },
      {"s32",   Reference(Types::Pool().s32)  },
      {"s64",   Reference(Types::Pool().s64)  },
      {"u8",    Reference(Types::Pool().u8)   },
      {"u16",   Reference(Types::Pool().u16)  },
      {"u32",   Reference(Types::Pool().u32)  },
      {"u64",   Reference(Types::Pool().u64)  },
      {"f16",   Reference(Types::Pool().f16)  },
      {"f32",   Reference(Types::Pool().f32)  },
      {"f64",   Reference(Types::Pool().f64)  },
      {"usize", Reference(Types::Pool().usize)},
      {"isize", Reference(Types::Pool().isize)},
  });
  // auto s32 = baseEnvironment.find("s32").value()->unboxType();
  // auto actualS32 = Types::Pool().s32;
  // for (auto [name, value] : baseEnvironment.defs) {
  //   fmt::println("{} be looking like {}", name, value);
  // }
  // fmt::println("S32 be looking like {}:{}", s32.value().value, TypeName(actualS32));

  return &baseEnvironment;
}

i32 Environment::globalIndex = 0;

