#include "types.h"
#include "value.h"

Environment* Environment::baseEnvironment() {
  static Types::TypePool& typePool = Types::Pool();
  // typePool.debugTypes();
  static Environment baseEnvironment(
    std::unordered_map<Identifier, Reference>{
      {"bool",  Reference(Types::Pool()._bool) },
      {"s8",    Reference(Types::Pool()._s8)   },
      {"s16",   Reference(Types::Pool()._s16)  },
      {"s32",   Reference(Types::Pool()._s32)  },
      {"s64",   Reference(Types::Pool()._s64)  },
      {"u8",    Reference(Types::Pool()._u8)   },
      {"u16",   Reference(Types::Pool()._u16)  },
      {"u32",   Reference(Types::Pool()._u32)  },
      {"u64",   Reference(Types::Pool()._u64)  },
      {"f16",   Reference(Types::Pool()._f16)  },
      {"f32",   Reference(Types::Pool()._f32)  },
      {"f64",   Reference(Types::Pool()._f64)  },
      {"usize", Reference(Types::Pool()._usize)},
      {"isize", Reference(Types::Pool()._isize)},
      {"void",  Reference(Types::Pool()._void) },
  });
  // auto s32 = baseEnvironment.find("s32").value()->unboxType();
  // auto actualS32 = Types::Pool().s32;
  // for (auto [name, value] : baseEnvironment.defs) {
  //   fmt::println("{} be looking like {}", name, value);
  // }
  // fmt::println("S32 be looking like {}:{}", s32.value().value, TypeName(actualS32));

  return &baseEnvironment;
}

u32 Environment::globalIndex = 0;
u32 Environment::nextGlobalTemporary = 1;
