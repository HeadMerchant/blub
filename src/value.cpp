#include "types.h"
#include "value.h"

Environment* Environment::baseEnvironment() {
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
  }
  );

  return &baseEnvironment;
}

u32 Environment::globalIndex = 0;
u32 Environment::nextGlobalTemporary = 1;
