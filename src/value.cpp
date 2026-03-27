#include "types.h"
#include "value.h"

Environment* Environment::baseEnvironment() {
  static Environment baseEnvironment(
    std::unordered_map<Identifier, Reference>{
      {"bool",  Reference(Pool()._bool) },
      {"s8",    Reference(Pool()._s8)   },
      {"s16",   Reference(Pool()._s16)  },
      {"s32",   Reference(Pool()._s32)  },
      {"s64",   Reference(Pool()._s64)  },
      {"u8",    Reference(Pool()._u8)   },
      {"u16",   Reference(Pool()._u16)  },
      {"u32",   Reference(Pool()._u32)  },
      {"u64",   Reference(Pool()._u64)  },
      {"f16",   Reference(Pool()._f16)  },
      {"f32",   Reference(Pool()._f32)  },
      {"f64",   Reference(Pool()._f64)  },
      {"usize", Reference(Pool()._usize)},
      {"isize", Reference(Pool()._isize)},
      {"void",  Reference(Pool()._void) },
  }
  );

  return &baseEnvironment;
}

u32 Environment::globalIndex = 0;
u32 Environment::nextGlobalTemporary = 1;
