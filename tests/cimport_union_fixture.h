#pragma once

struct ci_leaf {
  int leaf_value;
};

union ci_top_union {
  int as_int;
  float as_float;
};

struct ci_with_anon_union {
  union {
    int int_value;
    float float_value;
    struct ci_leaf leaf;
  };
  int tag;
};

struct ci_with_anon_struct {
  struct {
    int x;
    int y;
  };
  int z;
};

struct ci_with_nested_both {
  union {
    struct {
      int left;
      int right;
    };
    int pair[2];
  };
};
