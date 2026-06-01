#include "common.h"

struct LinkageName {
  struct Frame {
    string_view name;
    u32 anonymousIndex = 0;
  };
  vector<Frame> frames;
  vector<bool> consumed;

  string_view push(string_view name) {
    if (!frames.empty()) {
      auto newName = StringPool::inst().copy(frames.back().name);
      auto length = newName.length();
      length += StringPool::inst().copy(".").length();
      length += StringPool::inst().copy(name).length();
      newName = {newName.data(), newName.length()};
      frames.push_back({newName});
      return newName;
    } else {
      name = StringPool::inst().copy(name);
      frames.push_back({name});
      return name;
    }
  }

  string_view consumeOr(string_view name) {
    if (consumed.empty()) {
      return name;
    }
    if (consumed.back()) {
      return name;
    }
    consumed.back() = true;
    return frames.back().name;
  }

  u32 nextIndex() {
    if (frames.empty()) {
    }
  }
};
