#pragma once
#include "common.h"
#include <concepts>
#include <sys/mman.h>

template <typename T>
concept Index = std::same_as<T, u32> || std::same_as<T, u64>;

template <typename T, Index U> struct VirtualArena {
  T* data;
  U capacity;
  U size = 0;

  explicit VirtualArena(U capacity) : capacity(capacity) {
    auto bytes = static_cast<usize>(capacity) * sizeof(T);
    void* memory = mmap(
      nullptr,
      bytes,
      PROT_READ | PROT_WRITE,
      MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE,
      -1,
      0
    );
    if (memory == MAP_FAILED) throw std::bad_alloc();
    data = static_cast<T*>(memory);
  }

  T& push(T value) {
    if (size == capacity)
      throw std::invalid_argument("Virtual arena exhausted");
    return data[size++] = value;
  }

  T& operator[](u32 index) {
    return data[index];
  }

  const T& operator[](u32 index) const {
    return data[index];
  }
};

struct LinkageName {
  u32 index = 0;

  LinkageName() = default;
  LinkageName(Identifier identifier);
  LinkageName(std::string_view name);
  LinkageName(const char* name) : LinkageName(std::string_view(name)) {}
  static LinkageName fromIndex(u32 index) {
    LinkageName result;
    result.index = index;
    return result;
  }
  explicit operator bool() const {
    return index != 0;
  }
  bool operator==(const LinkageName&) const = default;
};

template <> struct std::hash<LinkageName> {
  size_t operator()(LinkageName name) const {
    return name.index;
  }
};

struct LinkageNames {
  static constexpr u32 AnonymousBit = 1u << 31;
  static constexpr u32 PayloadMask = ~AnonymousBit;
  static constexpr u32 Capacity = 16u << 20;

  struct Node {
    u32 part;
    u32 parent;
  };
  static_assert(sizeof(Node) == 8);

  VirtualArena<Node, u32> nodes{Capacity};
  std::unordered_map<u64, u32> nodeIndices;
  static inline thread_local char outputSeparator = '.';

  LinkageNames() {
    nodes.push({});
  }

  static LinkageNames& inst() {
    static LinkageNames names;
    return names;
  }

  static void setOutputSeparator(char separator) {
    outputSeparator = separator;
  }

  LinkageName append(LinkageName parent, Identifier identifier) {
    if (identifier.index >= AnonymousBit) {
      throw std::invalid_argument("Identifier does not fit in linkage node");
    }
    return intern(parent, identifier.index);
  }

  LinkageName appendAnonymous(LinkageName parent, u32 index) {
    if (index > PayloadMask) {
      throw std::invalid_argument("Anonymous linkage index does not fit");
    }
    return intern(parent, AnonymousBit | index);
  }

  LinkageName intern(LinkageName parent, u32 part) {
    u64 key = (static_cast<u64>(parent.index) << 32) | part;
    if (auto found = nodeIndices.find(key); found != nodeIndices.end()) {
      return LinkageName::fromIndex(found->second);
    }
    u32 index = nodes.size;
    nodes.push({.part = part, .parent = parent.index});
    nodeIndices.emplace(key, index);
    return LinkageName::fromIndex(index);
  }

  void write(std::ostream& output, LinkageName name, char separator) const {
    u32 parts[256];
    u32 count = 0;
    while (name) {
      if (count == 256) {
        throw std::invalid_argument("Linkage name nesting is too deep");
      }
      const auto& node = nodes[name.index];
      parts[count++] = node.part;
      name.index = node.parent;
    }
    bool needsSeparator = false;
    while (count != 0) {
      if (needsSeparator) output.put(separator);
      needsSeparator = true;
      u32 part = parts[--count];
      if (part & AnonymousBit) {
        output << (part & PayloadMask);
      } else {
        output << static_cast<std::string_view>(Identifier(part));
      }
    }
  }

  template <typename OutputIt>
  OutputIt writeTo(OutputIt output, LinkageName name, char separator) const {
    u32 parts[256];
    u32 count = 0;
    while (name) {
      if (count == 256) {
        throw std::invalid_argument("Linkage name nesting is too deep");
      }
      const auto& node = nodes[name.index];
      parts[count++] = node.part;
      name.index = node.parent;
    }
    bool needsSeparator = false;
    while (count != 0) {
      if (needsSeparator) *output++ = separator;
      needsSeparator = true;
      u32 part = parts[--count];
      if (part & AnonymousBit) {
        output = fmt::format_to(output, "{}", part & PayloadMask);
      } else {
        output = fmt::format_to(output, "{}", Identifier(part));
      }
    }
    return output;
  }

  usize length(LinkageName name, char separator) const {
    usize result = 0;
    bool needsSeparator = false;
    u32 parts[256];
    u32 count = 0;
    while (name) {
      parts[count++] = nodes[name.index].part;
      name.index = nodes[name.index].parent;
    }
    while (count != 0) {
      if (needsSeparator) result++;
      needsSeparator = true;
      u32 part = parts[--count];
      if (part & AnonymousBit) {
        u32 value = part & PayloadMask;
        do {
          result++;
          value /= 10;
        } while (value != 0);
      } else {
        result += Identifier(part).size();
      }
    }
    return result;
  }
};

inline LinkageName::LinkageName(Identifier identifier)
    : index(LinkageNames::inst().append({}, identifier).index) {}

inline LinkageName::LinkageName(std::string_view name)
    : LinkageName(Identifier(name)) {}

template <> struct fmt::formatter<LinkageName> : fmt::formatter<string_view> {
  template <typename FormatContext>
  auto format(LinkageName name, FormatContext& context) const {
    return LinkageNames::inst()
      .writeTo(context.out(), name, LinkageNames::outputSeparator);
  }
};
