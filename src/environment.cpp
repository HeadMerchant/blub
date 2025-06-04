#include "environment.h"
#include "interpreter/value.h"

Reference *print(std::span<Reference *> args) {
  if (args.size() != 1) {
    std::stringstream ss;
    ss << "Needed 1 arg for print; found: " << args.size() << "\n";
    for (auto arg : args) {
      ss << *arg->toString()->string() << ", ";
    }

    throw std::invalid_argument(ss.str());
  }
  std::string text = *args[0]->toString()->string();
  std::cerr << text << "\n";
  return nullptr;
};

std::function<Reference* (std::span<Reference*>)> printWrapper(print);
Reference printValue = Reference(&printWrapper);

Environment Environment::baseEnvironment = Environment(Map {
  {"print", &printValue},
  {"float", Reference::intrinsicType(ValueType::FLOAT)},
  {"int", Reference::intrinsicType(ValueType::FLOAT)},
  {"bool", Reference::intrinsicType(ValueType::BOOL)},
  {"string", Reference::intrinsicType(ValueType::STRING)},
});
