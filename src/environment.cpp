#include "environment.h"
#include "interpreter/value.h"

Reference *print(std::span<Reference *> args) {
  if (args.size() != 1) {
    std::stringstream ss;
    ss << "Needed 1 arg for print; found: " << args.size() << "\n";
    for (auto arg : args) {
      ss << std::get<StringType>(arg->toString()->value) << ", ";
    }

    throw std::invalid_argument(ss.str());
  }
  std::string text = std::get<StringType>(args[0]->toString()->value);
  std::cout << text << "\n";
  return nullptr;
};

std::function<Reference* (std::span<Reference*>)> printWrapper(print);
Reference printValue = Reference(&printWrapper);

Environment Environment::baseEnvironment = Environment(Map {
  {"print", &printValue},
  {"float", Reference::toType(Types::Intrinsic::FLOAT)},
  {"int", Reference::toType(Types::Intrinsic::INT)},
  {"bool", Reference::toType(Types::Intrinsic::BOOL)},
  {"string", Reference::toType(Types::Intrinsic::STRING)},
});
