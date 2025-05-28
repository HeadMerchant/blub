#include <sstream>
#include "environment.h"

InterpreterValue *print(std::span<InterpreterValue *> args) {
  if (args.size() != 1) {
    std::stringstream ss;
    ss << "Needed 1 arg for print; found: " << args.size() << "\n";
    for (auto arg : args) {
      ss << *arg->toString()->string() << ", ";
    }

    throw std::invalid_argument(ss.str());
  }
  StringType text = *args[0]->toString()->string();
  std::cout << text << "\n";
  return nullptr;
};
NativeFunction printWrapper = print;
InterpreterValue printValue = InterpreterValue(&printWrapper);

Environment Environment::baseEnvironment = Environment{.defs = {{"print", &printValue}}, .parent = nullptr};
