#include <iostream>
#pragma once

enum class LogLevel { DEBUG, ERROR };

std::ostream& logger(LogLevel level);
