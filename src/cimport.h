#include "types.h"
#include "value.h"
#include <filesystem>
#include <unordered_map>
#pragma once

using TypeCache = std::unordered_map<std::string_view, TypeIndex>;

// TODO: proper lexing+parsing: see
// https://github.com/nothings/stb/blob/master/stb_c_lexer.h
TypeIndex parseType(std::string_view qualType, TypeCache& cTypes, std::queue<std::string>& globals);
Environment* cBindings(fs::path cFile, std::string prefix, std::queue<std::string>& globals);
