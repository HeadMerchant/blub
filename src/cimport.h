#include "common.h"
#include "interpreter/value.h"
#include "types.h"
#include <filesystem>
#include <unordered_map>
#pragma once

using TypeCache = std::unordered_map<std::string_view, TypeIndex>;

// TODO: proper lexing+parsing: see
// https://github.com/nothings/stb/blob/master/stb_c_lexer.h
TypeIndex parseType(std::string_view qualType, TypeCache& cTypes, std::ostream& o);
Environment* cBindings(fs::path cFile, std::string prefix, std::ostream& outputFile);
