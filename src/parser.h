#pragma once
#include "tokenizer.h"
#include <vector>

class Parser {
    private:
        const std::vector<Token>& tokens;
        int current = 0;
};
