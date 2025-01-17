#include "greeting.h"
#include "filereader.h"
#include <iostream>

int main() {
    printGreeting();
    std::cout << readFile() << std::endl;
}