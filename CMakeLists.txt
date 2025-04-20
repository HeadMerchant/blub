cmake_minimum_required(VERSION 3.12)

project(blub)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

file(GLOB_RECURSE sources src/*.cpp src/*.h)

include_directories(src)
add_executable(blub ${sources})
