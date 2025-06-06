cmake_minimum_required(VERSION 3.12)

project(blub)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

file(GLOB_RECURSE sources src/*.cpp src/*.h)

include_directories(src)
add_executable(blub ${sources})
