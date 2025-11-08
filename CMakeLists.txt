cmake_minimum_required(VERSION 3.12)

set(CMAKE_C_COMPILER "/usr/bin/clang")
set(CMAKE_CXX_COMPILER "/usr/bin/clang++")

project(blub)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

file(GLOB_RECURSE sources src/*.cpp src/*.h)

include_directories(src)
add_executable(blub ${sources})


# dependencies
include(FetchContent)
FetchContent_Declare(
  fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt
  GIT_TAG        12.0.0)
FetchContent_MakeAvailable(fmt)
target_link_libraries(blub fmt::fmt)

FetchContent_Declare(
  simdjson
  GIT_REPOSITORY https://github.com/simdjson/simdjson.git
  GIT_TAG  tags/v4.0.7
  GIT_SHALLOW TRUE)
 
FetchContent_MakeAvailable(simdjson)
target_link_libraries(blub simdjson)
