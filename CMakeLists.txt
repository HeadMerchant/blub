cmake_minimum_required(VERSION 3.14)

set(CMAKE_C_COMPILER "/usr/bin/clang")
set(CMAKE_CXX_COMPILER "/usr/bin/clang++")

project(blub)

option(BUILD_TESTS "Build tests" ON)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

file(GLOB_RECURSE sources src/*.cpp src/*.h )
list(REMOVE_ITEM sources src/main.cpp)

add_executable(blub ${sources})
target_include_directories(blub PRIVATE src)

target_compile_options(blub PRIVATE -Werror=unused-variable -Werror=implicit-fallthrough)

# dependencies
include(FetchContent)
FetchContent_Declare(
  fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt
  GIT_TAG        12.0.0
  GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(fmt)

FetchContent_Declare(
  simdjson
  GIT_REPOSITORY https://github.com/simdjson/simdjson.git
  GIT_TAG  tags/v4.0.7
  GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(simdjson)

FetchContent_Declare(
  ordered_map
  GIT_REPOSITORY https://github.com/Tessil/ordered-map.git
  GIT_TAG        tags/v1.2.0
  GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(ordered_map)

FetchContent_Declare(
  doctest
  GIT_REPOSITORY https://github.com/doctest/doctest/
  GIT_TAG tags/v2.5.2
  GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(doctest)

target_link_libraries(blub
  PRIVATE
    fmt::fmt
    simdjson
    tsl::ordered_map
    doctest
)


add_custom_command (
    TARGET blub POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink
            ${CMAKE_SOURCE_DIR}/lib
            $<TARGET_FILE_DIR:blub>/lib
)

# TODO: put standard library in here
target_compile_definitions(blub PRIVATE
    LIB_BLUB_DIR="${CMAKE_CURRENT_BINARY_DIR}/lib"
)
