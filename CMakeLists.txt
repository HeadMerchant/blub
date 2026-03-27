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

add_library(libblub ${sources} deps/doctest.h)
target_include_directories(libblub PRIVATE src)

target_compile_options(libblub PRIVATE -Werror=unused-variable)

# option(ENABLE_TESTING "Enable doctest" ON)
# if (NOT ENABLE_TESTING)
#   add_compile_definitions(DOCTEST_CONFIG_DISABLE)
# endif()


# dependencies
include(FetchContent)
FetchContent_Declare(
  fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt
  GIT_TAG        12.0.0)
FetchContent_MakeAvailable(fmt)
target_link_libraries(libblub fmt::fmt)

FetchContent_Declare(
  simdjson
  GIT_REPOSITORY https://github.com/simdjson/simdjson.git
  GIT_TAG  tags/v4.0.7
  GIT_SHALLOW TRUE)

FetchContent_MakeAvailable(simdjson)
target_link_libraries(libblub simdjson)

add_executable(blub src/main.cpp)
target_link_libraries(blub libblub)
add_custom_command(
    TARGET blub POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink
            ${CMAKE_SOURCE_DIR}/lib
            $<TARGET_FILE_DIR:blub>/lib
)

# TODO: put standard library in here
target_compile_definitions(blub PRIVATE
    LIB_BLUB_DIR="${CMAKE_CURRENT_BINARY_DIR}/lib"
)

if(BUILD_TESTS)
    # enable_testing()

    add_executable(tests
        test/test_main.cpp
    )

    target_include_directories(tests PRIVATE deps)
    target_link_libraries(tests PRIVATE libblub)

    # add_test(NAME all_tests COMMAND tests)
endif()
