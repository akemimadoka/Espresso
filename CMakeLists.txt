cmake_minimum_required(VERSION 3.20)

project(Espresso CXX)

option(ESPRESSO_INCLUDE_TESTS "Include tests" OFF)

add_library(Espresso INTERFACE)

target_compile_features(Espresso INTERFACE cxx_std_23)
target_include_directories(Espresso INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
    $<INSTALL_INTERFACE:include>)

if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    target_compile_options(Espresso INTERFACE /utf-8)
endif()

if(ESPRESSO_INCLUDE_TESTS)
    add_executable(EspressoTest test/Main.cpp)

    target_link_libraries(EspressoTest PRIVATE Espresso)
endif()

install(TARGETS Espresso
    EXPORT Espresso)

install(DIRECTORY src/
    TYPE INCLUDE
    FILES_MATCHING PATTERN "*.h")

install(EXPORT Espresso
    DESTINATION cmake/Espresso)
