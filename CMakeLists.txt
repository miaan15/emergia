cmake_minimum_required(VERSION 3.14) # Version 3.14 allow to FetchContent_MakeAvailable

# Project: emergia
project(emergia LANGUAGES CXX)

#
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Demo
add_executable(emg_demo)
target_sources(emg_demo
    PRIVATE
        demo/main.cpp)

