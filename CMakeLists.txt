cmake_minimum_required(VERSION 3.14) # Version 3.14 allow to FetchContent_MakeAvailable

# Project: emergia
project(emergia LANGUAGES CXX)

#
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Include modules
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/compiler-flags.cmake")
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/message-color.cmake")

# Fetch dependencies
include(FetchContent)
set(FETCHCONTENT_QUIET OFF)

# glad
message(STATUS "${Yellow}Fetching glad...${ColorReset}")
FetchContent_Declare(
    glad
    GIT_REPOSITORY https://github.com/Dav1dde/glad.git
    GIT_TAG v2.0.8
)
FetchContent_GetProperties(glad)
FetchContent_MakeAvailable(glad)
message(STATUS "${Green}glad fetched to: ${glad_SOURCE_DIR}${ColorReset}")

# GLFW
message(STATUS "${Yellow}Fetching GLFW...${ColorReset}")
FetchContent_Declare(
    glfw
    GIT_REPOSITORY https://github.com/glfw/glfw.git
    GIT_TAG 3.4
)
FetchContent_GetProperties(glfw)
FetchContent_MakeAvailable(glfw)
message(STATUS "${Green}GLFW fetched to: ${glfw_SOURCE_DIR}${ColorReset}")

# glm
message(STATUS "${Yellow}Fetching glm...${ColorReset}")
FetchContent_Declare(
    glm
    GIT_REPOSITORY https://github.com/g-truc/glm.git
    GIT_TAG 1.0.1
)
FetchContent_GetProperties(glm)
FetchContent_MakeAvailable(glm)
message(STATUS "${Green}glm fetched to: ${glm_SOURCE_DIR}${ColorReset}")

# spdlog
message(STATUS "${Yellow}Fetching spdlog...${ColorReset}")
FetchContent_Declare(
    spdlog
    GIT_REPOSITORY https://github.com/gabime/spdlog.git
    GIT_TAG v1.15.3
)
FetchContent_GetProperties(spdlog)
FetchContent_MakeAvailable(spdlog)
message(STATUS "${Green}spdlog fetched to: ${spdlog_SOURCE_DIR}${ColorReset}")

# Demo
add_executable(emg_demo)
target_sources(emg_demo
    PRIVATE
        demo/main.cpp)

setup_compiler_flags(emg_demo)
