cmake_minimum_required(VERSION 3.30)

# Project: emergia
project(emergia LANGUAGES CXX)

#
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Options
option(EMG_BUILD_DEMOS "Enable build demos" OFF)
option(EMG_BUILD_TESTS "Enable build tests" OFF)

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

add_library(emergia INTERFACE)
add_library(emg ALIAS emergia)
add_library(emg::emg ALIAS emergia)

target_sources(emergia 
    PUBLIC
        FILE_SET emergia_headers
        TYPE HEADERS
        BASE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/src/
        FILES
            ${CMAKE_CURRENT_SOURCE_DIR}/src/memory/bucket.hpp
            ${CMAKE_CURRENT_SOURCE_DIR}/src/memory/pool.hpp
)
target_include_directories(emergia
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
)

if(EMG_BUILD_DEMOS)
    add_subdirectory(demo)
endif()

if(EMG_BUILD_TESTS)
    enable_testing()
    add_subdirectory(test)
endif()
