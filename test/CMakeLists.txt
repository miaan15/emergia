message(STATUS "${Yellow}Making tests...${ColorReset}")
FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG v1.15.2
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_GetProperties(googletest)
message(STATUS "${Yellow}Fetching GoogleTest...${ColorReset}")
FetchContent_MakeAvailable(googletest)
message(STATUS "${Green}GoogleTest fetched to: ${googletest_SOURCE_DIR}${ColorReset}")

# Test files
add_executable(emergia_test
    ./memory/allocators-test.cpp
)

target_link_libraries(emergia_test
    PRIVATE 
        emg
        GTest::gtest_main
)

setup_compiler_flags(emergia_test)

add_test(
    NAME emergia_test 
    COMMAND emergia_test
)

message(STATUS "${Green}Successfully make tests: ${CMAKE_BINARY_DIR}/test${ColorReset}")
