message(STATUS "${Yellow}Making demos...${ColorReset}")

add_executable(emergia_demo
    ./main.cpp
)

target_link_libraries(emergia_demo
    PRIVATE 
        emg
)

setup_compiler_flags(emergia_demo)

message(STATUS "${Green}Successfully make demos: ${CMAKE_BINARY_DIR}/demo${ColorReset}")
