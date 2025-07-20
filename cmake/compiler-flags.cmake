function(setup_compiler_flags target)
    target_compile_options(${target} PRIVATE
        $<$<CONFIG:Debug>:
            -Wall
            -Werror
            -Wextra
            -Wpedantic
            -Wconversion
            -Wcast-align
            -Wunused
            -Wshadow
            -Wold-style-cast
            -Wuninitialized
            -Woverloaded-virtual
            -Wsign-conversion
            -fno-omit-frame-pointer
        >
    )
endfunction()
