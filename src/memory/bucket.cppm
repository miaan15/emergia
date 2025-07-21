module;

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <new>
#include <type_traits>

#if defined(__linux__) || defined(__unix__)
#include <sys/mman.h>
#elif defined(_WIN32)
#include <Memoryapi.h>
#include <Windows.h>
#endif

export module emg.memory.bucket;

export namespace emg::memory {

template <std::size_t block_size, std::size_t bucket_size = UINT16_MAX * 2>
class bucket {
public:
    using embedded_list_node_type = std::conditional_t<block_size >= 2, std::uint16_t, std::uint8_t>;
    static constexpr std::size_t const embedded_list_node_type_size = sizeof(embedded_list_node_type);

    static constexpr std::size_t const num_blocks = bucket_size / block_size;

public:
    bucket() {
        // Try to allocate BUCKET_SIZE bytes in virtual memory
#if defined(__linux__) || defined(__unix__)
        void* p = ::mmap(
            nullptr,
            BUCKET_SIZE,
            PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS,
            -1,
            0);

        if (p == MAP_FAILED) {
            throw std::bad_alloc();
        }
#elif defined(_WIN32)
        void* p = ::VirtualAlloc(
            nullptr,
            BUCKET_SIZE,
            MEM_RESERVE | MEM_COMMIT,
            PAGE_READWRITE);

        if (p == nullptr) {
            throw std::bad_alloc();
        }
#else
#error "Not implemented."
#endif

        buffer = static_cast<unsigned char*>(p);
        num_used_blocks = 0;
        first_unused_block = 0;

        for (std::uint16_t i = 0; i < num_blocks; ++i) {
            next_node_in_embedded_list(i) = i + 1;
        }
    }

    ~bucket() {
#if defined(__linux__) || defined(__unix__)
        ::munmap(static_cast<void*>(buffer), BUCKET_SIZE);
#elif defined(_WIN32)
        ::VirtualFree(static_cast<void*>(buffer), 0, MEM_RELEASE);
#else
#error "Not implemented."
#endif
    }

    [[nodiscard]] auto allocate() -> void* {
        assert(num_used_blocks < num_blocks);

        const std::size_t block_idx = first_unused_block;

        first_unused_block = next_node_in_embedded_list(block_idx);

        ++num_used_blocks;

        return static_cast<void*>(buffer + block_idx * block_size);
    }

    auto deallocate(void* p) noexcept -> void {
        assert(num_used_blocks > 0);
        assert(p >= buffer && p < buffer + bucket_size);

        auto* q = static_cast<unsigned char*>(p);

        assert((q - buffer) % block_size == 0);

        const std::size_t block_idx = (q - buffer) / block_size;

        next_node_in_embedded_list(block_idx) = first_unused_block;

        first_unused_block = block_idx;

        --num_used_blocks;
    }

    [[nodiscard]] auto empty() const noexcept -> bool {
        return num_used_blocks == 0;
    }

    [[nodiscard]] auto full() const noexcept -> bool {
        return num_used_blocks == num_blocks;
    }

private:
    [[nodiscard]] auto next_node_in_embedded_list(std::size_t index) const noexcept -> embedded_list_node_type& {
        unsigned char* p = buffer + index * block_size;

        // NOTE: this might break if by some reason embedded list node type > 2 bytes
        if constexpr (embedded_list_node_type_size == 2) {
            return *reinterpret_cast<embedded_list_node_type*>(reinterpret_cast<uintptr_t>(p) % 2 == 0 ? p : p + 1);
        } else if constexpr (embedded_list_node_type_size == 1) {
            return *reinterpret_cast<embedded_list_node_type*>(p);
        } else {
            throw;
        }
    }

private:
    static constexpr std::size_t const BUCKET_SIZE = UINT16_MAX * 2;

    unsigned char* buffer;

    std::uint16_t num_used_blocks;
    std::uint16_t first_unused_block;
};

}  // namespace emg::memory
