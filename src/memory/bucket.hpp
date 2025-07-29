// ==================================================================================================
// The Bucket is a (kind of) allocator that manages a fixed-size chunk of memory, divided into an
// integral number of equal-sized blocks called Blocks, all Block have the same size.
//
//   Memory allocations are performed in units of Blocks: allocating n Blocks reserves n * Size
//   bytes of contiguous memory.
//
//   Tracks freed (not allocated or deallocated) Blocks internally to reduce fragmentation.
//
//   There is no memory overhead (except when Size < embeded-list's Node size).
//
// "How this work" (preliminary)
//
//   Freed Blocks are used to store a Node structure for forming an embedded singly linked list of
//   available (freed) Blocks.
//
//   On allocation, we traverses the embedded list to find a large enough Block chunk and return
//
//   On deallocation, the deallocated Block chunk is wrapped as a series of Nodes and inserted at
//   the front of the embedded list.
//
//   The embedded list is updated accordingly in both action.
//
// Detail
// TODO:
//
// Definition
//
//   Block and Node are actually pretty much the same, they are represent in a same block of memory,
//   or Node is embedded in the memory of the Block, Node is existed only if the Block is freed.
//
//   A Block chunk is a group of continuous freed Blocks
// ==================================================================================================
#ifndef _EMG_MEMORY_BUCKET_HPP
#define _EMG_MEMORY_BUCKET_HPP

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <new>
#include <ranges>

#if defined(__linux__) || defined(__unix__)
#include <sys/mman.h>
#elif defined(_WIN32)
#include <Memoryapi.h>
#include <Windows.h>
#endif

// TODO: benchmark to find the best number
#ifndef EMG_DEFAULT_BUCKET_SIZE
#define EMG_DEFAULT_BUCKET_SIZE INT16_MAX - 1 // (= maxint16 * 4 - 1)
#endif

#ifndef EMG_BUCKET_DEFRAC_ATTEMPT
#define EMG_BUCKET_DEFRAC_ATTEMPT 6
#endif

namespace emg::memory {

/**
 * @tparam Size Size of each block in bytes.
 * @tparam BucketSize Size of the memory the bucket holds, in bytes.
 *
 * @note BucketSize defaults to EMG_MAX_BUCKET_SIZE.
 * @requires BucketSize <= EMG_MAX_BUCKET_SIZE
 */
template <std::size_t Size, std::size_t BucketSize = EMG_DEFAULT_BUCKET_SIZE>
  requires(BucketSize <= Size * INT16_MAX)
class bucket {
public:
  using size_type = std::size_t;
  using block_type = std::uint16_t; // Block is just an index

private:
  // The embedded list is like a Single linked list
  // next_block is the index of the next freed Block that a Node points to
  // If it's the begin node of a Block chunk, flag is the size of that chunk, that value is
  // negative. If it's the end of the chunk. Otherwise, flag is undefined
  struct embedded_node {
    block_type next_block;
    std::int16_t flag;
  };
  using node_flag_type = decltype(std::declval<embedded_node>().flag);

public:
  static constexpr size_type BlockSize
      = std::max(Size, sizeof(embedded_node)); // because Node will be embedded in a Block
  static constexpr size_type TotalNumBlocks = BucketSize / BlockSize;

public:
  bucket() {
    // NOTE: This implement explicit need alignment of embedded_node to
    // be = 2, if not... well
    static_assert(alignof(embedded_node) == 2, "this implement explicit need alignment of "
                                               "embedded_node to be = 2");

    // Allocate BucketSize bytes in virtual memory
#if defined(__linux__) || defined(__unix__)
    void *p
        = ::mmap(nullptr, BucketSize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (p == MAP_FAILED) {
      throw std::bad_alloc();
    }
#elif defined(_WIN32)
    void *p = ::VirtualAlloc(nullptr, BucketSize, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);

    if (p == nullptr) {
      throw std::bad_alloc();
    }
#else
#error "Not implemented."
#endif

    buffer = static_cast<std::byte *>(p);
    num_used_blocks = 0;
    current_freed_block = 0;

    // At first, the whole memory is a big Block chunk
    get_node_from_block(0)->flag = static_cast<node_flag_type>(TotalNumBlocks);
    get_node_from_block(TotalNumBlocks - 1)->flag = static_cast<node_flag_type>(-TotalNumBlocks);
  }

  // No copying
  bucket(const bucket &) = delete;
  auto operator=(const bucket &) -> bucket & = delete;

  // but yes moving
  constexpr bucket(bucket &&other) noexcept
      : buffer(other.buffer), num_used_blocks(other.num_used_blocks),
        current_freed_block(other.current_freed_block) {
    other.buffer = nullptr;
    other.num_used_blocks = 0;
    other.current_freed_block = 0;
  }
  constexpr auto operator=(bucket &&other) noexcept -> bucket & {
    if (this != &other) {
      buffer = other.buffer;
      num_used_blocks = other.num_used_blocks;
      current_freed_block = other.current_freed_block;

      other.buffer = nullptr;
      other.num_used_blocks = 0;
      other.current_freed_block = 0;
    }
    return *this;
  }

  ~bucket() {
#if defined(__linux__) || defined(__unix__)
    ::munmap(static_cast<void *>(buffer), BucketSize);
#elif defined(_WIN32)
    ::VirtualFree(static_cast<void *>(buffer), 0, MEM_RELEASE);
#else
#error "Not implemented."
#endif
  }

  [[nodiscard]] auto allocate(size_type n) -> void * {
    // ==============================================================================================
    // The strategy here is pretty much the same as a single-linked list (expect the flag), we took
    // the suited Blocks chunk and then return it, then we link the previous Node to the next Node
    // of that Blocks chunk, then we update the Nodes' flag: when allocate, these allocated blocks
    // are always at the beginning of a Block chunk, so to keep the embedded list valid, we just
    // need to update the flags of the Node on the next Block after these allocated blocks and on
    // the Block at the end of the chunk
    // ==============================================================================================

    embedded_node *current_suited_node = get_node_from_block(current_freed_block);
    block_type current_suited_block = current_freed_block;

    // Search through the embedded list until found the suited Blocks
    embedded_node *previous_node = nullptr; // save the previous Node for the linked list
    while (current_suited_node->flag < static_cast<node_flag_type>(n)) {
      previous_node = current_suited_node;

      current_suited_block = current_suited_node->next_block;
      current_suited_node = get_node_from_block(current_suited_block);
    }

    block_type next_block_after_suited_chunk
        = n > 1 ? get_node_from_block(current_suited_block + static_cast<block_type>(n - 1))
                      ->next_block
                : current_suited_node->next_block;
    assert(next_block_after_suited_chunk <= TotalNumBlocks);

    if (previous_node != nullptr) {
      previous_node->next_block = next_block_after_suited_chunk; // preNode now points to nextNode
    }

    // Later, we will remove n Block of the suited Block chunk from the embedded list, so we need to
    // update the rest of the Block chunk flags.
    // We update only if the suited Block chunk is a sub-chunk of a bigger chunk
    if (next_block_after_suited_chunk - current_freed_block == static_cast<block_type>(n)) {
      embedded_node *next_node_after_suited_chunk
          = get_node_from_block(next_block_after_suited_chunk);
      next_node_after_suited_chunk->flag
          = current_suited_node->flag - static_cast<node_flag_type>(n);

      if (next_node_after_suited_chunk->flag > 1) {
        embedded_node *end_of_chunk_node = get_node_from_block(
            next_block_after_suited_chunk
            + static_cast<block_type>(next_node_after_suited_chunk->flag - 1));
        end_of_chunk_node->flag = -(next_node_after_suited_chunk->flag);
      }
    }

    // Attach the new first Node, if the suited Block we found is not the first freed Block then
    // nothing happens
    if (current_freed_block == current_suited_block) {
      current_freed_block = next_block_after_suited_chunk;
    }

    num_used_blocks += static_cast<uint16_t>(n);

    return static_cast<void *>(buffer + current_suited_block * BlockSize);
  }

  auto deallocate(void *p, size_type n) -> void {
    // ==============================================================================================
    // TODO:
    // ==============================================================================================
    assert(p >= buffer && static_cast<std::byte *>(p) + n < buffer + BucketSize);

    auto byte_p = static_cast<std::byte *>(p); // cast to std::byte * for arithmetic calculation

    assert(static_cast<block_type>(byte_p - buffer) % BlockSize == 0); // if not aligned with
                                                                       // block

    block_type first_block_of_allocated_chunk
        = static_cast<block_type>(byte_p - buffer) / BlockSize;
    embedded_node *first_node_of_allocated_chunk
        = get_node_from_block(first_block_of_allocated_chunk);
    embedded_node *end_node_of_allocated_chunk
        = get_node_from_block(first_block_of_allocated_chunk + static_cast<block_type>(n - 1));

    // Set flags for allocated chunk
    first_node_of_allocated_chunk->flag = static_cast<node_flag_type>(n);
    end_node_of_allocated_chunk->flag = static_cast<node_flag_type>(-n);
    // Link together Nodes from the allocated chunk
    {
      embedded_node *i_node = first_node_of_allocated_chunk;
      block_type i_block = first_block_of_allocated_chunk;

      while (i_node != end_node_of_allocated_chunk) {
        i_node->next_block = ++i_block;
        i_node = get_node_from_block(i_node->next_block);
      }
    }
    // Now put the allocated chunk at the front of the embedded list
    end_node_of_allocated_chunk->next_block = current_freed_block;

    block_type new_first_block_of_chunk
        = handle_defragment(first_block_of_allocated_chunk, end_node_of_allocated_chunk);

    current_freed_block = new_first_block_of_chunk;
  }

  constexpr auto operator==(const bucket<Size, BucketSize> &other) -> bool {
    return this->buffer == other.buffer;
  }

  constexpr auto operator!=(const bucket<Size, BucketSize> &other) -> bool {
    return !(*this == other);
  }

private:
  inline auto handle_defragment(block_type begin_block, embedded_node *end_node,
                                std::uint8_t attempts = EMG_BUCKET_DEFRAC_ATTEMPT) -> block_type {
    auto t_begin_block = begin_block;
    auto t_end_node = end_node;

    for (auto _ : std::views::iota(0u, attempts)) {
      block_type end_block_of_current_chunk = get_block_from_node(t_end_node);
      block_type next_block_after_current_chunk = t_end_node->next_block;
      embedded_node *next_node_after_current_chunk
          = get_node_from_block(next_block_after_current_chunk);

      if (next_block_after_current_chunk > end_block_of_current_chunk
          && next_block_after_current_chunk - end_block_of_current_chunk
                 == 1) { // if the next block is adjacement after the
                         // end of chunk
        embedded_node *begin_node_of_chunk = get_node_from_block(t_begin_block);
        embedded_node *end_node_of_chunk = get_node_from_block(
            next_block_after_current_chunk
            + static_cast<block_type>(next_node_after_current_chunk->flag - 1));

        begin_node_of_chunk->flag = begin_node_of_chunk->flag + next_node_after_current_chunk->flag;
        end_node_of_chunk->flag = -begin_node_of_chunk->flag;

        t_end_node = end_node_of_chunk;
      } else if (next_block_after_current_chunk < t_begin_block
                 && next_block_after_current_chunk
                            + get_node_from_block(next_block_after_current_chunk)->flag
                        == t_begin_block) { // if the next block is adjacement after the end of
                                            // chunk
        embedded_node *begin_node_of_chunk = next_node_after_current_chunk;
        embedded_node *end_node_of_chunk = t_end_node;

        begin_node_of_chunk->flag
            = begin_node_of_chunk->flag + get_node_from_block(t_begin_block)->flag;
        end_node_of_chunk->flag = -begin_node_of_chunk->flag;

        t_begin_block = get_block_from_node(begin_node_of_chunk);
      } else {
        break;
      }
    }

    return t_begin_block;
  }

  // NOTE: These 3 function is the main reason why alignment of
  // embedded_node have to = 2 (for better performance)
  [[nodiscard]] inline auto get_node_from_block(block_type block) const -> embedded_node * {
    auto *block_address = buffer + block * BlockSize;

    if constexpr (BlockSize % 2 == 0) {
      return reinterpret_cast<embedded_node *>(block_address);
    }

    if (block % 2 == 0) {
      return reinterpret_cast<embedded_node *>(block_address);
    } else {
      return reinterpret_cast<embedded_node *>(block_address + 1);
    }
  }

  [[nodiscard]] inline auto get_block_address_from_node(embedded_node *node) const -> std::byte * {
    if constexpr (BlockSize % 2 == 0) {
      return reinterpret_cast<std::byte *>(node);
    }

    auto intptr = reinterpret_cast<std::uintptr_t>(node);

    if (intptr % BlockSize == 0) {
      return reinterpret_cast<std::byte *>(node);
    } else {
      return reinterpret_cast<std::byte *>(node - 1);
    }
  }

  [[nodiscard]] inline auto get_block_from_node(embedded_node *node) const -> block_type {
    const auto *block_address = get_block_address_from_node(node);
    return static_cast<block_type>(block_address - buffer) / BlockSize;
  }

private:
  // The actually buffer of memory the Bucket allocated
  std::byte *buffer;

  std::uint16_t num_used_blocks;

  // This keep track of the freed Block that is the first Node of the embedded list, so that when we
  // try to allocate, we will start from here
  block_type current_freed_block;
};

} // namespace emg::memory

#endif
