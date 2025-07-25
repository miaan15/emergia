// ==================================================================================================
// The Bucket allocator manages a fixed-size chunk of memory, divided into an integral number of
// equal-sized blocks called Blocks, all Block have the same size.
//
//   Memory allocations are performed in units of blocks: allocating n blocks reserves n * Size
//   bytes of contiguous memory.
//
//   Tracks freed (not allocated or deallocated) blocks internally to reduce fragmentation.
//
//   There is no memory overhead (except when Size < embeded-list's Node size).
//
// "How this work" (preliminary)
//
//   Freed Blocks are used to store a Node structure for forming an embedded singly linked list of
//   available (freed) Blocks.
//
//   On allocation, we traverses the embedded list to find the Blocks chunk large enough, then
//   return that and remove that chunk from the list.
//
//   On deallocation, the deallocated block chunk is wrapped as a seies of Nodes and inserted at
//   thefront of the free list.
//
//   The embedded list is updated accordingly.
//
// Detail
// TODO:
//
// Definition
//
//   Block and Node are actually pretty much the same, they are represent in a same block of memory
//     Block is that memory as a whole.
//     Node is existed only if the Block is freed, Node is just uses a part of that memory to store
//     some data which is useful for the implementation
//
//   A Block chunk is a group of continuous freed Blocks
// ==================================================================================================
#ifndef _EMG_MEMORY_BUCKET_HPP
#define _EMG_MEMORY_BUCKET_HPP

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <memory>
#include <new>
#include <ranges>

#if defined(__linux__) || defined(__unix__)
#include <sys/mman.h>
#elif defined(_WIN32)
#include <Memoryapi.h>
#include <Windows.h>
#endif

#ifndef EMG_MAX_BUCKET_SIZE
#define EMG_MAX_BUCKET_SIZE INT16_MAX * 4
#endif

// TODO: benchmark this to find the best number
#ifndef EMG_BUCKET_DEFRAC_ATTEMPT
#define EMG_BUCKET_DEFRAC_ATTEMPT 6
#endif

namespace emg::memory {

// Size: size of Block (in bytes)
// BucketSize: size of the chunk of memory Bucket hold (in bytes)
// BucketSize must be < INT16_MAX * 4 since we use int to mark the list
// index (yes, negative is nessesary)
template <std::size_t Size, std::size_t BucketSize = EMG_MAX_BUCKET_SIZE>
  requires(BucketSize <= EMG_MAX_BUCKET_SIZE)
class bucket {
public:
  using size_type = std::size_t;
  using block_type = std::uint16_t;

private:
  // The embedded list is like a Single linked list
  // next_block is the index of the next freed Block that a Node links to
  // flag is a  TODO:
  struct embedded_node {
    block_type next_block;
    std::int16_t flag;
  };

public:
  // in order to embedded a Node in a  Block, the BlockSize must > Node size
  static constexpr size_type BlockSize = std::max(Size, sizeof(embedded_node));
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

    // At first, the whole is a big Block chunk
    for (size_type i : std::views::iota(0u, TotalNumBlocks)) {
      get_node_from_block(i)->next_block = i + 1;
    }
    get_node_from_block(0)->flags = TotalNumBlocks;
    for (size_type i : std::views::iota(1u, TotalNumBlocks)) {
      get_node_from_block(i)->flags = -i;
    }
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
    // The strategy here is pretty much the same as a single-linked list (expect the flag), we took
    // the suited Blocks chunk and then return it, then we link the previous Node to the next Node
    // of that Blocks chunk, then we update the Nodes' flag: when allocate, these allocated blocks
    // are always at the beginning of a Block chunk, so to keep the embedded list valid, we just
    // need to update the flags of the Node on the next Block after these allocated blocks and on
    // the Block at the end of the chunk

    embedded_node *current_suited_node = get_node_from_block(current_freed_block);
    block_type current_suited_block = current_freed_block;

    // Search through the embedded list until found the Blocks
    embedded_node *previous_node = nullptr;
    while (current_suited_node->space < n) {
      previous_node = current_suited_node;
      current_suited_node = get_node_from_block(current_suited_node->next_block);
    }

    block_type next_unused_block = // this Block that will soon become the
                                   // next first Node in the embedded list
        n > 1 ? get_node_from_block(current_suited_block + (n - 1))->next_block
              : current_suited_node->next_block;

    // Update next node and the end of chunk node
    embedded_node next_node = get_node_from_block(next_unused_block);
    next_node->flag = current_suited_node->flag - n;
    embedded_node end_of_chunk_node
        = get_node_from_block(current_suited_block + current_suited_node->flag);
    end_of_chunk_node->flag = -(next_node->flag);

    assert(next_unused_block <= TotalNumBlocks);

    block_type target_block = current_freed_block; // the result allocated Blocks
    current_freed_block = next_unused_block;       // attach the new first Node

    if (previous_node != nullptr) {
      previous_node->next_block = next_unused_block; // link the preNode to the nextNode
                                                     // (just like Single linked list)
    }

    num_used_blocks += n;

    return static_cast<void *>(buffer + target_block * BlockSize); // return the actaully memory
                                                                   // pointer of target_block
  }

  auto deallocate(void *p, size_type n) -> void {
    auto byte_p = static_cast<std::byte *>(p);

    assert((byte_p - buffer) % BlockSize == 0);
    block_type begin_of_allocated_block = (byte_p - buffer) / BlockSize;
    embedded_node *begin_of_allocated_node = get_node_from_block(begin_of_allocated_block);
    embedded_node *end_of_allocated_node = get_node_from_block(begin_of_allocated_block + n - 1);

    begin_of_allocated_node->flag = n;
    for (embedded_node *i_node = begin_of_allocated_node, i_block = begin_of_allocated_block;
         i_node != end_of_allocated_node; ++i_block) {
      i_node->next_block = i_block + 1;
      i_node = get_node_from_block(i_node->next_block);
    }
    end_of_allocated_node->flag = -n;
    end_of_allocated_node->next_block = current_freed_block;

    handle_defragment(begin_of_allocated_block, end_of_allocated_node);

    current_freed_block = begin_of_allocated_block;
  }

  [[nodiscard]] auto empty() noexcept -> bool {
    return num_used_blocks == 0;
  }

  [[nodiscard]] auto used_size() noexcept -> size_type {
    return num_used_blocks;
  }
  [[nodiscard]] auto max_size() noexcept -> size_type {
    return BucketSize;
  }

private:
  inline auto handle_defragment(block_type begin_block, embedded_node *end_node,
                                std::uint8_t attempts = EMG_BUCKET_DEFRAC_ATTEMPT) {
    block_type end_block, next_block;

    for (auto _ : std::views::iota(0u, attempts)) {
      end_block = get_block_from_node(end_node);
      next_block = end_node->next_block;
      embedded_node *next_node = get_node_from_block(next_block);

      if (next_block > end_block && next_block - end_block == 1) {
        embedded_node *begin_of_chunk_node = get_node_from_block(begin_block);
        embedded_node *end_of_chunk_node = get_node_from_block(next_node + next_node->flag - 1);

        begin_of_chunk_node = begin_of_chunk_node->flag + next_node->flag;
        end_of_chunk_node = -begin_of_chunk_node->flag;
      } else if (next_block < begin_block
                 && next_block + get_node_from_block(next_block)->flag == begin_block) {
        embedded_node *begin_of_chunk_node = next_node;
        embedded_node *end_of_chunk_node = end_node;

        begin_of_chunk_node = begin_of_chunk_node->flag + get_node_from_block(begin_block)->flag;
        end_of_chunk_node = -begin_of_chunk_node->flag;
      } else {
        break;
      }
    }
  }

  // NOTE: These 3 function is the main reason why alignment of
  // embedded_node have to = 2 (for better performance)
  [[nodiscard]] inline auto get_node_from_block(block_type block) const -> embedded_node * {
    const auto *block_address = buffer + block * BlockSize;

    if constexpr (BlockSize % 2 == 0) {
      return static_cast<embedded_node *>(block_address);
    }

    if (block % 2 == 0) {
      return static_cast<embedded_node *>(block_address);
    } else {
      return static_cast<embedded_node *>(block_address + 1);
    }
  }

  [[nodiscard]] inline auto get_block_address_from_node(embedded_node *node) const -> std::byte * {
    if constexpr (BlockSize % 2 == 0) {
      return static_cast<std::byte *>(node);
    }

    const auto intptr = reinterpret_cast<std::uintptr_t>(node);

    if (intptr % BlockSize == 0) {
      return static_cast<std::byte *>(intptr);
    } else {
      return static_cast<std::byte *>(intptr - 1);
    }
  }

  [[nodiscard]] inline auto get_block_from_node(embedded_node *node) const -> block_type {
    const auto *block_address = get_block_address_from_node(node);
    return static_cast<block_type>((block_address - buffer) / BlockSize);
  }

private:
  // The actually buffer of memory the Bucket allocated
  std::byte *buffer;

  std::uint16_t num_used_blocks;

  // This keep track of the freed Block that is the first Node of the
  // embedded list, so that when we try to allocate, we will start from
  // here
  block_type current_freed_block;
};

} // namespace emg::memory

#endif
