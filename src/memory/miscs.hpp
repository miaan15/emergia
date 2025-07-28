#include <cassert>
#include <cstddef>
#include <cstdint>

namespace emg::memory {

// Similar to the implementation of std::align but remove the need of lvalue
// reference
inline auto align(std::size_t align, // NOLINT(bugprone-easily-swappable-parameters)
                  std::size_t size,  // NOLINT(bugprone-easily-swappable-parameters)
                  const void *const ptr, std::size_t space) noexcept -> void * {
  assert((align & (align - 1)) == 0);

  if (space < size)
    return nullptr;
  const auto intptr = reinterpret_cast<std::uintptr_t>(ptr);
  const auto aligned = (intptr - 1u + align) & -align;
  const auto diff = aligned - intptr;
  if (diff > (space - size))
    return nullptr;
  else {
    return reinterpret_cast<void *>(aligned); // NOLINT(performance-no-int-to-ptr)
  }
}

inline auto is_aligned(std::size_t align, void *ptr) noexcept -> bool {
  assert((align & (align - 1)) == 0);

  return (reinterpret_cast<std::uintptr_t>(ptr) & (align - 1)) == 0;
}

} // namespace emg::memory
