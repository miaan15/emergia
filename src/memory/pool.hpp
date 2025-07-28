#ifndef _EMG_MEMORY_POOL_HPP
#define _EMG_MEMORY_POOL_HPP

#include "memory/bucket.hpp"

#ifndef EMG_DEFAULT_POOL_SIZE
#define EMG_DEFAULT_POOL_SIZE EMG_DEFAULT_BUCKET_SIZE
#endif

namespace emg::memory {

template <typename T, std::size_t PoolSize = EMG_DEFAULT_POOL_SIZE>
  requires(PoolSize <= sizeof(T) * INT16_MAX)
class pool {
public:
  using value_type = T;
  using pointer = T *;
  using const_pointer = const T *;
  using void_pointer = void *;
  using const_void_pointer = const void *;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;

  template <typename U>
  struct rebind {
    using other = pool<U, PoolSize>;
  };

public:
  pool() = default;
  ~pool() = default;

  pool(const pool &) = delete;
  auto operator=(const pool &) -> pool & = delete;

  pool(pool &&other) noexcept : _bucket(other._bucket) {}
  auto operator=(pool &&other) noexcept -> pool & {
    if (this != &other)
      this->_bucket = other._bucket;
  }

  [[nodiscard]] auto allocate(size_type n) -> T * {
    return static_cast<T *>(_bucket.allocate(n));
  }
  auto deallocate(void *p, size_type n) -> void {
    _bucket.deallocate(p, n);
  }

private:
  bucket<sizeof(T), EMG_DEFAULT_POOL_SIZE> _bucket;
};

template <typename T, std::size_t PoolSize>
constexpr auto operator==(const pool<T, PoolSize> &lhs, const pool<T, PoolSize> &rhs) {
  return lhs.buffer == rhs.buffer;
}

template <typename T, std::size_t PoolSize>
constexpr auto operator!=(const pool<T, PoolSize> &lhs, const pool<T, PoolSize> &rhs) {
  return !(lhs == rhs);
}

} // namespace emg::memory

#endif
