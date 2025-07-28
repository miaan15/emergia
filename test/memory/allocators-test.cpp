#include <gtest/gtest.h>
#include <memory>
#include <string>
#include <vector>

#include "memory/bucket.hpp"
#include "memory/pool.hpp"

TEST(bucket_test, basic_test) {
  emg::memory::bucket<sizeof(int)> bucket;
  emg::memory::pool<double> pool;

  auto *b_p = bucket.allocate(3);
  bucket.deallocate(b_p, 3);
  [[maybe_unused]] auto b_empty = bucket.empty();
  [[maybe_unused]] auto b_used = bucket.used_size();
  [[maybe_unused]] auto b_max = bucket.max_size();

  auto *p_p = pool.allocate(3);
  pool.deallocate(p_p, 3);

  std::vector<std::string, emg::memory::pool<std::string>> p_v;
  p_v.emplace_back("dsa");

  std::allocator_traits<emg::memory::pool<double>> p_at;
  auto _ = p_at.allocate(pool, 10);
}
