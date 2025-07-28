#include <gtest/gtest.h>

#include "memory/bucket.hpp"

TEST(bucket_test, basic_test) {
  emg::memory::bucket<sizeof(int)> bucket;

  auto *p = bucket.allocate(3);
  bucket.deallocate(p, 3);
  [[maybe_unused]] auto empty = bucket.empty();
  [[maybe_unused]] auto used = bucket.used_size();
  [[maybe_unused]] auto max = bucket.max_size();
}
