#include <array>
#include <gtest/gtest.h>
#include <set>

#include "memory/bucket.hpp"

TEST(bucket_test, constructor_make_the_right_data) {
  emg::memory::bucket<4, 400> bucket_4;
  EXPECT_EQ(bucket_4.BlockSize, 4);
  EXPECT_EQ(bucket_4.TotalNumBlocks, 100);

  emg::memory::bucket<1, 10> bucket_1;
  EXPECT_EQ(bucket_1.BlockSize, 4);
  EXPECT_EQ(bucket_1.TotalNumBlocks, 2);

  emg::memory::bucket<1024, 2048> bucket_10;
  EXPECT_EQ(bucket_10.BlockSize, 1024);
  EXPECT_EQ(bucket_10.TotalNumBlocks, 2);

  emg::memory::bucket<17, 100> bucket_128;
  EXPECT_EQ(bucket_128.BlockSize, 17);
  EXPECT_EQ(bucket_128.TotalNumBlocks, 5);

  emg::memory::bucket<INT16_MAX> bucket_INT16_MAX;
  EXPECT_EQ(bucket_INT16_MAX.BlockSize, INT16_MAX);
  EXPECT_EQ(bucket_INT16_MAX.TotalNumBlocks, 3);
}

TEST(bucket_test, allocated_memory_not_colliding) {
  emg::memory::bucket<6> bucket;
  std::set<void *> set;
  for (auto i = 0u; i < 10u; ++i) {
    auto *p = bucket.allocate(1);
    EXPECT_FALSE(set.contains(p));
    set.insert(p);
  }
  for (auto &p : set) {
    for (auto i = 1u; i < 6u; ++i) {
      EXPECT_FALSE(set.contains(reinterpret_cast<void *>(static_cast<std::byte *>(p) + i)));
    }
  }
}

TEST(bucket_test, allocated_memory_is_continuous) {
}

