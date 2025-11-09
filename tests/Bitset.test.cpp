#include <gtest/gtest.h>
#include <bitset>
#include <iostream>

#include <accat/auxilia/details/bitset.hpp>
#if defined(_MSC_VER) && !defined(__clang__)
// skip the tests on MSVC for now
inline static const auto msg = []() {
  std::cerr << "Note: Bitset tests are skipped on MSVC due to compiler "
               "internal error.\n";
  return nullptr;
}();
#else
using namespace accat::auxilia;

TEST(Bitset, BasicOperations) {
  constexpr auto lhs = 0b11001100ull;
  constexpr auto rhs = 0b10101010ull;

  constexpr bitset<8> b1{lhs};
  constexpr bitset<8> b2{rhs};

  constexpr auto and_result = b1 & b2;
  constexpr auto or_result = b1 | b2;
  constexpr auto xor_result = b1 ^ b2;

  EXPECT_EQ(and_result.to_ullong(), lhs & rhs);
  EXPECT_EQ(or_result.to_ullong(), lhs | rhs);
  EXPECT_EQ(xor_result.to_ullong(), lhs ^ rhs);

  using namespace accat::auxilia::literals;
  constexpr auto lhs_bs = "11001100"_bs;
  constexpr auto rhs_bs = "10101010"_bs;

  static_assert(lhs_bs.size() == b1.size(), "Size mismatch");
  static_assert(rhs_bs.size() == b2.size(), "Size mismatch");

  constexpr auto std_bs1 = std::bitset<8>("11001100");
  constexpr auto std_bs2 = std::bitset<8>("10101010");

  EXPECT_EQ(lhs_bs.to_ullong(), std_bs1.to_ullong());
  EXPECT_EQ(rhs_bs.to_ullong(), std_bs2.to_ullong());

  EXPECT_EQ(lhs_bs.to_ullong(), lhs);
  EXPECT_EQ(rhs_bs.to_ullong(), rhs);

  EXPECT_EQ(b1.to_string(), "11001100");
  EXPECT_EQ(b2.to_string(), "10101010");
}

TEST(Bitset, ConstructionAndSize) {
  constexpr bitset<16> b{};
  EXPECT_EQ(b.size(), 16u);
  EXPECT_TRUE(b.none());
  EXPECT_EQ(b.count(), 0u);
}

TEST(Bitset, BitAccessAndModify) {
  bitset<8> b;
  b.set(); // set all bits
  EXPECT_EQ(b.count(), 8u);
  b.reset(); // reset all bits
  EXPECT_EQ(b.count(), 0u);

  b.set(3); // set specific bit
  EXPECT_TRUE(b.test(3));
  b.flip(3); // flip that bit
  EXPECT_FALSE(b.test(3));

  // operator[] modify and read
  b[2] = true;
  EXPECT_TRUE(b[2]);
  b[2] = false;
  EXPECT_FALSE(b[2]);

  // set with value
  b.set(7, true);
  EXPECT_TRUE(b.test(7));
  b.set(7, false);
  EXPECT_FALSE(b.test(7));
}

TEST(Bitset, CountAnyNone) {
  constexpr bitset<8> b{0b10110000u};
  EXPECT_EQ(b.count(), 3u);
  EXPECT_TRUE(b.any());
  EXPECT_FALSE(b.none());

  constexpr bitset<8> empty{0u};
  EXPECT_EQ(empty.count(), 0u);
  EXPECT_FALSE(empty.any());
  EXPECT_TRUE(empty.none());
}

TEST(Bitset, Conversion) {
  constexpr bitset<8> b{0b00001111u};
  EXPECT_EQ(b.to_string(), std::string("00001111"));
  EXPECT_EQ(b.to_ullong(), 0x0Fu);

  bitset<8> b2{0b10000000u};
  EXPECT_EQ(b2.to_string(), std::string("10000000"));
  EXPECT_EQ(b2.to_ullong(), 0x80u);
}

TEST(Bitset, ShiftOperators) {
  constexpr bitset<8> b{0b00001111u};
  auto left = b << 2;
  auto right = b >> 2;

  EXPECT_EQ(left.to_ullong(), 0b00111100u);
  EXPECT_EQ(right.to_ullong(), 0b00000011u);

  // shifting assignment
  bitset<8> s = b;
  s <<= 4;
  EXPECT_EQ(s.to_ullong(), 0b11110000u);
  s >>= 2;
  EXPECT_EQ(s.to_ullong(), 0b00111100u);
}

TEST(Bitset, CompareOperators) {
  constexpr bitset<8> a{0b01010101u};
  constexpr bitset<8> b{0b01010101u};
  constexpr bitset<8> c{0b10101010u};

  EXPECT_TRUE(a == b);
  EXPECT_FALSE(a != b);
  EXPECT_TRUE(a != c);
  EXPECT_FALSE(a == c);
}
#endif
