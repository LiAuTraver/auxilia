#include <gtest/gtest.h>
#include <accat/auxilia/details/chars.hpp>

using namespace accat::auxilia;

TEST(BasicChars, DefaultCtor) {
  constexpr chars<5> c;
  EXPECT_EQ(c.size(), 4);
  EXPECT_EQ(c.length(), 4);
}

TEST(BasicChars, CArrayCtor) {
  constexpr chars<6> c("hello");
  EXPECT_EQ(c.size(), 5);
  EXPECT_EQ(c[0], 'h');
  EXPECT_EQ(c[4], 'o');
}

TEST(BasicChars, CopyCtor) {
  constexpr chars<6> c1("hello");
  constexpr chars<6> c2(c1);
  EXPECT_EQ(c1, c2);
}

TEST(BasicChars, InitializerListCtor) {
  constexpr chars<5> c{'t', 'e', 's', 't'};
  EXPECT_EQ(c[0], 't');
  EXPECT_EQ(c[3], 't');
}

TEST(BasicChars, CopyAssign) {
  constexpr chars<6> c1("hello");
  chars<6> c2 = c1;
  EXPECT_EQ(c1, c2);
}

TEST(BasicChars, CArrayAssign) {
  chars<6> c = "world";
  EXPECT_EQ(c, "world");
}

TEST(BasicChars, EqualityOperator) {
  constexpr chars<6> c1("hello");
  constexpr chars<6> c2("hello");
  constexpr chars<6> c3("world");

  EXPECT_TRUE(c1 == c2);
  EXPECT_FALSE(c1 == c3);
}

TEST(BasicChars, EqualityWithCArray) {
  constexpr chars<6> c("hello");
  EXPECT_TRUE(c == "hello");
  EXPECT_FALSE(c == "world");
}

TEST(BasicChars, SubscriptOperator) {
  chars<6> c("hello");
  EXPECT_EQ(c[0], 'h');
  c[0] = 'H';
  EXPECT_EQ(c[0], 'H');
}

TEST(BasicChars, AtMethod) {
  chars<6> c("hello");
  EXPECT_EQ(c.at(1), 'e');
  c.at(1) = 'E';
  EXPECT_EQ(c.at(1), 'E');
}

TEST(BasicChars, FrontBack) {
  chars<6> c("hello");
  EXPECT_EQ(c.front(), 'h');
  EXPECT_EQ(c.back(), 'o');

  c.front() = 'H';
  c.back() = 'O';
  EXPECT_EQ(c.front(), 'H');
  EXPECT_EQ(c.back(), 'O');
}

TEST(BasicChars, Data) {
  constexpr chars<6> c("hello");
  const auto *ptr = c.data();
  EXPECT_EQ(ptr[0], 'h');
  EXPECT_EQ(ptr[4], 'o');
}

TEST(BasicChars, ForwardIter) {
  chars<6> c("hello");
  std::string result;
  for (auto ch : c) {
    result += ch;
  }
  EXPECT_EQ(result, "hello");
}

TEST(BasicChars, ReverseIter) {
  chars<6> c("hello");
  std::string result;
  for (auto it = c.rbegin(); it != c.rend(); ++it) {
    result += *it;
  }
  EXPECT_EQ(result, "olleh");
}

TEST(BasicChars, ConstIterators) {
  constexpr chars<6> c("hello");
  std::string result;
  for (auto it = c.cbegin(); it != c.cend(); ++it) {
    result += *it;
  }
  EXPECT_EQ(result, "hello");
}

TEST(BasicChars, IteratorArithmetic) {
  chars<6> c("hello");
  auto it = c.begin();
  EXPECT_EQ(*it, 'h');
  EXPECT_EQ(*(it + 2), 'l');
  EXPECT_EQ(*(it + 4), 'o');

  auto it2 = c.end();
  EXPECT_EQ(it2 - it, 5);
}

TEST(BasicChars, Count) {
  constexpr chars<6> c("hello");
  EXPECT_EQ(c.count('l'), 2);
  EXPECT_EQ(c.count('h'), 1);
  EXPECT_EQ(c.count('x'), 0);
}

TEST(BasicChars, Contains) {
  constexpr chars<6> c("hello");
  EXPECT_TRUE(c.contains('h'));
  EXPECT_TRUE(c.contains('l'));
  EXPECT_FALSE(c.contains('x'));
}

TEST(BasicChars, Clear) {
  chars<6> c("hello");
  c.clear();
  EXPECT_EQ(c.count('\0'), 5);
}

TEST(BasicChars, Fill) {
  chars<5> c;
  c.fill<'x'>();
  EXPECT_EQ(c[0], 'x');
  EXPECT_EQ(c[3], 'x');
}

TEST(BasicChars, Replace) {
  chars<6> c("hello");
  c.replace<0, 'H'>();
  EXPECT_EQ(c[0], 'H');
  EXPECT_EQ(c, "Hello");
}

TEST(BasicChars, Reverse) {
  chars<6> c("hello");
  c.reverse();
  EXPECT_EQ(c, "olleh");
}

TEST(BasicChars, StartsWithChar) {
  constexpr chars<6> c("hello");
  EXPECT_TRUE(c.starts_with('h'));
  EXPECT_FALSE(c.starts_with('e'));
}

TEST(BasicChars, StartsWithCArray) {
  constexpr chars<6> c("hello");
  EXPECT_TRUE(c.starts_with("hel"));
  EXPECT_FALSE(c.starts_with("ell"));
}

TEST(BasicChars, StartsWithBasicChars) {
  constexpr chars<6> c1("hello");
  constexpr chars<4> c2("hel");
  constexpr chars<4> c3("wor");
  EXPECT_TRUE(c1.starts_with(c2));
  EXPECT_FALSE(c1.starts_with(c3));
}

TEST(BasicChars, ToStringView) {
  constexpr chars<6> c("hello");
  std::string_view sv = c;
  EXPECT_EQ(sv, "hello");
  EXPECT_EQ(sv.size(), 5);
}

TEST(BasicChars, ToString) {
  constexpr chars<6> c("hello");
  std::string s = c;
  EXPECT_EQ(s, "hello");
  EXPECT_EQ(s.size(), 5);
}

TEST(BasicChars, FromLiteral) {
  using namespace accat::auxilia::literals;
  constexpr auto s = "test\n"_c;
  EXPECT_EQ(s.size(), 5);
  EXPECT_EQ(s, "test\n");
}

TEST(BasicChars, AsChars) {
  constexpr auto c = as_chars("test");
  EXPECT_EQ(c.size(), 4);
  EXPECT_EQ(c, "test");
}

TEST(BasicChars, FromCArray) {
  constexpr basic_chars c("hello");
  static_assert(std::is_same_v<decltype(c), const chars<6>>);
  EXPECT_EQ(c, "hello");
}

TEST(BasicChars, FromInitList) {
  constexpr basic_chars c{'a', 'b', 'c'};
  EXPECT_EQ(c.size(), 3);
  EXPECT_EQ(c[0], 'a');
  EXPECT_EQ(c[2], 'c');
}

TEST(BasicChars, WChars) {
  constexpr wchars<6> wc(L"hello");
  EXPECT_EQ(wc.size(), 5);
  EXPECT_EQ(wc[0], L'h');
}

#ifdef __cpp_char8_t
TEST(BasicChars, Char8) {
  constexpr u8chars<6> u8c(u8"hello");
  EXPECT_EQ(u8c.size(), 5);
  EXPECT_EQ(u8c[0], u8'h');
}
#endif

TEST(BasicChars, Char16) {
  constexpr u16chars<6> u16c(u"hello");
  EXPECT_EQ(u16c.size(), 5);
  EXPECT_EQ(u16c[0], u'h');
}

TEST(BasicChars, Char32) {
  constexpr u32chars<6> u32c(U"hello");
  EXPECT_EQ(u32c.size(), 5);
  EXPECT_EQ(u32c[0], U'h');
}
