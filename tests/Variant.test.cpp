#include <gtest/gtest.h>

#include <accat/auxilia/details/Variant.hpp>
#include <accat/auxilia/details/format.hpp>

using namespace accat::auxilia;

TEST(Variant, DefaultConstruction) {
  Variant<Monostate, int, double> v;
  EXPECT_TRUE(v.is_type<Monostate>());
  auto str =
      v.visit(match([](const int &i) { return std::to_string(i); },
                    [](const double &d) { return std::to_string(d); },
                    [](const auto &) -> std::string { return "Don't care"; }));
  EXPECT_EQ(str, "Don't care");
  EXPECT_TRUE(v.empty());
  EXPECT_EQ(v.index(), 0);
}

TEST(Variant, CopyConstruction) {
  Variant<Monostate, int, double> v1 = 42;
  auto v2 = v1;
  EXPECT_TRUE(v2.is_type<int>());
  EXPECT_EQ(v2.get<int>(), 42);
  EXPECT_EQ(v2.index(), 1);
  EXPECT_EQ(v1, v2);
}

TEST(Variant, MoveConstruction) {
  Variant<Monostate, int, double> v1 = 42;
  auto v2 = std::move(v1);
  EXPECT_TRUE(v2.is_type<int>());
  EXPECT_EQ(v2.get<int>(), 42);
  EXPECT_EQ(v2.index(), 1);
  EXPECT_TRUE(v1.empty());

  EXPECT_EQ(v1.index(), 0);
  EXPECT_TRUE(v1.is_type<Monostate>());
  EXPECT_EQ(v1.get_if<int>(), nullptr);
}
#include <accat/auxilia/details/Status.hpp>
using namespace std::literals;
TEST(Variant, PatternMatching) {
  Variant<Monostate, Status, std::string> v1 =
      InvalidArgumentError("Invalid argument");
  auto str =
      v1.visit(match([](const Status &s) { return s.message(); },
                     [](const std::string &s) -> std::string_view { return s; },
                     [](const auto &) { return "Don't care"sv; }));
  EXPECT_EQ(str, "Invalid argument");
}

TEST(Variant, VisitMultiple) {
  Variant<Monostate, int, double> v1 = 42;
  Variant<Monostate, int, double> v2 = 3.14;
  auto pattern =
      match([](const int &i,
               const int j) { return "integer: "s + std::to_string(i + j); },
            [](const double &d1, const double d2) {
              return "double: "s + std::to_string(d1 + d2);
            },
            [](const int &i, const double &d) {
              return "mixed: "s + std::to_string(i + d);
            },

            [](const auto &, const auto &) { return "Don't care"s; });
  auto result = visit(pattern, v1, v2);
  EXPECT_EQ(result, "mixed: 45.140000"s);
}

TEST(Variant, ToString) {
  Variant<Monostate, int, double> v1 = 42;
  EXPECT_EQ(v1.to_string(), "42");
  v1.emplace(3.14);
  EXPECT_EQ(v1.to_string(), "3.14");
  v1.clear();
  auto str = format("v1: {}", v1);
  EXPECT_EQ(str, "v1: Monostate");
  v1.emplace_and_then(100).clear();
}
TEST(Variant, MonostateLikeType) {
  struct MyMonostate : Monostate {
    auto to_string(const FormatPolicy) const { return "MyMonostate"s; }
    auto to_string_view(const FormatPolicy) const { return "MyMonostate"s; }
  };
  Variant<MyMonostate, int, double> v1 = 42;
  EXPECT_EQ(v1.to_string(), "42");
  v1.reset();
  auto str = format("v1: {}", v1);
  EXPECT_EQ(str, "v1: MyMonostate");
}
