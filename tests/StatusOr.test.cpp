#include "accat/auxilia/status/StatusOr.hpp"

#include <gtest/gtest.h>

#include "accat/auxilia/utility/Noise.hpp"

using namespace accat::auxilia;

template <typename T>
concept CanStore = requires { typename StatusOr<T>::value_type; };

TEST(StatusOr, Constraints) {
  static_assert(CanStore<int>);
  // static_assert(!CanStore<int&>);
  static_assert(CanStore<std::string>);
  // static_assert(!CanStore<StatusOr<int>>);
  // static_assert(!CanStore<Status>);
  static_assert(CanStore<std::vector<int>>);
}

TEST(StatusOr, ValueConstruction) {
  StatusOr<int> so(5);
  EXPECT_TRUE(so.ok());
  EXPECT_EQ(so.value(), 5);
}

TEST(StatusOr, StatusConstruction) {
  StatusOr<int> so(InvalidArgumentError("Invalid argument"));
  EXPECT_FALSE(so.ok());
  EXPECT_EQ(so.code(), Status::kInvalidArgument);
  EXPECT_EQ(so.message(), "Invalid argument");
}

StatusOr<int> getValue(bool shouldSucceed) {
  if (shouldSucceed)
    return {42};
  else
    return {InvalidArgumentError("Failed to get value")};
}

TEST(StatusOr, ValueRetrieval) {
  EXPECT_EQ(getValue(true).to_string(), "42");
  EXPECT_TRUE(getValue(true).ok());

  EXPECT_EQ(getValue(false).code(), Status::kInvalidArgument);
  EXPECT_EQ(getValue(false).message(), "Failed to get value");
}

TEST(StatusOr, ExplicitBool) {
  if (auto maybe_int = getValue(true)) {
    EXPECT_TRUE(maybe_int.ok());
    EXPECT_EQ(maybe_int.value(), 42);
  } else {
    EXPECT_FALSE("Should not reach here");
  }

  if (auto maybe_int = getValue(false)) {
    EXPECT_FALSE(maybe_int.ok());
    EXPECT_EQ(maybe_int.code(), Status::kInvalidArgument);
    EXPECT_EQ(maybe_int.message(), "Failed to get value");
  } else {
    EXPECT_TRUE("Should reach here");
  }
}

TEST(StatusOr, Monadic) {
  auto an_int = getValue(true)
                    .and_then([](int value) {
                      EXPECT_EQ(value, 42);
                      return UnknownError("An error occurred");
                    })
                    .or_else([](auto &&error) /* -> StatusOr<int> */ {
                      EXPECT_EQ(error.code(), Status::kUnknown);
                      EXPECT_EQ(error.message(), "An error occurred");
                      return 43;
                    });
  EXPECT_TRUE(an_int.ok());
  EXPECT_EQ(an_int.value(), 43);
  auto a_str = an_int.and_then(
      [](int value) -> StatusOr<std::string> { return std::to_string(value); });
  EXPECT_EQ(a_str.value(), "43");
}

TEST(StatusOr, MonadicTransform) {
  auto an_int = getValue(true).transform([](int value) {
    EXPECT_EQ(value, 42);
    return 43;
  });
  EXPECT_TRUE(an_int.ok());
  EXPECT_EQ(*an_int, 43);

  auto another_int = getValue(false)
                         .transform([](int value) {
                           EXPECT_FALSE("Should not reach here");
                           return 43;
                         })
                         .transform_error([](auto &&error) {
                           EXPECT_EQ(error.code(), Status::kInvalidArgument);
                           EXPECT_EQ(error.message(), "Failed to get value");
                           return 44;
                         });
  EXPECT_EQ(another_int.value(), 44);
}

TEST(StatusOr, ToOptional) {
  auto s1 = getValue(true);
  EXPECT_TRUE(s1.ok());
  auto opt1 = s1.rvalue().to_optional();
  EXPECT_EQ(*opt1, 42);
  // s1 stores an int which is trivially copyable; hence we need a different way
  // to test

  auto s2 = StatusOr(std::string("hello"));
  EXPECT_TRUE(s2.ok());
  auto opt_str = s2.rvalue().to_optional();
  EXPECT_EQ(*opt_str, "hello");

  EXPECT_TRUE(s2.code() == Status::Code::kMovedFrom); // moved from

  auto opt2 = getValue(false).to_optional();
  EXPECT_FALSE(opt2.has_value());
}
