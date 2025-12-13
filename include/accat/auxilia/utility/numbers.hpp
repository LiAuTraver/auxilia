#pragma once

#include <concepts>
#include <string_view>
#include <utility>

#include "accat/auxilia/base/format.hpp"
#include "accat/auxilia/meta/type_traits.hpp"

namespace accat::auxilia {
namespace details {
inline constexpr auto _is_valid_base(const char c) -> bool {
  return c == 'x' || c == 'X' || c == 'o' || c == 'O' || c == 'b' || c == 'B';
}
inline constexpr auto _get_base(const char c) -> int {
  return c == 'x' || c == 'X' ? 16 : c == 'o' || c == 'O' ? 8 : 2;
}
} // namespace details

template <typename Integer>
inline constexpr auto to_integer(std::string_view value)
    -> std::optional<Integer> {
  static_assert(std::integral<Integer>);
  int Base = 10;

  if (value.size() >= 2 && value[0] == '0' &&
      details::_is_valid_base(value[1])) {
    Base = details::_get_base(value[1]);
    value = value.substr(2);
  }

  Integer number;
  const auto res =
      std::from_chars(value.data(), value.data() + value.size(), number, Base);
  if (res.ec == std::errc())
    return {number};
  Println("Unable to convert string '{0}' to integer: at {1}, "
          "error: {2}",
          value,
          res.ptr,
          std::make_error_code(res.ec).message());
  return {};
}
template <typename Floating>
inline constexpr auto to_floating(std::string_view value)
    -> std::optional<Floating> {
  static_assert(std::floating_point<Floating>);
  Floating number;
  const auto res =
      std::from_chars(value.data(), value.data() + value.size(), number);
  if (res.ec == std::errc())
    return {number};
  Println("Unable to convert string '{0}' to floating-point number: at {1}, "
          "error: {2}",
          value,
          res.ptr,
          std::make_error_code(res.ec).message());
  return {};
}
template <typename Num>
inline constexpr auto to_number(std::string_view sv) noexcept {
  if constexpr (std::integral<Num>) {
    return to_integer<Num>(std::forward<std::string_view>(sv));
  } else if constexpr (std::floating_point<Num>) {
    return to_floating<Num>(std::forward<std::string_view>(sv));
  } else {
    always_false<Num>("not a number type");
  }
}
} // namespace accat::auxilia
