#pragma once

#include "./config.hpp"

namespace accat::auxilia::ranges::views::detail {
struct _swap_endian_fn {
  template <std::ranges::viewable_range R>
  [[nodiscard]] AC_STATIC_CALL_OPERATOR_ constexpr auto operator()(R &&r)
      AC_CONST_CALL_OPERATOR_->decltype(auto) {
    return std::forward<R>(r) | std::views::reverse | std::views::common;
  }

  // Allow piping, temporary workaround
  template <std::ranges::viewable_range R>
  [[nodiscard]] friend constexpr auto operator|(R &&r, const _swap_endian_fn &e)
      -> decltype(auto) {
    return e.operator()(std::forward<R>(r));
  }
};
} // namespace accat::auxilia::ranges::views::detail
namespace accat::auxilia::ranges::views {
/// @brief inverts the endianness of the given range(char-like elements)
inline constexpr detail::_swap_endian_fn swap_endian;
/// @brief trims the leading and trailing whitespace-like characters
/// from given range(char-like elements)
inline constexpr auto trim(const string_view str) -> string_view {
  auto beg = std::ranges::find_if_not(str, isspacelike);
  auto end = std::ranges::find_last_if_not(str, isspacelike).end();
  return {std::move(beg), std::move(end)};
}
} // namespace accat::auxilia::ranges::views

namespace accat::auxilia {
namespace views = ranges::views; // NOLINT(misc-unused-alias-decls)
} // namespace accat::auxilia
