#pragma once

#include "./config.hpp"

namespace accat::auxilia::ranges::views::detail {
#if defined(__cpp_lib_ranges_chunk_by) &&                                      \
    __cpp_lib_ranges_chunk_by >= 202202L &&                                    \
    defined(_MSC_VER) // workaround for gnu/llvm
struct _invert_endianness_fn {
  template <std::ranges::viewable_range R>
  [[nodiscard]] AC_STATIC_CALL_OPERATOR constexpr auto
  operator()(R &&r) AC_CONST_CALL_OPERATOR->decltype(auto) {
    // clang-format off
    return std::forward<R>(r) 
              | std::views::chunk(2) 
              | std::views::transform([](auto chunk) { 
                  return chunk | std::views::reverse; 
                }) 
              | std::views::reverse
              | std::views::join
              | std::views::common
          ;
    // clang-format on
  }

  // Allow piping, temporary workaround
  template <std::ranges::viewable_range R>
  [[nodiscard]] friend constexpr auto operator|(R &&r,
                                                const _invert_endianness_fn &e)
      -> decltype(auto) {
    return e.operator()(std::forward<R>(r));
  }
};
#endif

struct _trim_fn {
  template <std::ranges::viewable_range R>
  [[nodiscard]] AC_STATIC_CALL_OPERATOR constexpr auto
  operator()(R &&r) AC_CONST_CALL_OPERATOR->decltype(auto) {
    // clang-format off
    return std::forward<R>(r) 
              | std::views::drop_while(isspacelike) 
              | std::views::reverse 
              | std::views::drop_while(isspacelike) 
              | std::views::reverse
              | std::views::common
          ;
    // clang-format on
  }

  // ditto
  /// @copydoc _invert_endianness_fn::operator()
  template <std::ranges::viewable_range R>
  [[nodiscard]] friend constexpr auto operator|(R &&r, const _trim_fn &t)
      -> decltype(auto) {
    return t.operator()(std::forward<R>(r));
  }
};
} // namespace accat::auxilia::ranges::views::detail
namespace accat::auxilia::ranges::views {
#if defined(__cpp_lib_ranges_chunk_by) &&                                      \
    __cpp_lib_ranges_chunk_by >= 202202L &&                                    \
    defined(_MSC_VER) // workaround for gnu/llvm
/// @brief inverts the endianness of the given range(char-like elements)
inline constexpr detail::_invert_endianness_fn invert_endianness;
#endif
/// @brief trims the leading and trailing whitespace-like characters
/// from given range(char-like elements)
inline constexpr detail::_trim_fn trim;
} // namespace accat::auxilia::ranges::views

namespace accat::auxilia {
namespace views = ranges::views; // NOLINT(misc-unused-alias-decls)
} // namespace accat::auxilia
