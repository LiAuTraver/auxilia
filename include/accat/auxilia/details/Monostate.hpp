#pragma once

#include <compare>

#include "./config.hpp"
#include "./format.hpp"

namespace accat::auxilia {
/// @brief Represents a stateless type that can be used as a
/// placeholder
/// @implements Printable, Viewable
/// @remark Different from `std::monostate`, this class implements two
/// traits which can be extremely important in
/// my @link Variant @endlink class;
/// dont't use consteval because the class is not final
EXPORT_AUXILIA
struct AC_EMPTY_BASES AC_NOVTABLE Monostate : Printable, Viewable {
public:
  inline constexpr Monostate() = default;
  /* not virtual */ inline AC_CONSTEXPR20 ~Monostate() noexcept = default;
  inline constexpr Monostate(const Monostate &) {}
  inline constexpr Monostate(Monostate &&) noexcept {}
  inline /* not consteval */ constexpr auto operator=(const Monostate &)
      -> Monostate & {
    return *this;
  }
  inline /* not consteval */ constexpr auto operator=(Monostate &&) noexcept
      -> Monostate & {
    return *this;
  }

public:
  inline /* not consteval */ constexpr auto to_string(const FormatPolicy) const
      -> string_type {
    return "Monostate";
  }
  inline /* not consteval */ constexpr auto
  to_string_view(const FormatPolicy) const -> string_view_type {
    return "Monostate";
  }
  inline consteval auto data(const FormatPolicy) { return "Monostate"; }

  friend inline /* not consteval */ constexpr auto
  operator==(const Monostate &, const Monostate &) noexcept -> bool {
    return true;
  }
  friend inline /* not consteval */ constexpr auto
  operator<=>(const Monostate &, const Monostate &) noexcept
      -> std::strong_ordering {
    return std::strong_ordering::equal;
  }
};
} // namespace accat::auxilia
