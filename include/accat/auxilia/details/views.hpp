#pragma once

#include <ranges>
#include <concepts>
#include <type_traits>
#include <algorithm>
#include <string>
#include <string_view>
#include <utility>

#include "./config.hpp"

namespace accat::auxilia::ranges::views::details {
struct _swap_endian_fn {
  template <std::ranges::viewable_range R>
  [[nodiscard]] AC_STATIC_CALL_OPERATOR constexpr auto operator()(R &&r)
      AC_CONST_CALL_OPERATOR->decltype(auto) {
    return std::forward<R>(r) | std::views::reverse | std::views::common;
  }

  // Allow piping, temporary workaround
  template <std::ranges::viewable_range R>
  [[nodiscard]] friend constexpr auto operator|(R &&r, const _swap_endian_fn &e)
      -> decltype(auto) {
    return e.operator()(std::forward<R>(r));
  }
};

template <std::ranges::input_range R>
  requires std::ranges::view<R>
class surround_view : public std::ranges::view_interface<surround_view<R>> {
  R base_;
  using T = std::ranges::range_value_t<R>;
  T front_, back_;

public:
  // aliased required to make pipelining possible
  using value_type = T;
  using reference = T;
  using difference_type = std::ranges::range_difference_t<R>;

  surround_view() = default;

  surround_view(R r, T front, T back)
      : base_(std::move(r)), front_(front), back_(back) {}

  // simple state machine: {first, middle, last}
  struct iterator {
    enum class state { front, inner, back, done };

    // ditto
    using value_type = T;
    using reference = T;
    using difference_type = std::ranges::range_difference_t<R>;
    using iterator_category = std::input_iterator_tag;
    using iterator_concept = std::input_iterator_tag;

    surround_view *parent = nullptr;
    std::ranges::iterator_t<R> it;
    state st = state::front;

    T operator*() const {
      switch (st) {
      case state::inner:
        return *it;
      case state::front:
        return parent->front_;
      case state::back:
        return parent->back_;
      default:
        AC_THROW_OR_DIE_("invalid state");
      }
    }

    iterator &operator++() {
      switch (st) {
      case state::inner:
        ++it;
        if (it == std::ranges::end(parent->base_))
          st = state::back;
        break;
      case state::front:
        it = std::ranges::begin(parent->base_);
        if (it == std::ranges::end(parent->base_))
          st = state::back;
        else
          st = state::inner;
        break;
      case state::back:
        st = state::done;
        break;
      case state::done:
        break;
      }
      return *this;
    }

    void operator++(int) { ++*this; }

    bool operator==(std::default_sentinel_t) const { return st == state::done; }
  };

  iterator begin() { return iterator{this}; }
  std::default_sentinel_t end() const noexcept { return {}; }

  // Add base() accessor (commonly expected for views)
  R base() const &
    requires std::copy_constructible<R>
  {
    return base_;
  }
  R base() && { return std::move(base_); }
};

template <class R>
surround_view(R &&,
              std::ranges::range_value_t<R>,
              std::ranges::range_value_t<R>)
    -> surround_view<std::views::all_t<R>>;

// Range adaptor closure
template <typename T> struct surround_adaptor_closure {
  T front_;
  T back_;

  surround_adaptor_closure(T front, std::type_identity_t<T> back)
      : front_(front), back_(back) {}

  template <std::ranges::input_range R> auto operator()(R &&r) const {
    return surround_view(std::forward<R>(r), front_, back_);
  }
};

// Range adaptor object
struct _surround_fn {
  template <typename T>
  AC_STATIC_CALL_OPERATOR auto operator()(T front, std::type_identity_t<T> back)
      AC_CONST_CALL_OPERATOR {
    return surround_adaptor_closure<T>{front, back};
  }
  template <typename T>
  AC_STATIC_CALL_OPERATOR auto operator()(T surround) AC_CONST_CALL_OPERATOR {
    return surround_adaptor_closure<T>{surround, surround};
  }

  template <std::ranges::input_range R, typename T>
  AC_STATIC_CALL_OPERATOR auto
  operator()(R &&r, T front, std::type_identity_t<T> back)
      AC_CONST_CALL_OPERATOR {
    return surround_view(std::forward<R>(r), front, back);
  }
};

template <std::ranges::input_range R, typename T>
auto operator|(R &&r, const surround_adaptor_closure<T> &adaptor) {
  return adaptor(std::forward<R>(r));
}

} // namespace accat::auxilia::ranges::views::details
EXPORT_AUXILIA
namespace accat::auxilia::ranges::views {
/// @brief inverts the endianness of the given range(char-like elements)
inline constexpr details::_swap_endian_fn swap_endian;
inline constexpr details::_surround_fn surround;
/// @brief trims the leading and trailing whitespace-like characters
/// from given range(char-like elements)
inline constexpr auto trim(std::string_view sv) -> std::string_view {
  std::size_t i = 0;
  while (i < sv.size() && isspacelike(sv[i]))
    ++i;
  std::size_t j = sv.size();
  while (j > i && isspacelike(sv[j - 1]))
    --j;
  return sv.substr(i, j - i);
}
} // namespace accat::auxilia::ranges::views

namespace accat::auxilia {
using ranges::views::details::surround_view;
namespace views = ranges::views; // NOLINT(misc-unused-alias-decls)
} // namespace accat::auxilia
