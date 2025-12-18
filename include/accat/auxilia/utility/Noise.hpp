#pragma once

#include "accat/auxilia/base/config.hpp"
#include "accat/auxilia/base/format.hpp"
#include "accat/auxilia/meta/type_traits.hpp"

namespace accat::auxilia {
/// @brief A utility class for debugging object lifecycle events.
template <
    const basic_chars_storage Constructor = "Noise instance created  ",
    const basic_chars_storage Destructor = "Noise instance destroyed",
    const basic_chars_storage CopyConstructor = "Noise copy constructed  ",
    const basic_chars_storage CopyAssignment = "Noise copy assigned     ",
    const basic_chars_storage MoveConstructor = "Noise move constructed  ",
    const basic_chars_storage MoveAssignment = "Noise move assigned     ">
struct AC_EMPTY_BASES AC_NOVTABLE Noise : Printable {
  inline Noise() noexcept { _do_log<Constructor>(); }
  inline Noise(const Noise &other) noexcept { _do_log<CopyConstructor>(); }
  inline Noise(Noise &&other) noexcept { _do_log<MoveConstructor>(); }
  inline ~Noise() noexcept { _do_log<Destructor>(); }
  inline Noise &operator=(const Noise &other) noexcept {
    _do_log<CopyAssignment>();
    return *this;
  }
  inline Noise &operator=(Noise &&other) noexcept {
    _do_log<MoveAssignment>();
    return *this;
  }
  constexpr auto to_string(auto) const noexcept { return typeid(*this).name(); }

private:
  template <const basic_chars_storage Message>
  constexpr void _do_log() noexcept {
    Println("{} at {}", Message.arr, voidify_unfancy(this));
  }
};
Noise() -> Noise<>;
} // namespace accat::auxilia
