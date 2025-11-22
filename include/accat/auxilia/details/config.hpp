#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <future>
#include <tuple>
#include <type_traits>
#include <concepts>

#include "variadic-inl.h" // IWYU pragma: export
#include "macros.hpp"     // IWYU pragma: export

EXPORT_AUXILIA
namespace accat::auxilia {
struct Monostate;
struct Printable;
struct Viewable;
/// @brief a concept that checks if the types are variantable for my custom
/// @link Variant @endlink class,
/// where the first type must be @link Monostate @endlink or its derived class
/// @tparam Types the types to check
template <typename... Types>
concept Variantable = requires {
  (std::is_same_v<std::tuple_element_t<0, std::tuple<Types...>>, Monostate> ||
   std::is_base_of_v<Monostate,
                     std::tuple_element_t<0, std::tuple<Types...>>>) &&
      (std::is_default_constructible_v<Types> && ...);
};
/// @brief represents a value that can be stored in a
/// @link StatusOr @endlink object
/// @tparam Ty the type of the value
/// @remarks similiar to Microsoft's @link std::_SMF_control @endlink class,
/// which was used in @link std::optional @endlink
template <typename Ty>
concept Storable =
#if _WIN32
    std::conjunction_v<std::is_default_constructible<Ty>,
                       std::is_nothrow_destructible<Ty>,
                       std::is_nothrow_constructible<Ty>,
                       std::is_same<std::remove_reference_t<Ty>, Ty>>
#else // workaround for linux
    true
#endif
    ;

template <typename...> class Variant;
class Status;
template <typename> class StatusOr;

inline constexpr auto isspacelike =
    [](const char c) constexpr noexcept -> bool {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
};
inline constexpr auto isnotspacelike = std::not_fn(isspacelike);

/// @brief remove leading and trailing newlines and spaces from a raw string
inline consteval const char *raw(const char *str) noexcept {

  while (str && *str && (*str == '\n'))
    ++str;

  auto ptr = const_cast<char *>(str);
  while (ptr && *ptr)
    ptr++;

  while (ptr && ptr != str && isspacelike(*ptr))
    *ptr-- = '\0';

  return str;
}
namespace literals {
inline consteval auto operator""_raw(const char *str, const size_t) noexcept
    -> const char * {
  return raw(str);
}
} // namespace literals
/// @brief Asynchronously execute a function with the given arguments
/// @note just a wrapper, but it's REAL async
auto async(auto &&func, auto... args) -> decltype(auto)
  requires std::invocable<decltype(func), decltype(args)...>
{
  return std::async(std::launch::async,
                    std::forward<decltype(func)>(func),
                    std::forward<decltype(args)>(args)...);
}

// idk how people came up with this magic number, here's the reference:
// https://softwareengineering.stackexchange.com/a/402543
inline constexpr auto hash_magic_number_32bit = 0x9e3779b9u;
inline constexpr auto hash_magic_number_64bit = 0x9e3779b97f4a7c15ull;
inline constexpr auto epsilon = "ε";
inline constexpr auto npos = static_cast<size_t>(-1);
inline constexpr auto npos32 = static_cast<uint32_t>(-1);

inline constexpr auto is_epsilon = [](const char *ptr) constexpr noexcept {
  return static_cast<unsigned char>(*(ptr + 0)) == 0xCE && //
         static_cast<unsigned char>(*(ptr + 1)) == 0xB5;
};

AC_FLATTEN inline auto set_console_output_cp_utf8() noexcept {
  return details::_set_console_output_cp_utf8();
}
AC_FLATTEN AC_NODISCARD inline bool is_debugger_present() noexcept {
  return details::_is_debugger_present();
}
AC_FLATTEN AC_FORCEINLINE inline void breakpoint() noexcept {
  details::_debugbreak();
}
AC_FLATTEN AC_FORCEINLINE inline void break_if_debugging() noexcept {
  if (details::_is_debugger_present())
    details::_debugbreak();
}
template <typename To, typename From>
AC_FLATTEN AC_FORCEINLINE inline To as(From &&f) noexcept {
  return static_cast<To>(f);
}

// from MSVC STL:
// converts from a fancy pointer to a plain pointer
template <class FancyPtrTy>
AC_NODISCARD constexpr auto unfancy(FancyPtrTy ptr) noexcept {
  return std::addressof(*ptr);
}
// do nothing for plain pointers
template <class PtrTy>
AC_NODISCARD constexpr PtrTy *unfancy(PtrTy *ptr) noexcept {
  return ptr;
}

// converts from a (potentially null) fancy pointer to a plain pointer
template <class FancyPtrTy>
AC_NODISCARD constexpr auto unfancy_maybe_null(FancyPtrTy ptr) noexcept {
  return ptr ? std::addressof(*ptr) : nullptr;
}
// do nothing for plain pointers
template <class PtrTy>
AC_NODISCARD constexpr PtrTy *unfancy_maybe_null(PtrTy *ptr) noexcept {
  return ptr;
}
template <class PtrTy> AC_NODISCARD void *voidify_unfancy(PtrTy Ptr) noexcept {
  if constexpr (std::is_pointer_v<PtrTy>) {
    return Ptr;
  } else {
    return std::addressof(*Ptr);
  }
}
} // namespace accat::auxilia
