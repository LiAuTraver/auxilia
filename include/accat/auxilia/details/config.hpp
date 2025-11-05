#pragma once
#include "./macros.hpp"

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
using string = ::std::string;
using string_view = ::std::string_view;
using path = ::std::filesystem::path;
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
/// @brief shorthand of static_cast. sugar is all you need :)
template <typename To, typename From>
inline constexpr To as(From &&from) noexcept {
  return static_cast<To>(from);
}
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
} // namespace accat::auxilia
