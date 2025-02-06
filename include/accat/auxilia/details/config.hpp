#pragma once
#include "./macros.hpp"
EXPORT_AUXILIA
namespace accat::auxilia {
class Monostate;
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
// FIXME: not working
template <typename Ty>
concept Storable = std::conjunction_v<std::is_default_constructible<Ty>,
                                      std::is_nothrow_destructible<Ty>,
                                      std::is_nothrow_constructible<Ty>>;

template <typename...> class Variant;
class Status;
template <typename> class StatusOr;
using string = ::std::string;
using string_view = ::std::string_view;
using path = ::std::filesystem::path;
using ifstream = ::std::ifstream;
using ostringstream = ::std::ostringstream;
using namespace ::std::string_view_literals;
using namespace ::std::string_literals;
inline constexpr auto isspacelike = [](const char &c) constexpr noexcept -> bool {
  return c == ' ' || c == '\t' || c == '\n' || c == '\r';
};
inline constexpr auto isnotspacelike = [](const char &c) constexpr noexcept -> bool {
  return not isspacelike(c);
};
inline consteval const char *raw(const char *str) {
  while (str && *str && (*str == '\n')) {
    ++str;
  }
  auto ptr = const_cast<char *>(str);
  while (ptr && *ptr)
    ptr++;

  // remove trailing newline and space
  while (ptr && ptr != str && isspacelike(*ptr)) {
    *ptr = '\0';
    ptr--;
  }
  return str;
}
inline consteval auto operator""_raw(const char *str, const size_t) noexcept
    -> const char * {
  return raw(str);
}
template <typename T = void> inline T *alloc(const size_t size) {
  if (auto ptr = malloc(size))
    return static_cast<T *>(ptr);

  fprintf(stderr, "Failed to allocate memory. The program will now exit.");
  contract_assert(0, "Failed to allocate memory")
  std::abort();
}

template <const auto &Str> struct array_size_t {
  inline static constexpr auto size =
      std::extent_v<std::remove_reference_t<decltype(Str)>>;
};
template <const auto &Str>
using array_size_v = typename array_size_t<Str>::size;
} // namespace accat::auxilia
