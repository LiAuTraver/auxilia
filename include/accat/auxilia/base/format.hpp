#pragma once

#include <cstdint>
#include <cstdio>
#include <ostream>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

#include "macros.hpp"
#include "config.hpp"
#if !AC_USE_STD_FMT
#  include <fmt/color.h>
#  include <fmt/xchar.h>
namespace accat::auxilia {
template <typename... T>
inline auto
Format(fmt::text_style ts, fmt::format_string<T...> fmt, T &&...args) {
  return ::fmt::format(ts, fmt, std::forward<T>(args)...);
}
template <typename... T>
inline auto
Format(fmt::text_style ts, fmt::wformat_string<T...> fmt, T &&...args) {
  return ::fmt::format(ts, fmt, std::forward<T>(args)...);
}
template <typename... T>
inline auto
Print(fmt::text_style ts, fmt::format_string<T...> fmt, T &&...args) {
  return ::fmt::print(
      ::std::forward<fmt::text_style>(ts), (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Print(::std::FILE *f,
                  fmt::text_style ts,
                  fmt::format_string<T...> fmt,
                  T &&...args) {
  return ::fmt::print(f,
                      ::std::forward<fmt::text_style>(ts),
                      (fmt),
                      ::std::forward<T>(args)...);
}
template <typename... T>
inline auto
Println(fmt::text_style ts, fmt::format_string<T...> fmt, T &&...args) {
  ::fmt::print(
      ::std::forward<fmt::text_style>(ts), (fmt), ::std::forward<T>(args)...);
  ::putchar('\n');
}
template <typename... T>
inline auto Println(::std::FILE *f,
                    fmt::text_style ts,
                    fmt::format_string<T...> fmt,
                    T &&...args) {
  ::fmt::print(f,
               ::std::forward<fmt::text_style>(ts),
               (fmt),
               ::std::forward<T>(args)...);
  ::puts("\n");
}
} // namespace accat::auxilia
#endif
namespace accat::auxilia {
template <typename T, typename Char = char>
concept Formattable = AC_STD_OR_FMT formattable<T, Char>;
template <typename... T> using FormatString = AC_STD_OR_FMT format_string<T...>;

// omit the locale overload
template <typename... T>
AC_NODISCARD inline auto Format(AC_STD_OR_FMT format_string<T...> fmt,
                                T &&...args) {
  return AC_STD_OR_FMT format((fmt), ::std::forward<T>(args)...);
}
template <typename... T>
  requires(!std::is_same_v<AC_STD_OR_FMT format_string<T...>, T...>)
AC_NODISCARD inline auto Format(T &&...args) {
  return AC_STD_OR_FMT format(("{}"), ::std::forward<T>(args)...);
}

template <typename... T>
inline auto
Print(::std::FILE *f, AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print(f, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Print(AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print((fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto
Print(::std::FILE *f, AC_STD_OR_FMT wformat_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print(f, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Print(AC_STD_OR_FMT wformat_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print((fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto
Print(::std::ostream &os, AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print(os, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto
Print(::std::wostream &os, AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT print(os, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
  requires(!std::is_same_v<AC_STD_OR_FMT format_string<T...>, T...>)
inline auto Print(T &&...args) {
  return AC_STD_OR_FMT format(("{}"), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Println(::std::ostream &os,
                    AC_STD_OR_FMT format_string<T...> fmt,
                    T &&...args) {
  return AC_STD_OR_FMT println(os, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Println(::std::wostream &os,
                    AC_STD_OR_FMT format_string<T...> fmt,
                    T &&...args) {
  return AC_STD_OR_FMT println(os, (fmt), ::std::forward<T>(args)...);
}

template <typename... T>
inline auto
Println(::std::FILE *f, AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT println(f, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Println(AC_STD_OR_FMT format_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT println((fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto
Println(::std::FILE *f, AC_STD_OR_FMT wformat_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT println(f, (fmt), ::std::forward<T>(args)...);
}
template <typename... T>
inline auto Println(AC_STD_OR_FMT wformat_string<T...> fmt, T &&...args) {
  return AC_STD_OR_FMT println((fmt), ::std::forward<T>(args)...);
}
inline auto Println(void) { return AC_STD_OR_FMT println(""); }

template <typename... T>
  requires(!std::is_same_v<AC_STD_OR_FMT format_string<T...>, T...>)
inline auto Println(T &&...args) {
  return AC_STD_OR_FMT print(("{}\n"), ::std::forward<T>(args)...);
}
} // namespace accat::auxilia

namespace accat::auxilia::details {
class _out_fn {
public:
  AC_FLATTEN
  AC_FORCEINLINE AC_STATIC_CALL_OPERATOR inline AC_CONSTEXPR20 decltype(auto)
  operator()(auto &&...args) AC_CONST_CALL_OPERATOR {
    return Println(std::forward<decltype(args)>(args)...);
  }
};
AC_FLATTEN AC_FORCEINLINE static inline AC_CONSTEXPR20 decltype(auto)
operator<<(const _out_fn &out, auto &&arg) {
  Println(std::forward<decltype(arg)>(arg));
  return out;
}
class _err_fn {
public:
  AC_FLATTEN
  AC_FORCEINLINE AC_STATIC_CALL_OPERATOR inline AC_CONSTEXPR20 decltype(auto)
  operator()(auto &&...args) AC_CONST_CALL_OPERATOR {
    return Println(stderr, std::forward<decltype(args)>(args)...);
  }
};
AC_FLATTEN AC_FORCEINLINE static inline AC_CONSTEXPR20 decltype(auto)
operator<<(const _err_fn &err, auto &&arg) {
  Println(stderr, std::forward<decltype(arg)>(arg));
  return err;
}
} // namespace accat::auxilia::details

namespace accat::auxilia {
inline constexpr details::_out_fn Out;
inline constexpr details::_err_fn Err;

template <typename... Ts> class Variant;
enum class FormatPolicy : uint8_t;

struct Printable;
struct Viewable;
template <typename Ty>
concept is_printable = requires { std::is_base_of_v<Printable, Ty>; };
template <typename Ty>
concept is_viewable = requires { std::is_base_of_v<Viewable, Ty>; };

template <typename Ty> inline bool is_integer(Ty &&value) noexcept {
  static_assert(std::is_arithmetic_v<std::remove_cvref_t<Ty>>);
  return std::trunc(::std::forward<Ty>(value)) == value;
}
template <typename... Ts> struct match : Ts... {
  using Ts::operator()...;
};
/// @brief deduction guide for `match`
template <typename... Ts> match(Ts...) -> match<Ts...>;
enum class FormatPolicy : unsigned char {
  kDefault = 0,
  kDetailed = 1,
  kBrief = 2,
};
/// @interface Printable
/// @brief A class that represents a printable object; can be directly
/// printed via `std::cout` or `fmt::print`.
/// @note use public inheritance to make fmt::print work.
struct AC_NOVTABLE AC_EMPTY_BASES Printable {

private:
  template <typename Ty> struct please_define_to_string_method_for;

protected:
  using string_type = std::string;
  template <typename T>
    requires std::is_base_of_v<Printable, T>
  friend auto operator<<(std::ostream &os, const T &p) -> std::ostream & {
    if constexpr (requires { p.to_string(FormatPolicy::kDefault); }) {
      return os << p.to_string(FormatPolicy::kDefault);
    } else if constexpr (requires { p.to_string(); }) {
      return os << p.to_string();
    } else {
      please_define_to_string_method_for<T>{};
    }
  }
  template <typename T>
    requires std::is_base_of_v<Printable, T>
  AC_NODISCARD friend auto
  format_as(const T &p,
            const FormatPolicy format_policy = FormatPolicy::kDefault)
      -> Printable::string_type {
    if constexpr (requires { p.to_string(format_policy); }) {
      return p.to_string(format_policy);
    } else if constexpr (requires { p.to_string(); }) {
      return p.to_string();
    } else {
      please_define_to_string_method_for<T>{};
    }
  }

public:
  static constexpr auto Brief = [](is_printable auto &&p) {
    return p.to_string(FormatPolicy::kBrief);
  };
  static constexpr auto Detailed = [](is_printable auto &&p) {
    return p.to_string(FormatPolicy::kDetailed);
  };
  static constexpr auto Default = [](is_printable auto &&p) {
    return p.to_string(FormatPolicy::kDefault);
  };
};

/// @interface Viewable
struct AC_NOVTABLE AC_EMPTY_BASES Viewable {
private:
  template <typename Ty> struct please_define_to_string_view_method_for;

protected:
  using string_view_type = std::string_view;
  template <typename T>
    requires std::is_base_of_v<Viewable, T>
  friend auto operator<<(std::ostream &os, const T &v) -> std::ostream & {
    if constexpr (requires { v.to_string_view(FormatPolicy::kDefault); }) {
      return os << v.to_string_view(FormatPolicy::kDefault);
    } else if constexpr (requires { v.to_string_view(); }) {
      return os << v.to_string_view();
    } else {
      please_define_to_string_view_method_for<T>{};
    }
  }
  template <typename T>
    requires std::is_base_of_v<Viewable, T>
  AC_NODISCARD friend auto
  format_as(const T &v,
            const FormatPolicy format_policy = FormatPolicy::kDefault)
      -> Viewable::string_view_type {
    if constexpr (requires { v.to_string_view(format_policy); }) {
      return v.to_string_view(format_policy);
    } else if constexpr (requires { v.to_string_view(); }) {
      return v.to_string_view();
    } else {
      please_define_to_string_view_method_for<T>{};
    }
  }

public:
  static constexpr auto Brief = [](is_viewable auto &&v) {
    return v.to_string_view(FormatPolicy::kBrief);
  };
  static constexpr auto Detailed = [](is_viewable auto &&v) {
    return v.to_string_view(FormatPolicy::kDetailed);
  };
  static constexpr auto Default = [](is_viewable auto &&v) {
    return v.to_string_view(FormatPolicy::kDefault);
  };
};
} // namespace accat::auxilia

namespace std {
template <typename Derived>
  requires std::is_base_of_v<::accat::auxilia::Printable, Derived>
struct formatter<Derived> {
  inline constexpr auto parse(format_parse_context &ctx) const noexcept {
    return ctx.begin();
  }
  template <typename FormatContext>
  inline constexpr auto format(const Derived &p, FormatContext &ctx) const {
    return format_to(
        ctx.out(), "{}", p.to_string(::accat::auxilia::FormatPolicy::kDefault));
  }
};
} // namespace std
