#pragma once
#include "./variadic-inl.h"

#if __cplusplus < 202002L && not defined(AC_SILENCE_CPP_STANDARD_CHECK)
#  if defined(_MSC_VER) && !defined(__clang__)
#    error "This library requires at least C++20."
#  else
#    error "This library requires at least C++20. " \
           "If you are certain C++20 has turned on, " \
           "maybe you forgot to add /Zc:__cplusplus."
#  endif
#endif

#if defined(__GNUC__) && defined(_WIN32)
#  error This library does not support GCC on Windows.
#endif

#if defined(AC_CPP_DEBUG)
/// @def AC_DEBUG_ENABLED
/// @note only defined in debug mode; never define it when submitting
/// the code or it'll mess up the test. #AC_CPP_DEBUG was defined in
/// the CMakeLists.txt, which can be turned on by set the environment
/// variable `AC_CPP_DEBUG` to `ON`.
/// @attention debug mode needs external libraries to work properly,
/// namely, `fmt`, `spdlog`, and potentially `gtest` and `Google
/// Benchmark`. release mode carries no dependencies and only requires
/// C++23.
#  define AC_DEBUG_ENABLED 1
/// @def AC_USE_FMT_FORMAT
/// @note use fmt::print, fmt::println when compiling with
/// clang-cl.exe will cause some wired error: Critical error detected
/// c0000374 A breakpoint instruction (__debugbreak() statement or a
/// similar call) was executed, which related to heap corruption. The
/// program will terminate.
/// @internal now it's ok again, dunno why.
#endif
#if !defined(AC_USE_STD_FMT)
#  if __has_include(<fmt/format.h>)
#    define AC_USE_STD_FMT 0
#  else
#    define AC_USE_STD_FMT 1
#  endif
#endif

/// @note I use Source Code Annotation sometimes; but libstdc++
/// doesn't support; so we manually define it here.
#if __has_include(<sal.h>)
#  include <sal.h> // IWYU pragma: export
#else
#  define _In_
#  define _Inout_
#  define _In_opt_
#  define _Out_
#  define _Outptr_
#  define _Outptr_result_maybenull_
#  define _Outptr_opt_
#endif

#include <source_location>

#if !AC_USE_STD_FMT
#  include <fmt/format.h>
#  include <fmt/ostream.h>
#  include <fmt/color.h>
#  include <fmt/std.h>
#  include <fmt/ranges.h>
#  include <fmt/xchar.h>
#  define AC_STD_OR_FMT ::fmt::
#else
#  define AC_USE_STD_FMT 1
#  include <format>
#  include <iostream>
#  if __has_include(<print>)
#    include <print>
#  endif
#  define AC_STD_OR_FMT ::std::
#endif

// although C++ standard may be updated, the constexpr feature may not be
// supported by all compilers.
#if __cpp_constexpr >= 202000L
/// @def AC_CONSTEXPR20
/// @brief c++20-style constexpr support
#  define AC_CONSTEXPR20 constexpr
#else
#  define AC_CONSTEXPR20
#endif

#if __cpp_constexpr >= 202211L

/// @def AC_CONSTEXPR23
/// @brief c++23-style constexpr support
#  define AC_CONSTEXPR23 constexpr
#else
#  define AC_CONSTEXPR23
#endif

#if __cpp_consteval >= 202211L
/// @def AC_CONSTEVAL
/// @brief propagate consteval support
#  define AC_CONSTEVAL consteval
#else
#  define AC_CONSTEVAL AC_CONSTEXPR20
#endif

#if __cpp_static_call_operator >= 202207L
#  define AC_STATIC_CALL_OPERATOR static
#  define AC_CONST_CALL_OPERATOR
#else
#  define AC_STATIC_CALL_OPERATOR
#  define AC_CONST_CALL_OPERATOR const
#endif

#if __has_cpp_attribute(nodiscard) >= 201907L
#  define AC_NODISCARD_REASON(...) [[nodiscard(__VA_ARGS__)]]
/// @def AC_NODISCARD
/// @brief nodiscard attribute
/// @note why this macro? mostly because attribute with `[[]]` costs more
/// typing,
///       especially if clangd is used. ;)
#  define AC_NODISCARD [[nodiscard]]
#elif __has_cpp_attribute(nodiscard) >= 201603L
#  define AC_NODISCARD_REASON(...) [[nodiscard]]
#  define AC_NODISCARD [[nodiscard]]
#else
#  define AC_NODISCARD_REASON(...)
#  define AC_NODISCARD
#endif

#ifdef __clang__
/// @def AC_FLATTEN
/// @brief function flatten attribute
/// @note non-standard but well supported.
#  define AC_FLATTEN [[gnu::flatten]]
/// @def AC_NO_SANITIZE_ADDRESS
/// @brief function no address sanitizer attribute
/// @note non-standard but well supported.
#  define AC_NO_SANITIZE_ADDRESS [[clang::no_sanitize("address")]]
/// @def AC_FORCEINLINE
/// @brief function force inline attribute
/// @note the attribute is non-standard
///       but supported by all three major compilers.
#  define AC_FORCEINLINE [[clang::always_inline]]
#  define AC_DEBUG_FUNCTION_NAME __PRETTY_FUNCTION__
#elif defined(_MSC_VER)
#  define AC_FLATTEN [[msvc::flatten]]
#  define AC_NO_SANITIZE_ADDRESS __declspec(no_sanitize_address)
#  define AC_FORCEINLINE [[msvc::forceinline]]
#  define AC_DEBUG_FUNCTION_NAME __FUNCSIG__
#elif defined(__GNUC__)
#  define AC_FLATTEN [[gnu::flatten]]
#  define AC_NO_SANITIZE_ADDRESS __attribute__((no_sanitize("address")))
#  define AC_FORCEINLINE [[gnu::always_inline]]
#  define AC_DEBUG_FUNCTION_NAME __PRETTY_FUNCTION__
#else
#  define AC_FLATTEN
#  define AC_NO_SANITIZE_ADDRESS
#  define AC_FORCEINLINE inline
#  define AC_DEBUG_FUNCTION_NAME __func__
#endif

#ifdef _WIN32
/// @def AC_NOVTABLE
/// @note msvc-specific keyword to prevent vtable generation.
///     Used in(and should only in) interface classes(i.e., pure interface and
///     never instantiated directly).
/// @remark no gcc/clang equivalent.
#  define AC_NOVTABLE __declspec(novtable)
/// @def AC_EMPTY_BASES
/// @note msvc-specific keyword to enforce empty base optimization.
/// @remark no gcc/clang equivalent.
#  define AC_EMPTY_BASES __declspec(empty_bases)
#else
#  define AC_NOVTABLE
#  define AC_EMPTY_BASES
#endif

#ifdef AUXILIA_BUILD_MODULE
#  define EXPORT_AUXILIA export
#  define AUXILIA_USE_STD_MODULE
#else
#  define EXPORT_AUXILIA
#  if AC_DEBUG_ENABLED
#    include <spdlog/spdlog.h> // IWYU pragma: export
#  endif
#endif

/// @note by current time the library was written, GNU on Windows seems failed
/// to perform linking for `stacktrace` and `spdlog`.
#include <stacktrace>
#if !AC_USE_STD_FMT
template <> struct ::fmt::formatter<::std::stacktrace> {
  constexpr auto parse(fmt::format_parse_context &ctx) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const ::std::stacktrace &st, FormatContext &ctx) const {
    auto out = ctx.out();
    for (const auto &entry : st) {
      out = fmt::format_to(out,
                           "{} {} {} {}\n",
                           entry.description(),
                           entry.native_handle(),
                           entry.source_file(),
                           entry.source_line());
    }
    return out;
  }
};
#endif
#define AC_FORMAT(...) (AC_STD_OR_FMT format("" __VA_ARGS__))
#define AC_STACKTRACE AC_FORMAT("\n{}", ::std::stacktrace::current())
#define AC_UNREACHABLE_IMPL                                                    \
  [[assume(false)]];                                                           \
  [[unlikely]] std::unreachable(); // 'not all control paths return a value'

#ifdef AC_DEBUG_ENABLED
#  if __has_include(<spdlog/spdlog.h>)
#    define AC_DEBUG_LOGGING(_level_, _msg_, ...)                              \
      ::spdlog::_level_(_msg_ __VA_OPT__(, ) __VA_ARGS__);
#  elif __has_include(<fmt/core.h>)
#    define AC_DEBUG_LOGGING(_level_, _msg_, ...)                              \
      fmt::print(#_level_ ": " _msg_ __VA_OPT__(, ) __VA_ARGS__);              \
      fmt::print("\n");
#  elif __has_include(<print>)
#    define AC_DEBUG_LOGGING(_level_, _msg_, ...)                              \
      std::print(#_level_ ": " _msg_ __VA_OPT__(, ) __VA_ARGS__);              \
      std::print("\n");
#  elif __has_include(<format>)
#    define AC_DEBUG_LOGGING(_level_, _msg_, ...)                              \
      std::clog << std::format(#_level_ ": " _msg_ __VA_OPT__(, ) __VA_ARGS__) \
                << std::endl;
#  else
#    define AC_DEBUG_LOGGING(_level_, _msg_, ...)
#  endif
namespace accat::auxilia::details {
EXPORT_AUXILIA
struct _dbg_block_helper_struct_ {};
template <class Fun_> struct _dbg_block_ {
  inline constexpr _dbg_block_(Fun_ f) noexcept(noexcept(f())) {
    (void)f();
    this->~_dbg_block_();
  }
  inline constexpr ~_dbg_block_() noexcept = default;
};
EXPORT_AUXILIA
template <class Fun_>
AC_STATIC_CALL_OPERATOR inline constexpr auto
operator*(_dbg_block_helper_struct_, Fun_ f_)
    AC_CONST_CALL_OPERATOR noexcept(noexcept(f_())) -> _dbg_block_<Fun_> {
  return {f_};
}
} // namespace accat::auxilia::details

#  define AC_DEBUG_BLOCK                                                       \
    ::accat::auxilia::details::_dbg_block_helper_struct_{} *[&]()              \
        ->void // NOLINT(bugprone-macro-parentheses)

#  define AC_DEBUG_ONLY(...) __VA_ARGS__
#  define AC_STATIC_ASSERT(_cond_, ...)                                        \
    static_assert(_cond_ __VA_OPT__(, ) __VA_ARGS__)

/// @note detect if gtest was included, if so, emit a different message.
#  ifdef GTEST_API_
#    define AC_DEBUG_LOGGING_SETUP(_exec_, _level_, _msg_, ...)                \
      ::spdlog::set_level(spdlog::level::_level_);                             \
      ::spdlog::set_pattern("[\033[33m" #_exec_ ":\033[0m %^%5l%$] %v");       \
      AC_DEBUG_LOGGING(_level_,                                                \
                       "\033[33m" _msg_ " with gtest."                         \
                       "\033[0m" __VA_OPT__(, ) __VA_ARGS__)
#  else
#    define AC_DEBUG_LOGGING_SETUP(_exec_, _level_, _msg_, ...)                \
      ::spdlog::set_level(spdlog::level::_level_);                             \
      ::spdlog::set_pattern("[%^%l%$] %v");                                    \
      AC_DEBUG_LOGGING(_level_,                                                \
                       "\033[33m" _msg_ "."                                    \
                       "\033[0m" __VA_OPT__(, ) __VA_ARGS__)
#  endif
#else
#  define AC_DEBUG_LOGGING(...) (void)0;
#endif

/// @note magic_enum seems to require __PRETTY_FUNCTION__ to be defined
#if !defined(__PRETTY_FUNCTION__) && !defined(__INTELLISENSE__)
#  if defined(__FUNCSIG__)
#    define __PRETTY_FUNCTION__ __FUNCSIG__
#  else
#    define __PRETTY_FUNCTION__ __func__
#  endif
#endif

#ifdef __clang__
#  define AC_DEBUG_BREAK_IMPL_ __builtin_debugtrap();
#elif defined(__GNUC__)
#  define AC_DEBUG_BREAK_IMPL_ __builtin_trap();
#elif defined(_MSC_VER)
#  define AC_DEBUG_BREAK_IMPL_ __debugbreak();
#else
#  include <csignal>
#  define AC_DEBUG_BREAK_IMPL_ raise(SIGTRAP);
#endif

#ifdef _WIN32
#  include <vcruntime.h>
extern "C" __declspec(dllimport) int __stdcall IsDebuggerPresent();
extern "C" __declspec(dllimport) int SetConsoleOutputCP(unsigned int);
#elif defined(__linux__)
#  include <sys/ptrace.h>
#endif

namespace accat::auxilia::details {
AC_FLATTEN inline bool _is_debugger_present() noexcept {
#ifdef _WIN32
  return ::IsDebuggerPresent();
#elif defined(__linux__)
  return ::ptrace(PTRACE_TRACEME, 0, NULL, 0) == -1;
#else
  // not implemented
  return false;
#endif
}
AC_FLATTEN inline void _set_console_output_cp_utf8() noexcept {
#ifdef _WIN32
  ::SetConsoleOutputCP(65001);
#else
  system("export LANG=en_US.UTF-8");
  system("export LC_CTYPE=en_US.UTF-8");
#endif
}
} // namespace accat::auxilia::details

#include <cstdio>
#define AC_DEBUG_BREAK                                                         \
  do {                                                                         \
    if (::accat::auxilia::details::_is_debugger_present()) {                   \
      AC_DEBUG_BREAK_IMPL_                                                     \
    } else {                                                                   \
      ::std::fprintf(                                                          \
          stderr,                                                              \
          "Fatal: program exits abnormally. please consult debugger.\n");      \
      ::std::exit(3);                                                          \
    }                                                                          \
  } while (false);

#ifdef AC_DEBUG_ENABLED
#  define AC_AMBIGUOUS_ELSE_BLOCKER                                            \
    switch (0)                                                                 \
    case 0:                                                                    \
    default:
#  define AC_FILENAME (::std::source_location::current().file_name())
#  define AC_FUNCTION_NAME AC_DEBUG_FUNCTION_NAME
#  define AC_LINE (::std::source_location::current().line())
#  define AC_COLUMN (::std::source_location::current().column())
#  define AC_PRINT_ERROR_MSG_IMPL_WITH_MSG(x, _msg_)                           \
    spdlog::critical("in file {0}, line {2} column {3},\n"                     \
                     "           function {1},\n"                              \
                     "Constraints not satisfied:\n"                            \
                     "           Expect `{4}` to be true.\n"                   \
                     "Additional message: {5}\n"                               \
                     "Stacktrace:{6}",                                         \
                     AC_FILENAME,                                              \
                     AC_FUNCTION_NAME,                                         \
                     AC_LINE,                                                  \
                     AC_COLUMN,                                                \
                     #x,                                                       \
                     (_msg_),                                                  \
                     ::accat::auxilia::details::_is_debugger_present()         \
                         ? "<please consult debugger>"                         \
                         : AC_STACKTRACE);
#  if defined(GTEST_API_) && defined(__cpp_exceptions) &&                      \
      (!defined(AC_IGNORE_GTEST)) && __cpp_exceptions
#    define AC_RUNTIME_REQUIRE_IMPL_WITH_MSG(_cond_, _msg_)                    \
      AC_AMBIGUOUS_ELSE_BLOCKER                                                \
      if ((_cond_))                                                            \
        ;                                                                      \
      else {                                                                   \
        throw std::runtime_error(_msg_);                                       \
      }
#  else
#    define AC_RUNTIME_REQUIRE_IMPL_WITH_MSG(_cond_, _msg_)                    \
      AC_AMBIGUOUS_ELSE_BLOCKER                                                \
      if (!!(_cond_))                                                          \
        ;                                                                      \
      else {                                                                   \
        AC_PRINT_ERROR_MSG_IMPL_WITH_MSG(_cond_, _msg_)                        \
        AC_DEBUG_BREAK                                                         \
      }
#  endif

#  define AC_RUNTIME_REQUIRE_1(_cond_, ...)                                    \
    AC_RUNTIME_REQUIRE_IMPL_WITH_MSG(_cond_, AC_FORMAT(__VA_ARGS__))

// unused
#  define AC_RUNTIME_REQUIRE_0(_cond_) AC_RUNTIME_REQUIRE_1(_cond_, #_cond_)

#  define AC_RUNTIME_ASSERT(...) AC_VFUNC(AC_RUNTIME_REQUIRE, __VA_ARGS__)

#  define AC_PRECONDITION(...) AC_VFUNC(AC_RUNTIME_REQUIRE, __VA_ARGS__)

#  define AC_NOEXCEPT_IF(...) // nothing
#  define AC_NOEXCEPT         // nothing
#  define AC_TODO_(...)                                                        \
    AC_RUNTIME_ASSERT(false, "Not implemented: " #__VA_ARGS__);                \
    std::abort(); // shut up the warning 'not all control paths return a
                  // value'
#  define AC_UNREACHABLE(...)                                                  \
    AC_RUNTIME_ASSERT(false, "Unreachable code.");                             \
    AC_UNREACHABLE_IMPL

#else
// if debug was turned off, do nothing.
#  define AC_RUNTIME_ASSERT(...) (void)0;
#  define AC_PRECONDITION(...) (void)0;
#  define AC_DEBUG_LOGGING_SETUP(...) (void)0;
#  define AC_DEBUG_BLOCK [&]()->void
#  define AC_DEBUG_ONLY(...)
#  define AC_STATIC_ASSERT(...) static_assert(...)
#  define AC_DBG_BREAK (void)0;
#  define AC_NOEXCEPT_IF(...) noexcept(__VA_ARGS__)
#  define AC_NOEXCEPT noexcept
#  define AC_UNREACHABLE(...) AC_UNREACHABLE_IMPL
#  if defined(__cpp_exceptions) && __cpp_exceptions
#    include <stdexcept>
#    define AC_TODO_(...)                                                      \
      throw ::std::logic_error(                                                \
          ::std::format("TODO" __VA_OPT__(": ") #__VA_ARGS__));
#  elif __has_include(<spdlog/spdlog.h>)
#    define AC_TODO_(...)                                                      \
      AC_DEBUG_LOGGING(critical, "TODO" __VA_OPT__(": ") #__VA_ARGS__);        \
      AC_DEBUG_BREAK
#  else
#    define AC_TODO_(...)                                                      \
      fprintf(stderr, "TODO: " #__VA_ARGS__ "\n");                             \
      AC_DEBUG_BREAK
#  endif
#endif

#if defined(__cpp_exceptions) && __cpp_exceptions
#  define AC_THROW_OR_DIE_(_msg_) throw std::runtime_error(_msg_)
#  undef AC_NOEXCEPT
#  define AC_NOEXCEPT // nothing
#else
#  define AC_THROW_OR_DIE_(_msg_)                                              \
    do {                                                                       \
      fprintf(stderr, "Fatal error: %s\n", _msg_);                             \
      AC_DEBUG_BREAK                                                           \
      std::abort();                                                            \
    } while (false)
#endif

#ifndef AC_HAS_EXPLICIT_THIS_PARAMETER
#  if (defined(__cpp_explicit_this_parameter) &&                               \
       __cpp_explicit_this_parameter >= 202110L) ||                            \
      (defined(_MSC_VER) && defined(_HAS_CXX23) && _HAS_CXX23)
#    define AC_HAS_EXPLICIT_THIS_PARAMETER 1
#  else
// workaround
#    define AC_HAS_EXPLICIT_THIS_PARAMETER 0
#  endif
#endif

namespace accat::auxilia::details {
/// @see
/// https://stackoverflow.com/questions/32432450/what-is-standard-defer-finalizer-implementation-in-c
EXPORT_AUXILIA
struct _deferer_helper_struct_ {};
template <class Fun_> struct _deferrer_ {
  Fun_ f_;
  inline constexpr _deferrer_(Fun_ f_) noexcept : f_(f_) {}
  inline constexpr ~_deferrer_() noexcept(noexcept(f_())) { f_(); }
};
EXPORT_AUXILIA
template <class Fun_>
inline AC_CONSTEXPR20 auto operator*(_deferer_helper_struct_, Fun_ f_) noexcept
    -> _deferrer_<Fun_> {
  return {f_};
}
} // namespace accat::auxilia::details

#define AC_DEFER                                                               \
  const auto AC_EXPAND_COUNTER(_accat_auxilia_details_defer_block_at_) =       \
      ::accat::auxilia::details::_deferer_helper_struct_{} *[&]()

#ifdef AC_DEBUG_ENABLED
#  define AC_POSTCONDITION(...) AC_DEFER{AC_RUNTIME_ASSERT(__VA_ARGS__)};
#else
#  define AC_POSTCONDITION(...) (void)0;
#endif

// NOLINTBEGIN(bugprone-macro-parentheses)
/// @def AC_BITMASK_OPS
/// @brief define the basic bitmask operations for the given bitmask
/// type. the bitmask type should be an enum class.
/// @param _bitmask_ the bitmask type
/// @note this macro was borrowed from Microsoft's STL implementation.
#define AC_BITMASK_OPS(_bitmask_)                                              \
  [[nodiscard]]                                                                \
  inline constexpr _bitmask_ operator&(                                        \
      _bitmask_ _left_,                                                        \
      _bitmask_ _right_) noexcept { /* return _left_ & _right_ */              \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return (static_cast<_bitmask_>(static_cast<_intergral_type_>(_left_) &     \
                                   static_cast<_intergral_type_>(_right_)));   \
  }                                                                            \
  [[nodiscard]]                                                                \
  inline constexpr _bitmask_ operator|(                                        \
      _bitmask_ _left_,                                                        \
      _bitmask_ _right_) noexcept { /* return _left_ | _right_ */              \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return (static_cast<_bitmask_>(static_cast<_intergral_type_>(_left_) |     \
                                   static_cast<_intergral_type_>(_right_)));   \
  }                                                                            \
  [[nodiscard]]                                                                \
  inline constexpr _bitmask_ operator^(                                        \
      _bitmask_ _left_,                                                        \
      _bitmask_ _right_) noexcept { /* return _left_ ^ _right_ */              \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return (static_cast<_bitmask_>(static_cast<_intergral_type_>(_left_) ^     \
                                   static_cast<_intergral_type_>(_right_)));   \
  }                                                                            \
  [[nodiscard]]                                                                \
  inline constexpr _bitmask_ &operator&=(                                      \
      _bitmask_ &_left_,                                                       \
      _bitmask_ _right_) noexcept { /* return _left_ &= _right_ */             \
    return (_left_ = _left_ & _right_);                                        \
  }                                                                            \
  [[nodiscard]]                                                                \
  inline constexpr _bitmask_ &operator|=(                                      \
      _bitmask_ &_left_,                                                       \
      _bitmask_ _right_) noexcept { /* return _left_ |= _right_ */             \
    return (_left_ = _left_ | _right_);                                        \
  }                                                                            \
  [[nodiscard]]                                                                \
  inline constexpr _bitmask_ &operator^=(                                      \
      _bitmask_ &_left_,                                                       \
      _bitmask_ _right_) noexcept { /* return _left_ ^= _right_ */             \
    return (_left_ = _left_ ^ _right_);                                        \
  }                                                                            \
  [[nodiscard]]                                                                \
  inline constexpr _bitmask_ operator~(                                        \
      _bitmask_ _left_) noexcept { /* return ~_left_ */                        \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return (static_cast<_bitmask_>(~static_cast<_intergral_type_>(_left_)));   \
  }                                                                            \
  [[nodiscard]]                                                                \
  inline constexpr bool operator==(                                            \
      _bitmask_ _value_,                                                       \
      std::underlying_type_t<_bitmask_>                                        \
          _zero_) noexcept { /* return _left_ == _right_ */                    \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return static_cast<_intergral_type_>(_value_) == _zero_;                   \
  }                                                                            \
  [[nodiscard]]                                                                \
  inline constexpr bool operator!(                                             \
      _bitmask_ _left_) noexcept { /* return !_left_ */                        \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return !static_cast<_intergral_type_>(_left_);                             \
  }

/// @def AC_BITMASK_OPSNESTED
/// @brief Useful if the enum class is nested inside a template class(usually
/// you won't do this).
#define AC_BITMASK_OPSNESTED_(_bitmask_)                                       \
  [[nodiscard]] friend inline constexpr _bitmask_ operator&(                   \
      _bitmask_ _left_,                                                        \
      _bitmask_ _right_) noexcept { /* return _left_ & _right_ */              \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return (static_cast<_bitmask_>(static_cast<_intergral_type_>(_left_) &     \
                                   static_cast<_intergral_type_>(_right_)));   \
  }                                                                            \
  [[nodiscard]] friend inline constexpr _bitmask_ operator|(                   \
      _bitmask_ _left_,                                                        \
      _bitmask_ _right_) noexcept { /* return _left_ | _right_ */              \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return (static_cast<_bitmask_>(static_cast<_intergral_type_>(_left_) |     \
                                   static_cast<_intergral_type_>(_right_)));   \
  }                                                                            \
  [[nodiscard]] friend inline constexpr _bitmask_ operator^(                   \
      _bitmask_ _left_,                                                        \
      _bitmask_ _right_) noexcept { /* return _left_ ^ _right_ */              \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return (static_cast<_bitmask_>(static_cast<_intergral_type_>(_left_) ^     \
                                   static_cast<_intergral_type_>(_right_)));   \
  }                                                                            \
  [[nodiscard]] friend inline constexpr _bitmask_ &operator&=(                 \
      _bitmask_ &_left_,                                                       \
      _bitmask_ _right_) noexcept { /* return _left_ &= _right_ */             \
    return (_left_ = _left_ & _right_);                                        \
  }                                                                            \
  [[nodiscard]] friend inline constexpr _bitmask_ &operator|=(                 \
      _bitmask_ &_left_,                                                       \
      _bitmask_ _right_) noexcept { /* return _left_ |= _right_ */             \
    return (_left_ = _left_ | _right_);                                        \
  }                                                                            \
  [[nodiscard]] friend inline constexpr _bitmask_ &operator^=(                 \
      _bitmask_ &_left_,                                                       \
      _bitmask_ _right_) noexcept { /* return _left_ ^= _right_ */             \
    return (_left_ = _left_ ^ _right_);                                        \
  }                                                                            \
  [[nodiscard]] friend inline constexpr _bitmask_ operator~(                   \
      _bitmask_ _left_) noexcept { /* return ~_left_ */                        \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return (static_cast<_bitmask_>(~static_cast<_intergral_type_>(_left_)));   \
  }                                                                            \
  [[nodiscard]]                                                                \
  friend inline constexpr bool operator==(                                     \
      _bitmask_ _value_,                                                       \
      std::underlying_type_t<_bitmask_>                                        \
          _zero_) noexcept { /* return _left_ == _right_ */                    \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return static_cast<_intergral_type_>(_value_) == _zero_;                   \
  }                                                                            \
                                                                               \
  [[nodiscard]]                                                                \
  friend inline constexpr bool operator!(                                      \
      _bitmask_ _left_) noexcept { /* return !_left_ */                        \
    using _intergral_type_ = ::std::underlying_type_t<_bitmask_>;              \
    return !static_cast<_intergral_type_>(_left_);                             \
  }                                                                            \
// NOLINTEND(bugprone-macro-parentheses)
