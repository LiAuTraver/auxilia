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
#  if !defined(AC_USE_STD_FMT) && __has_include(<fmt/format.h>)
#    define AC_USE_STD_FMT 0
#  endif
#endif

/// @note I use Source Code Annotation sometimes; but libstdc++
/// doesn't support; so we manually define it here.
#if __has_include(<sal.h>)
#  include <sal.h>
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
#else
#  define AC_USE_STD_FMT 1
#  include <format>
#  include <iostream>
#  if __has_include(<print>)
#    include <print>
#  endif
#endif

// although C++ standard may be updated, the constexpr feature may not be
// supported by all compilers.
#if __cpp_constexpr >= 202000L
#  define AC_CONSTEXPR20_ constexpr
#else
#  define AC_CONSTEXPR20_
#endif

#if __cpp_constexpr >= 202211L
#  define AC_CONSTEXPR23_ constexpr
#else
#  define AC_CONSTEXPR23_
#endif

#if __cpp_consteval >= 202211L
#  define AC_CONSTEVAL_ consteval
#else
#  define AC_CONSTEVAL_ AC_CONSTEXPR20_
#endif

#if __cpp_static_call_operator >= 202207L
#  define AC_STATIC_CALL_OPERATOR_ static
#  define AC_CONST_CALL_OPERATOR_
#else
#  define AC_STATIC_CALL_OPERATOR_
#  define AC_CONST_CALL_OPERATOR_ const
#endif

#if __has_cpp_attribute(nodiscard) >= 201907L
#  define AC_NODISCARD_REASON_(...) [[nodiscard(__VA_ARGS__)]]
#  define AC_NODISCARD_ [[nodiscard]]
#elif __has_cpp_attribute(nodiscard) >= 201603L
#  define AC_NODISCARD_REASON_(...) [[nodiscard]]
#  define AC_NODISCARD_ [[nodiscard]]
#else
#  define AC_NODISCARD_REASON_(...)
#  define AC_NODISCARD_
#endif

#ifdef __clang__
#  define AC_FLATTEN_ [[gnu::flatten]]
#  define AC_NO_SANITIZE_ADDRESS_ [[clang::no_sanitize("address")]]
#  define AC_FORCEINLINE_ [[clang::always_inline]]
#  define AC_DEBUG_FUNCTION_NAME_ __PRETTY_FUNCTION__
#elif defined(_MSC_VER)
#  define AC_FLATTEN_ [[msvc::flatten]]
#  define AC_NO_SANITIZE_ADDRESS_ __declspec(no_sanitize_address)
#  define AC_FORCEINLINE_ [[msvc::forceinline]]
#  define AC_DEBUG_FUNCTION_NAME_ __FUNCSIG__
#elif defined(__GNUC__)
#  define AC_FLATTEN_ [[gnu::flatten]]
#  define AC_NO_SANITIZE_ADDRESS_ __attribute__((no_sanitize("address")))
#  define AC_FORCEINLINE_ [[gnu::always_inline]]
#  define AC_DEBUG_FUNCTION_NAME_ __PRETTY_FUNCTION__
#else
#  define AC_FLATTEN_
#  define AC_NO_SANITIZE_ADDRESS_
#  define AC_FORCEINLINE_ inline
#  define AC_DEBUG_FUNCTION_NAME_ __func__
#endif

#ifdef _WIN32
#  define AC_NOVTABLE_ __declspec(novtable)
#  define AC_EMPTY_BASES_ __declspec(empty_bases)
#else
#  define AC_NOVTABLE_
#  define AC_EMPTY_BASES_
#endif

#ifdef AUXILIA_BUILD_MODULE
#  define EXPORT_AUXILIA export
#  define AUXILIA_USE_STD_MODULE
#else
#  define EXPORT_AUXILIA
#  if AC_DEBUG_ENABLED
#    include <spdlog/spdlog.h>
#  endif
#endif

/// @note by current time the library was written, GNU on Windows seems failed
/// to perform linking for `stacktrace` and `spdlog`.
#if !AC_USE_STD_FMT && defined(_WIN32)
#  include <stacktrace>
#  if !AC_USE_STD_FMT
#    define AC_STACKTRACE (::std::format("\n{}", ::std::stacktrace::current()))
#  else
template <>
struct ::fmt::formatter<::std::stacktrace> : ::fmt::formatter<::std::string> {
  template <typename FormatContext>
  auto format(const ::std::stacktrace &st, FormatContext &ctx)
      -> decltype(ctx.out()) {
    std::string result;
    for (const auto &entry : st) {
      result += fmt::format("{} {} {} {}\n",
                            entry.description(),
                            entry.native_handle(),
                            entry.source_file(),
                            entry.source_line());
    }
    return ::fmt::format_to(ctx, "{}", result);
  }
};
#    define AC_STACKTRACE ::fmt::format("\n{}", ::std::stacktrace::current())
#  endif
#else
#  define AC_STACKTRACE ("<no further information>")
#endif

#define AC_UNREACHABLE_IMPL                                                    \
  [[assume(false)]];                                                           \
  [[unlikely]] std::unreachable(); // 'not all control paths return a value'

#ifdef AC_DEBUG_ENABLED
#  if __has_include(<spdlog/spdlog.h>)
#    define AC_DEBUG_LOGGING(_level_, _msg_, ...)                              \
      ::spdlog::_level_(_msg_ __VA_OPT__(, ) __VA_ARGS__);
#  elif __has_include(<print>)
#    define AC_DEBUG_LOGGING(_level_, _msg_, ...)                              \
      std::print(#_level_ ": " _msg_ __VA_OPT__(, ) __VA_ARGS__);              \
      std::print("\n");
#  elif __has_include(<format>)
#    define AC_DEBUG_LOGGING(_level_, _msg_, ...)                              \
      std::cout << std::format(#_level_ ": " _msg_ __VA_OPT__(, ) __VA_ARGS__) \
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
AC_STATIC_CALL_OPERATOR_ inline constexpr auto
operator*(_dbg_block_helper_struct_, Fun_ f_)
    AC_CONST_CALL_OPERATOR_ noexcept(noexcept(f_())) -> _dbg_block_<Fun_> {
  return {f_};
}
} // namespace accat::auxilia::details

#  define AC_DEBUG_BLOCK                                                       \
    ::accat::auxilia::details::_dbg_block_helper_struct_{} *[&]()              \
        ->void // NOLINT(bugprone-macro-parentheses)

#  define AC_DEBUG_ONLY(...) __VA_ARGS__

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

/// @note magic_enum seems to require __PRETTY_FUNCTION__ to be defined; also
/// language server clangd does not work.
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
extern "C" __declspec(dllimport) int __stdcall IsDebuggerPresent();
extern "C" __declspec(dllimport) int SetConsoleOutputCP(unsigned int);
#elif defined(__linux__)
#  include <sys/ptrace.h>
#endif

namespace accat::auxilia::details {
AC_FLATTEN_ inline bool _is_debugger_present() noexcept {
#ifdef _WIN32
  return ::IsDebuggerPresent();
#elif defined(__linux__)
  return ::ptrace(PTRACE_TRACEME, 0, NULL, 0) == -1;
#else
  // not implemented
  return false;
#endif
}
} // namespace accat::auxilia::details
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
#  define AC_FUNCTION_NAME AC_DEBUG_FUNCTION_NAME_
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

#  if AC_USE_STD_FMT
#    define AC_RUNTIME_REQUIRE_IMPL(_cond_, ...)                               \
      AC_RUNTIME_REQUIRE_IMPL_WITH_MSG(                                        \
          _cond_ __VA_OPT__(, ::std::format(__VA_ARGS__)))
#  else
#    define AC_RUNTIME_REQUIRE_IMPL(_cond_, ...)                               \
      AC_RUNTIME_REQUIRE_IMPL_WITH_MSG(                                        \
          _cond_ __VA_OPT__(, ::fmt::format(__VA_ARGS__)))
#  endif

#  define AC_RUNTIME_ASSERT(_cond_, ...)                                       \
    AC_RUNTIME_REQUIRE_IMPL(_cond_ __VA_OPT__(, ) __VA_ARGS__);

#  define AC_PRECONDITION(...) AC_RUNTIME_REQUIRE_IMPL(__VA_ARGS__)
#  define AC_NOEXCEPT_IF(...) // nothing
#  define AC_NOEXCEPT         // nothing
#  define AC_TODO_(...)                                                        \
    AC_RUNTIME_ASSERT(false, "Not implemented: " #__VA_ARGS__);                \
    std::abort(); // shut up the warning 'not all control paths return a value'
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
#  define AC_DBG_BREAK (void)0;
#  define AC_NOEXCEPT_IF(...) noexcept(__VA_ARGS__)
#  define AC_NOEXCEPT noexcept
#  define AC_UNREACHABLE(...) AC_UNREACHABLE_IMPL
#  if defined(__cpp_exceptions) && __cpp_exceptions
#    include <stdexcept>
#    define AC_TODO_(...)                                                      \
      throw ::std::logic_error(::std::format("TODO: " #__VA_ARGS__));
#  elif __has_include(<spdlog / spdlog.h>)
#    define AC_TODO_(...) AC_DEBUG_LOGGING(critical, "TODO: " #__VA_ARGS__);
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
inline AC_CONSTEXPR20_ auto operator*(_deferer_helper_struct_, Fun_ f_) noexcept
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
#define AC_BITMASK_OPS_(_bitmask_)                                             \
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

#define AC_BITMASK_OPS_NESTED_(_bitmask_)                                      \
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
