//===-------- auxilia.hpp - Auxilia Library Header -------*- C++ -*-===//
//
// Part of the auxilia project.
// Licensed under the Apache License v2.0.
//
// auxilia - from Latin word, meaning "help, aid, or assistance".
// A C++23 library for general-purpose utilities.
//
// This header file includes all the macros provided by the auxilia library.
//
//
//   Copyright 2025 LiAuTraver
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.
//
//===---------------------------------------------------------------===//

#pragma once

#include "./details/macros.hpp"

#ifdef defer
#  warning "defer was already defined. please check the code."
#  pragma pop_macro("defer")
#endif
#define defer AC_DEFER

#if (defined(_MSVC_TRADITIONAL) && _MSVC_TRADITIONAL) && !defined(__clang__)
/// @brief MSVC traditional preprocessor
/// @def dbg(_level_, _msg_, ...)
/// @note MSVC cross-platform compatible preprocessor acts like clang and gcc.
/// @see MSVC experimental preprocessor
/// https://learn.microsoft.com/en-us/cpp/preprocessor/preprocessor-experimental-overview
#  pragma message "MSVC traditional preprocessor was used. "                   \
                  "no additional debug info will be provided. "                \
                  "to enable MSVC's new preprocessor, "                        \
                  "add compiler flag `/Zc:preprocessor`."
#  define dbg(_level_, _msg_, ...)                                             \
    AC_DEBUG_LOGGING(_level_, _msg_ __VA_OPT__(, ) __VA_ARGS__)
#  define contract_assert(...)
#  define contract_check(...) assert(__VA_ARGS__)
#  define precondition(...)
#  define postcondition(...)
#else
/// @def contract_assert(condition, message)
/// @brief if the condition is false, trigger a trap if debugger is attached,
///        otherwise, abort the program.
/// @note idea borrowed from c++2c's contract programming proposal.
///       However, this macro will only work if debug mode is enabled.
/// @see P2961R2
/// https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2023/p2961r2.pdf
#  define contract_assert(...) AC_RUNTIME_ASSERT(__VA_ARGS__)
/// @def contract_check(condition, message)
/// @brief handy macro for checking whether a condition holds true
#  define contract_check(_cond_) AC_RUNTIME_ASSERT(_cond_, "expect " #_cond_)
#  define precondition(...) AC_PRECONDITION(__VA_ARGS__)
#  define postcondition(...) AC_POSTCONDITION(__VA_ARGS__);
#  define dbg(...) AC_DEBUG_LOGGING(__VA_ARGS__)
#endif

#define dbg_block AC_DEBUG_BLOCK
#define dbg_only(...) AC_DEBUG_ONLY(__VA_ARGS__)
#define dbg_break AC_DEBUG_BREAK

/// @def TODO mimic from kotlin's `TODO` function, which throws an
/// exception and is also discoverable
/// by IDE(which is why this macro exists).
#define TODO(...) AC_TODO_(__VA_ARGS__)

/// @def AC_NOVTABLE
/// @note msvc-specific keyword to prevent vtable generation.
///     Used in(and should only in) interface classes(i.e., pure interface and
///     never instantiated directly).
/// @remark no gcc/clang equivalent.
#define AC_NOVTABLE AC_NOVTABLE_

/// @def AC_EMPTY_BASES
/// @note msvc-specific keyword to enforce empty base optimization.
/// @remark no gcc/clang equivalent.
#define AC_EMPTY_BASES AC_EMPTY_BASES_

/// @def AC_CONSTEVAL
/// @brief propagate consteval support
#define AC_CONSTEVAL AC_CONSTEVAL_

/// @def AC_CONSTEXPR20
/// @brief c++20-style constexpr support
#define AC_CONSTEXPR20 AC_CONSTEXPR20_

/// @def AC_CONSTEXPR23
/// @brief c++23-style constexpr support
#define AC_CONSTEXPR23 AC_CONSTEXPR23_

#define AC_CONST_CALL_OPERATOR AC_CONST_CALL_OPERATOR_
#define AC_STATIC_CALL_OPERATOR AC_STATIC_CALL_OPERATOR_
#define AC_DEBUG_FUNCTION_NAME AC_DEBUG_FUNCTION_NAME_

/// @def AC_NODISCARD
/// @brief nodiscard attribute
/// @note why this macro? mostly because attribute with `[[]]` costs more
/// typing,
///       especially if clangd is used. ;)
#define AC_NODISCARD AC_NODISCARD_
#define AC_NODISCARD_REASON(...) AC_NODISCARD_REASON_(__VA_ARGS__)

/// @def AC_FORCEINLINE
/// @brief function force inline attribute
/// @note the attribute is non-standard
///       but supported by all three major compilers.
#define AC_FORCEINLINE AC_FORCEINLINE_

/// @def AC_FLATTEN
/// @brief function flatten attribute
/// @note non-standard but well supported.
#define AC_FLATTEN AC_FLATTEN_

/// @def AC_NO_SANITIZE_ADDRESS
/// @brief function no address sanitizer attribute
/// @note non-standard but well supported.
#define AC_NO_SANITIZE_ADDRESS AC_NO_SANITIZE_ADDRESS_

/// @def AC_SPDLOG_INITIALIZATION(_exec_, _log_level_) initializes the
/// spdlog framework
/// @note only call it once in the whole exec; never call it twice.
#define AC_SPDLOG_INITIALIZATION(_exec_, _log_level_)                          \
  [[maybe_unused]]                                                             \
  const auto AC_SPDLOG_INITIALIZATION = [](void) -> ::std::nullptr_t {         \
    AC_DEBUG_LOGGING(info, "\033[36mspdlog framework initialized.\033[0m")     \
    AC_DEBUG_LOGGING_SETUP(_exec_, _log_level_, "Debug mode enabled")          \
    return nullptr;                                                            \
  }();

/// @def AC_BITMASK_OPS
/// @brief define the basic bitmask operations for the given bitmask
/// type. the bitmask type should be an enum class.
/// @param _bitmask_ the bitmask type
/// @note this macro was borrowed from Microsoft's STL implementation.
#define AC_BITMASK_OPS(_bitmask_) AC_BITMASK_OPS_(_bitmask_)

/// @def AC_BITMASK_OPS_NESTED
/// @brief Useful if the enum class is nested inside a template class(usually
/// you won't do this).
#define AC_BITMASK_OPS_NESTED(_bitmask_) AC_BITMASK_OPS_NESTED_(_bitmask_)
