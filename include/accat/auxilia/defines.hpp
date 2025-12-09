//===-------- defines.hpp - Auxilia Library Header -------*- C++ -*-===//
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

#include "base/macros.hpp"

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

/// @def AC_SPDLOG_INITIALIZATION(_exec_, _log_level_) initializes the
/// spdlog framework
/// @note only call it once in the whole exec; never call it twice.
#define AC_SPDLOG_INITIALIZATION(_exec_, _log_level_, ...)                     \
  [[maybe_unused]] __VA_ARGS__ static const auto AC_SPDLOG_INITIALIZATION =    \
      [](void) -> ::std::nullptr_t {                                           \
    AC_DEBUG_LOGGING(info, "\033[36mspdlog framework initialized.\033[0m")     \
    AC_DEBUG_LOGGING_SETUP(_exec_, _log_level_, "Debug mode enabled")          \
    return nullptr;                                                            \
  }();

#define return_if_not(_status_) AC_RETURN_IF_NOT(_status_)

#ifdef defer
#  warning "defer was already defined. please check the code."
#  pragma pop_macro("defer")
#endif
#define defer AC_DEFER
