//===-------- auxfwd.hpp - Auxilia Library Header --------*- C++ -*-===//
//
// Part of the auxilia project.
// Licensed under the Apache License v2.0.
//
// auxilia - from Latin word, meaning "help, aid, or assistance".
// A C++23 library for general-purpose utilities.
//
// This header file forward declares all auxilia class and function utilities.
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
// it's recommended better to include std headers directly rather than doing
// this. however, I choose not to re-modify it.
namespace
#if defined(__GLIBCXX__) && !defined(_WIN32)
    std::inline __cxx11
#elif defined(_LIBCPP_VERSION) && !defined(_WIN32)
    std::inline __1
#else
    std
#endif
{
template <class, class, class> class basic_string;
} // namespace std::inline __cxx11
namespace
#if defined(_LIBCPP_VERSION) && !defined(_WIN32)
    std::inline __1
#else
    std
#endif
{
template <class> struct hash;
template <class> struct equal_to;
template <class, class> class basic_string_view;
template <class> class allocator;
template <class> struct char_traits;
} // namespace std::inline __1

namespace accat::auxilia {
#if not defined(_MSC_VER) || defined(__clang__)
/// @file accat/auxilia/container/bitset.hpp
template <size_t> class bitset;
#endif
/// @file accat/auxilia/meta/type_traits.hpp
template <typename, size_t> struct basic_chars_storage;
/// @file accat/auxilia/meta/container_traits.hpp
template <typename> struct container_traits;
/// @file accat/auxilia/container/flat_map.hpp
template <typename, typename, typename, typename, typename> class flat_map;
/// @file accat/auxilia/container/chars.hpp
template <typename, size_t, typename> class basic_chars;
/// @file accat/auxilia/meta/Monostate.hpp
struct Monostate;
/// @file accat/auxilia/base/format.hpp
enum class FormatPolicy : unsigned char;
struct Printable;
struct Viewable;

/// @file accat/auxilia/utility/program_options.hpp
namespace program_options {
class Option;
class Parser;
} // namespace program_options

/// @file accat/auxilia/utility/Property.hpp
template <typename Instance,
          typename Field,
          typename ReturnType,
          Field (Instance::*getter)() const,
          ReturnType (Instance::*setter)(const Field &)>
struct Property;
/// @file accat/auxilia/status/Status.hpp
class Status;
/// @file accat/auxilia/status/StatusOr.hpp
template <typename> class StatusOr;
/// @file accat/auxilia/container/Variant.hpp
template <typename...> class Variant;
/// @file accat/auxilia/container/Trie.hpp
template <typename, typename, typename, typename, typename> class Trie;
/// @file accat/auxilia/memory/memory.hpp
class MemoryPool;
} // namespace accat::auxilia
