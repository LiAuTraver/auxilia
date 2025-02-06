#pragma once
///////////////////////////// BEGIN OF INCLUDES ////////////////////////////////
#ifdef __clang__
#  pragma clang system_header
#elifdef __GNUC__
#  pragma GCC system_header
#elifdef _MSC_VER
#  pragma system_header
#else
#  pragma system_header
#endif
#if __has_include(<fmt/format.h>)
#  include <fmt/format.h>
#  include <fmt/ostream.h>
#  include <fmt/color.h>
#  include <fmt/std.h>
#  include <fmt/xchar.h>
#else
#  include <format>
#  include <iostream>
#  if __has_include(<print>)
#    include <print>
#  endif
#endif
#ifdef AC_USE_STD_MODULE
import std;
#else
#  include <algorithm>
#  include <any>
#  include <atomic>
#  include <bit>
#  include <cmath>
#  include <compare>
#  include <concepts>
#  include <cstdint>
#  include <filesystem>
#  include <fstream>
#  include <functional>
#  include <ios>
#  include <iostream>
#  include <limits>
#  include <memory_resource>
#  include <mutex>
#  include <ostream>
#  include <random>
#  include <ranges>
#  include <source_location>
#  include <sstream>
#  include <stdexcept>
#  include <string>
#  include <string_view>
#  include <type_traits>
#  include <typeinfo>
#  include <unordered_set>
#  include <utility>
#  include <variant>
#  include <version>
/// @note I use Source Code Annotation sometimes; but libstdc++
/// doesn't support; so we manually define it here.
#  if __has_include(<sal.h>)
#    include <sal.h>
#  else
#    define _In_
#    define _Inout_
#    define _Out_
#    define _Outptr_
#    define _Outptr_result_maybenull_
#    define _Outptr_opt_
#  endif
#endif
////////////////////////////// END OF INCLUDES /////////////////////////////////
