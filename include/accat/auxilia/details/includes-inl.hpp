﻿#pragma once
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
#ifdef AC_USE_STD_MODULE
import std;
#else
#  include <algorithm>
#  include <atomic>
#  include <bit>
#  include <cmath>
#  include <compare>
#  include <concepts>
#  include <coroutine>
#  include <cstdint>
#  include <filesystem>
#  include <fstream>
#  include <functional>
#  include <future>
#  include <ios>
#  include <iosfwd>
#  include <iostream>
#  include <limits>
#  include <memory_resource>
#  include <mutex>
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
#  if __has_include(<version>)
#    include <version>
#  endif
/// @note I use Source Code Annotation sometimes; but libstdc++
/// doesn't support; so we manually define it here.
#  if __has_include(<sal.h>)
#    include <sal.h>
#  else
#    define _In_
#    define _Inout_
#    define _In_opt_
#    define _Out_
#    define _Outptr_
#    define _Outptr_result_maybenull_
#    define _Outptr_opt_
#  endif
#endif
////////////////////////////// END OF INCLUDES /////////////////////////////////
