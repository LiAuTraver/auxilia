#pragma once

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <ios>
#include <optional>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <vector>

#include "accat/auxilia/base/config.hpp"
#include "accat/auxilia/base/macros.hpp"
#include "accat/auxilia/status/Status.hpp"
#include "accat/auxilia/status/StatusOr.hpp"

namespace accat::auxilia::details {
template <typename TargetType>
inline auto check_file(const std::filesystem::path &path) noexcept {
  if (not std::filesystem::exists(path))
    return NotFoundError("The file does not exist");

  // if (std::filesystem::file_size(path) % sizeof(TargetType))
  // ^^^^ this THROWs! ^^^^ we want `noexcept`

  auto ec = std::error_code{};
  auto fileSize = std::filesystem::file_size(path, ec);
  if (ec)
    return UnknownError(ec.message());
  if (fileSize % sizeof(TargetType))
    return InvalidArgumentError(
        "The file size is not a multiple of the target type size");

  return OkStatus();
}
} // namespace accat::auxilia::details
#pragma warning(push)
#pragma warning(disable : 4702)
EXPORT_AUXILIA
namespace accat::auxilia {

#if defined(__cpp_lib_ranges_chunk) && __cpp_lib_ranges_chunk >= 202202L
template <typename TargetType,
          std::endian Endianess = std::endian::native,
          typename CharType = char>
inline StatusOr<std::basic_string<CharType>>
read_as_bytes(const std::filesystem::path &path) {
  static_assert(Endianess == std::endian::native ||
                    Endianess == std::endian::big ||
                    Endianess == std::endian::little,
                "Unsupported endianess");

  if (auto res = details::check_file<TargetType>(path); !res)
    return {res};

  auto file =
      std::basic_ifstream<CharType>(path, std::ios::in | std::ios::binary);
  auto buffer = std::basic_ostringstream<CharType>{};
  buffer << file.rdbuf();

  if constexpr (Endianess == std::endian::native)
    return {std::move(buffer).str()};

  // differs from the native endianess; reverse the bytes
  auto data = std::move(buffer).str();
  std::ranges::for_each(data | std::views::chunk(sizeof(TargetType)),
                        std::ranges::reverse);
  return {std::move(data)};
}
#  pragma warning(pop)
#endif

#if defined(__cpp_lib_ranges_to_container) &&                                  \
    __cpp_lib_ranges_to_container >= 202202L
template <typename CharType = char>
auto as_raw_bytes(const std::basic_string<CharType> &data) {
  // clang-format off
  return data 
          | std::views::transform(&as<std::byte,CharType>)
          | std::views::common
          | std::ranges::to<std::vector<std::byte>>();
  // clang-format on
}
// read raw bytes
template <std::endian Endianess = std::endian::native>
inline StatusOr<std::vector<std::byte>>
read_raw_bytes(const std::filesystem::path &path) {
  if (auto res = read_as_bytes<char, Endianess, char>(path)) {
    return as_raw_bytes(*std::move(res));
  } else {
    return {std::move(res).as_status()};
  }
}
#endif

template <typename CharT = char>
inline std::optional<std::basic_string<CharT>>
readfile(const std::basic_string_view<CharT> myPath) {
  if constexpr (std::is_same_v<CharT, char>) {
    // use plain FILE* for char
    FILE *file = nullptr;
    AC_DEFER {
      // pass NULL to fclose is undefined behavior accodring to C standard.
      if (file)
        ::fclose(file);
    };
    if (::fopen_s(&file, myPath.data(), "rb") || !file)
      return std::nullopt;

    if (::fseek(file, 0, SEEK_END) != 0)
      return std::nullopt;

    const long mySize = ::ftell(file);
    if (mySize < 0)
      return std::nullopt;

    ::rewind(file);

    std::basic_string<CharT> myStr;
    myStr.resize_and_overwrite(static_cast<size_t>(mySize),
                               [&](CharT *const data, const size_t) {
                                 return ::fread(data, 1, mySize, file);
                               });

    return std::make_optional(std::move(myStr));
  }
  // fall back to filebuf for wchar_t, etc.
  // I probably never uses character type other than `char`;
  // anyway I used Claude Sonnet 4.5 to complete code below.
  // ^^^ Human Write / AI gen-ed vvv
  else {

    std::basic_filebuf<CharT> myfb;
    AC_DEFER { myfb.close(); };

    if (!myfb.open(myPath, std::ios::in | std::ios::binary))
      return std::nullopt;

    const auto mySize = myfb.pubseekoff(0, std::ios::end, std::ios::in);
    if (mySize < 0)
      return std::nullopt;

    myfb.pubseekpos(0, std::ios::in);

    std::basic_string<CharT> myStr;
    myStr.resize_and_overwrite(
        static_cast<size_t>(mySize), [&](CharT *const data, const size_t) {
          return static_cast<size_t>(myfb.sgetn(data, mySize));
        });

    return std::make_optional(std::move(myStr));
  }
}
template <typename CharT = char>
bool writefile(const std::filesystem::path &myPath,
               std::basic_string<CharT> &&myData) {
  const auto pathStr = myPath.string();
  if constexpr (std::is_same_v<CharT, char>) {
    FILE *file = nullptr;
    AC_DEFER {
      if (file)
        ::fclose(file);
    };
    if (::fopen_s(&file, pathStr.data(), "wb") || !file)
      return false;

    const size_t written = ::fwrite(myData.data(), 1, myData.size(), file);
    return written == myData.size();
  }
  // ditto.
  else {
    std::basic_filebuf<CharT> myfb;
    AC_DEFER { myfb.close(); };

    if (!myfb.open(pathStr.data(), std::ios::out | std::ios::binary))
      return false;

    const auto written = myfb.sputn(myData.data(), myData.size());
    return written == static_cast<std::streamsize>(myData.size());
  }
}
} // namespace accat::auxilia

namespace accat::auxilia::bit {
// helper to extract bits [Beg, End)
template <size_t Beg, size_t End, typename T>
inline constexpr T extract(const T value) noexcept {
  static_assert(Beg <= End, "Beg must be less than or equal to End");
  static_assert(End <= sizeof(T) * 8, "End must be within the bit width of T");

  constexpr T mask = (static_cast<T>(1) << (End - Beg)) - 1;
  return (value >> Beg) & mask;
}

// helper to concatenate bits from multiple [Begin, End) ranges
template <typename T, size_t Begin, size_t End, size_t... Rest>
inline constexpr T concat(const T value) noexcept {
  static_assert(sizeof...(Rest) % 2 == 0, "Rest must be a multiple of 2");
  if constexpr (sizeof...(Rest) == 0) {
    return extract<Begin, End>(value);
  } else {
    auto chunk = extract<Begin, End, T>(value);
    constexpr auto bitCount = End - Begin;
    return chunk | (concat<T, Rest...>(value) << bitCount);
  }
}

// helper to perform sign extension
// note: `NewWidthTy` shall be the same type as pc when used.
template <size_t OrigWidth, typename OrigTy, typename NewWidthTy>
inline constexpr auto sign_extend(const OrigTy value) noexcept {
  static_assert(sizeof(OrigTy) * 8 >= OrigWidth, "OrigTy is too small");
  static_assert(sizeof(NewWidthTy) * 8 >= OrigWidth, "NewWidthTy is too small");
  static_assert(std::is_integral_v<OrigTy>, "OrigTy must be an integral type");
  static_assert(std::is_integral_v<NewWidthTy>,
                "NewWidthTy must be an integral type");

  const auto signBit = (value >> (OrigWidth - 1)) & 1;
  if (signBit) {
    // if the sign bit is set, extend the sign bit to the left
    return value | ~((static_cast<OrigTy>(1) << OrigWidth) - 1);
  } else {
    // if the sign bit is not set, just return the value
    return value;
  }
}

// helper to decode 2's complement, return original value if sign bit is 1,
// otherwise return the negated value
template <typename T>
inline constexpr std::make_signed_t<T> two_complement(const T value) noexcept {
  static_assert(std::is_integral_v<T>, "T must be an integral type");
  const auto signBit = (value >> (sizeof(T) * 8 - 1)) & 1;
  if (signBit) {
    return value;
  } else {
    return ~value + 1;
  }
}
} // namespace accat::auxilia::bit
