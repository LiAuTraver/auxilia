#pragma once

#include "./config.hpp"
#include "./StatusOr.hpp"

namespace accat::auxilia::details {
template <typename TargetType>
inline Status check_file(const std::filesystem::path &path) noexcept {
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

  return {};
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
    return {std::move(buffer.str())};

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
std::vector<std::byte> as_raw_bytes(const std::basic_string<CharType> &data) {
  // clang-format off
  return data 
          | std::views::transform([](auto &&c) {
              return static_cast<std::byte>(c);
            })
          | std::views::common
          | std::ranges::to<std::vector<std::byte>>();
  // clang-format on
}
// read raw bytes
template <std::endian Endianess = std::endian::native>
inline StatusOr<std::vector<std::byte>>
read_raw_bytes(const std::filesystem::path &path) {
  if (auto res = read_as_bytes<char, Endianess, char>(path); !res) {
    return {res};
  } else {
    return as_raw_bytes(*std::move(res));
  }
}
#endif
} // namespace accat::auxilia
