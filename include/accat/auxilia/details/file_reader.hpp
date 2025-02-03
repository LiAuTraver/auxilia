#pragma once

#include "./Status.hpp"
#include "./config.hpp"
#include <algorithm>
#include <bit>
#include <cstddef>
#include <utility>
#include <vector>
#include <filesystem>
#include <fstream>
#include <ranges>
#include <future>

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

  return OkStatus();
}
} // namespace accat::auxilia::details
#pragma warning(push)
#pragma warning(disable : 4702)
EXPORT_AUXILIA
namespace accat::auxilia {

#if __cpp_lib_ranges_chunk >= 202202L
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

/// @brief Asynchronously execute a function with the given arguments
/// @note just a wrapper, but it's REAL async
auto async(auto &&func, auto... args) -> decltype(auto)
  requires std::invocable<decltype(func), decltype(args)...>
{
  return std::async(std::launch::async,
                    std::forward<decltype(func)>(func),
                    std::forward<decltype(args)>(args)...);
}
} // namespace accat::auxilia

namespace accat::auxilia {
/// @brief a simple file reader that reads the contents of a file
/// @note the file reader is not thread-safe, and will consume a lot of memory
/// if the file is too big.
/// @deprecated will be removed in the future
class file_reader {
public:
  using path_t = path;
  using string_t = string;
  using ifstream_t = ifstream;
  using ostringstream_t = ostringstream;

public:
  inline explicit constexpr file_reader(path_t path_)
      : filePath(std::move(path_)) {}
  inline constexpr ~file_reader() = default;

public:
  [[nodiscard]] inline string_t get_contents() const {
    ifstream_t file(filePath);
    if (not file)
      return {};

    ostringstream_t buffer;
    buffer << file.rdbuf();
    return buffer.str();
  }
  [[nodiscard]] inline path_t filepath() const { return filePath; }

private:
  const path_t filePath;
};
} // namespace accat::auxilia
