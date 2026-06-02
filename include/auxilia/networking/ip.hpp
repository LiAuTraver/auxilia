#pragma once

#include <array>
#include <bit>
#include <cstddef>
#include <optional>
#include <ranges>
#include <span>
#include <type_traits>

#include "auxilia/base/config.hpp"
#include "auxilia/base/format.hpp"
#include "auxilia/status/StatusOr.hpp"

#include "os.hpp"
namespace auxilia::net::ip {
/// bytes stored in a host-byte-order
class address_v4 : Printable {
  friend class address;

  using underlying_bytes_type =
      std::array<std::underlying_type_t<std::byte>, 4ULL>;
  using raw_type = decltype(std::declval<details::in4_addr_t>().s_addr);

public:
  using bytes_type = std::array<std::byte, 4ULL>;
  using bytes_view_type = const std::span<std::byte, 4ULL>;
  using uint_type = uint32_t;

private:
  static_assert(sizeof(uint32_t) == 4 * sizeof(std::byte));
  static_assert(sizeof(uint32_t) == sizeof(raw_type));
  consteval address_v4(const unsigned char (&addr)[4]) noexcept
      : addr_(std::bit_cast<bytes_type>(addr)) {}

private:
  bytes_type addr_;

public:
  constexpr address_v4() noexcept {}
  constexpr address_v4(const bytes_type addr) noexcept : addr_(addr) {}

  address_v4(const address_v4 &) = default;
  address_v4(address_v4 &&) = default;
  address_v4 &operator=(const address_v4 &) = default;
  address_v4 &operator=(address_v4 &&) = default;

  static consteval address_v4 loopback() noexcept { return {{127, 0, 0, 1}}; }
  static consteval address_v4 broadcast() noexcept {
    return {{255, 255, 255, 255}};
  }
  static consteval address_v4 unspecified() noexcept { return {{0, 0, 0, 0}}; }
  static consteval auto family() noexcept { return family::v4; }
  static constexpr StatusOr<address_v4>
  from_str(const std::string_view str) noexcept {
    if (str == "localhost" || str == "127.0.0.1")
      return net::ip::address_v4::loopback();

    bytes_type bytes;
    size_t part = 0;
    unsigned value = 0;
    bool has_digit = false;
    for (const char ch : str) {
      if (ch >= '0' && ch <= '9') {
        has_digit = true;
        value = value * 10 + static_cast<unsigned>(ch - '0');
        if (value > 255)
          return InvalidArgumentError("{} exceeds 255", value);
      } else if (ch == '.') {
        if (!has_digit || part >= 3)
          return InvalidArgumentError("invalid IPv4 address format '{}'", str);
        bytes[part++] = static_cast<std::byte>(value);
        value = 0;
        has_digit = false;
      } else {
        return InvalidArgumentError("invalid IPv4 address format '{}'", str);
      }
    }
    if (!has_digit || part != 3)
      return InvalidArgumentError("invalid IPv4 address format '{}'", str);
    bytes[part] = static_cast<std::byte>(value);
    return address_v4(bytes);
  }

public:
  constexpr auto to_uint() const noexcept {
    return details::net2host(std::bit_cast<uint_type>(addr_));
  }
  constexpr auto native_handle() const noexcept -> details::in4_addr_t {
    details::in4_addr_t in4_addr;
    in4_addr.s_addr = std::bit_cast<raw_type>(addr_);
    return in4_addr;
  }
  constexpr auto to_bytes() const noexcept { return addr_; }
  auto to_string(const FormatPolicy policy) const -> string_type {
    if (policy == FormatPolicy::kBrief)
      return Format(to_uint());
    else if (policy == FormatPolicy::kDefault) {
      return Format("{}.{}.{}.{}",
                    std::to_integer<uint8_t>(addr_[0]),
                    std::to_integer<uint8_t>(addr_[1]),
                    std::to_integer<uint8_t>(addr_[2]),
                    std::to_integer<uint8_t>(addr_[3]));
    } else {
      return Format(std::bit_cast<underlying_bytes_type>(addr_));
    }
  }
  constexpr explicit operator uint_type() const noexcept { return to_uint(); }
  constexpr explicit(false) operator details::in4_addr_t() const noexcept {
    return native_handle();
  }
  constexpr explicit operator class address() const noexcept;
};
static_assert(std::is_trivially_destructible_v<address_v4>);
class address_v6 : Printable {
  friend class address;

  using underlying_bytes_type =
      std::array<std::underlying_type_t<std::byte>, 16ULL>;
  using raw_type = decltype(std::declval<details::in6_addr_t>().s6_addr);

public:
  using scope_id_type = unsigned short;
  using bytes_type = std::array<std::byte, 16ULL>;
  using bytes_view_type = const std::span<const std::byte, 16ULL>;

private:
  consteval address_v6(const unsigned char (&addr)[16],
                       const scope_id_type scope_id = 0) noexcept
      : addr_(std::bit_cast<bytes_type>(addr)), scope_id_(scope_id) {}
  static_assert(sizeof(bytes_type) == 16 * sizeof(std::byte));
  static_assert(sizeof(bytes_type) == sizeof(raw_type));

private:
  bytes_type addr_;
  scope_id_type scope_id_;

public:
  constexpr address_v6() noexcept {}
  constexpr address_v6(const bytes_type addr,
                       const scope_id_type scope_id = 0) noexcept
      : addr_(addr), scope_id_(scope_id) {}

  address_v6(const address_v6 &) = default;
  address_v6(address_v6 &&) = default;
  address_v6 &operator=(const address_v6 &) = default;
  address_v6 &operator=(address_v6 &&) = default;

  static consteval address_v6 loopback() noexcept {
    return {{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1}};
  }
  static consteval address_v6 unspecified() noexcept {
    return {{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};
  }

public:
  constexpr auto to_bytes() const noexcept { return addr_; }
  constexpr auto to_bytes_view() const noexcept -> bytes_view_type {
    return addr_;
  }
  constexpr auto native_handle() const noexcept {
    details::in6_addr_t in6_addr;
    // byte array so we don't have to worry about endianness
    std::ranges::copy_n(std::bit_cast<underlying_bytes_type>(addr_).begin(),
                        std::ranges::size(in6_addr.s6_addr),
                        in6_addr.s6_addr);
    return in6_addr;
  }
  static consteval auto family() noexcept { return family::v6; }
  constexpr auto scope_id() const noexcept { return scope_id_; }
  auto to_string(const FormatPolicy policy) const -> string_type {
    if (policy == FormatPolicy::kBrief)
      return Format("{}, {}",
                    std::bit_cast<std::array<uint64_t, 2ULL>>(addr_),
                    scope_id_);
    else if (policy == FormatPolicy::kDefault)
      return Format("{:x}:{:x}:{:x}:{:x}:{:x}:{:x}:{:x}:{:x}%{}",
                    (std::to_integer<uint16_t>(addr_[0]) << 8) |
                        std::to_integer<uint16_t>(addr_[1]),
                    (std::to_integer<uint16_t>(addr_[2]) << 8) |
                        std::to_integer<uint16_t>(addr_[3]),
                    (std::to_integer<uint16_t>(addr_[4]) << 8) |
                        std::to_integer<uint16_t>(addr_[5]),
                    (std::to_integer<uint16_t>(addr_[6]) << 8) |
                        std::to_integer<uint16_t>(addr_[7]),
                    (std::to_integer<uint16_t>(addr_[8]) << 8) |
                        std::to_integer<uint16_t>(addr_[9]),
                    (std::to_integer<uint16_t>(addr_[10]) << 8) |
                        std::to_integer<uint16_t>(addr_[11]),
                    (std::to_integer<uint16_t>(addr_[12]) << 8) |
                        std::to_integer<uint16_t>(addr_[13]),
                    (std::to_integer<uint16_t>(addr_[14]) << 8) |
                        std::to_integer<uint16_t>(addr_[15]),
                    scope_id_);
    else
      return Format(
          "{}, {}", std::bit_cast<underlying_bytes_type>(addr_), scope_id_);
  }
  constexpr explicit(false) operator details::in6_addr_t() const noexcept {
    return native_handle();
  }
  constexpr explicit operator address() const noexcept;
};
static_assert(std::is_trivially_destructible_v<address_v6>);
class address : Printable {
  union {
    address_v4 v4_;
    address_v6 v6_;
  };
  enum : uint8_t { ipv4, ipv6 } type_;

public:
  constexpr address() noexcept {}
  constexpr address(const address_v4 &v4) noexcept : v4_(v4), type_(ipv4) {}
  constexpr address(const address_v6 &v6) noexcept : v6_(v6), type_(ipv6) {}
  address(const address &) = default;
  address(address &&) = default;
  address &operator=(const address &) = default;
  address &operator=(address &&) = default;
  template <family family = family::v4>
  static consteval address unspecified() noexcept {
    if constexpr (family == family::v4)
      return address_v4::unspecified();
    else
      return address_v6::unspecified();
  }

public:
  constexpr auto is_ipv4() const noexcept { return type_ == ipv4; }
  constexpr auto is_ipv6() const noexcept { return type_ == ipv6; }
  constexpr auto to_v4() const {
    AC_RUNTIME_ASSERT(is_ipv4(), "bad variant access")
    return v4_;
  }
  constexpr auto to_v6() const {
    AC_RUNTIME_ASSERT(is_ipv6(), "bad variant access")
    return v6_;
  }
  template <typename = void>
  [[clang::no_specializations]]
  constexpr auto native_handle() const noexcept {
    if (is_ipv4())
      return v4_.native_handle();
    else
      return v6_.native_handle();
  }
  auto to_string(const FormatPolicy policy) const -> string_type {
    if (is_ipv4())
      return v4_.to_string(policy);
    else
      return v6_.to_string(policy);
  }
  constexpr auto bytes_view() const noexcept -> std::span<const std::byte> {
    if (is_ipv4())
      return v4_.addr_;
    else
      return v6_.addr_;
  }
  constexpr auto scope_id() const noexcept
      -> std::optional<address_v6::scope_id_type> {
    if (is_ipv6())
      return std::make_optional(v6_.scope_id());
    else
      return std::nullopt;
  }
  constexpr auto family() const noexcept {
    if (is_ipv4())
      return family::v4;
    else
      return family::v6;
  }
};
inline constexpr address_v4::operator address() const noexcept {
  return {*this};
}
inline constexpr address_v6::operator address() const noexcept {
  return {*this};
}
} // namespace auxilia::net::ip
