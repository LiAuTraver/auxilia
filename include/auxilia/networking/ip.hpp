#pragma once

#include <array>
#include <bit>
#include <span>
#include <type_traits>

#include "auxilia/base/config.hpp"
#include "auxilia/base/format.hpp"

#include "os.hpp"
namespace auxilia::net::ip {
class address_v4 : Printable {
  friend class address;

public:
  using bytes_type = std::array<std::byte, 4ULL>;
  using bytes_view_type = const std::span<std::byte, 4ULL>;
  using uint_type = uint32_t;

private:
  static_assert(sizeof(uint32_t) == 4 * sizeof(std::byte));
  static_assert(sizeof(uint32_t) ==
                sizeof(decltype(std::declval<details::in4_addr_t>().s_addr)));
  bytes_type addr;

  consteval address_v4(const unsigned char (&addr)[4]) noexcept {
    this->addr = std::bit_cast<bytes_type>(addr);
  }

public:
  constexpr address_v4() noexcept {}
  constexpr address_v4(const bytes_type addr) noexcept : addr(addr) {}

  address_v4(const address_v4 &) = default;
  address_v4(address_v4 &&) = default;
  address_v4 &operator=(const address_v4 &) = default;
  address_v4 &operator=(address_v4 &&) = default;

  static consteval address_v4 loopback() noexcept { return {{127, 0, 0, 1}}; }
  static consteval address_v4 broadcast() noexcept {
    return {{255, 255, 255, 255}};
  }

public:
  constexpr auto to_uint() const noexcept {
    return std::bit_cast<uint_type>(addr);
  }
  constexpr auto to_bytes() const noexcept { return addr; }
  auto to_string(FormatPolicy policy) const -> string_type {
    if (policy == FormatPolicy::kBrief)
      return Format("{}", to_uint());
    else if (policy == FormatPolicy::kDefault) {
      return Format("{}.{}.{}.{}",
                    std::to_integer<uint8_t>(addr[0]),
                    std::to_integer<uint8_t>(addr[1]),
                    std::to_integer<uint8_t>(addr[2]),
                    std::to_integer<uint8_t>(addr[3]));
    } else {
      return Format("{}", std::bit_cast<std::array<uint8_t, 4ULL>>(addr));
    }
  }
  constexpr explicit operator uint_type() const noexcept { return to_uint(); }
  constexpr explicit(false) operator details::in4_addr_t() const noexcept {
    details::in4_addr_t in4_addr;
    in4_addr.s_addr = to_uint();
    return in4_addr;
  }
  constexpr explicit operator address() const noexcept;
};
static_assert(std::is_trivially_destructible_v<address_v4>);
class address_v6 : Printable {
  friend class address;

public:
  using bytes_type = std::array<std::byte, 16ULL>;
  using bytes_view_type = const std::span<const std::byte, 16ULL>;

private:
  bytes_type addr;

  consteval address_v6(const unsigned char (&addr)[16]) noexcept {
    this->addr = std::bit_cast<bytes_type>(addr);
  }

public:
  constexpr address_v6() noexcept {}
  constexpr address_v6(const bytes_type &addr) noexcept : addr(addr) {}
  constexpr address_v6(bytes_type &&addr) noexcept : addr(std::move(addr)) {}

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
  constexpr auto to_bytes() const noexcept { return addr; }
  constexpr auto to_bytes_view() const noexcept -> bytes_view_type {
    return addr;
  }
  auto to_string(FormatPolicy policy) const -> string_type {
    if (policy == FormatPolicy::kBrief) {
      return Format("{}", std::bit_cast<std::array<uint64_t, 2ULL>>(addr));
    } else if (policy == FormatPolicy::kDefault) {
      return Format("{:x}:{:x}:{:x}:{:x}:{:x}:{:x}:{:x}:{:x}",
                    (std::to_integer<uint16_t>(addr[0]) << 8) |
                        std::to_integer<uint16_t>(addr[1]),
                    (std::to_integer<uint16_t>(addr[2]) << 8) |
                        std::to_integer<uint16_t>(addr[3]),
                    (std::to_integer<uint16_t>(addr[4]) << 8) |
                        std::to_integer<uint16_t>(addr[5]),
                    (std::to_integer<uint16_t>(addr[6]) << 8) |
                        std::to_integer<uint16_t>(addr[7]),
                    (std::to_integer<uint16_t>(addr[8]) << 8) |
                        std::to_integer<uint16_t>(addr[9]),
                    (std::to_integer<uint16_t>(addr[10]) << 8) |
                        std::to_integer<uint16_t>(addr[11]),
                    (std::to_integer<uint16_t>(addr[12]) << 8) |
                        std::to_integer<uint16_t>(addr[13]),
                    (std::to_integer<uint16_t>(addr[14]) << 8) |
                        std::to_integer<uint16_t>(addr[15]));
    } else {
      return Format("{}", std::bit_cast<std::array<uint8_t, 16ULL>>(addr));
    }
  }
  constexpr explicit operator address() const noexcept;
};
static_assert(std::is_trivially_destructible_v<address_v6>);
class address : Printable {
  union {
    address_v4 v4_{};
    address_v6 v6_;
  };
  enum : uint8_t { ipv4, ipv6 } type_{ipv4};

public:
  constexpr address() noexcept {}
  constexpr address(const address_v4 &v4) noexcept : v4_(v4), type_(ipv4) {}
  constexpr address(const address_v6 &v6) noexcept : v6_(v6), type_(ipv6) {}
  static constexpr address v4() noexcept { return address(address_v4()); }
  static constexpr address v6() noexcept { return address(address_v6()); }
  address(const address &) = default;
  address(address &&) = default;
  address &operator=(const address &) = default;
  address &operator=(address &&) = default;

public:
  constexpr auto is_ipv4() const noexcept { return type_ == ipv4; }
  constexpr auto is_ipv6() const noexcept { return type_ == ipv6; }
  auto to_string(FormatPolicy policy) const -> string_type {
    if (is_ipv4())
      return v4_.to_string(policy);
    else
      return v6_.to_string(policy);
  }
  constexpr auto bytes_view() const noexcept -> std::span<const std::byte> {
    if (is_ipv4())
      return v4_.addr;
    else
      return v6_.addr;
  }
};
inline constexpr address_v4::operator address() const noexcept {
  return address(*this);
}
inline constexpr address_v6::operator address() const noexcept {
  return address(*this);
}
} // namespace auxilia::net::ip
