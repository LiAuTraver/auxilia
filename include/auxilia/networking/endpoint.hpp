#pragma once

#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <ranges>
#include <type_traits>
#include <utility>

#include "auxilia/base/format.hpp"

#include "os.hpp"
#include "ip.hpp"

#include "protocol.hpp"

namespace auxilia::net {
template <typename> class socket;
template <typename Protocol> class endpoint : Printable {
  static_assert(InternetProtocol<Protocol>);
  friend consteval auto check_offset() noexcept;
  friend class socket<Protocol>;
  union {
    details::sockaddr_t base_;
    details::sockaddr_in4_t v4_;
    details::sockaddr_in6_t v6_;
  };

public:
  using port_type = details::port_type;
  using protocol_type = Protocol;

public:
  constexpr endpoint() noexcept {}
  constexpr endpoint(const ip::address_v4 &v4, const port_type port) noexcept {
    v4_.sin_family = std::to_underlying(ip::family::v4);
    v4_.sin_port = details::host2net(port);
    v4_.sin_addr = v4; // endianess handled inside the conversion
    // should set paddings to zero
    std::ranges::fill(v4_.sin_zero, 0);
  }
  constexpr endpoint(const ip::address_v6 &v6, const port_type port) noexcept {
    v6_.sin6_family = std::to_underlying(ip::family::v6);
    v6_.sin6_port = details::host2net(port);
    v6_.sin6_addr = v6;
    v6_.sin6_scope_id = v6.scope_id();
  }
  constexpr endpoint(const ip::address &ip, const port_type port) noexcept {
    if (ip.is_ipv4())
      *this = endpoint(ip.to_v4(), port);
    else
      *this = endpoint(ip.to_v6(), port);
  }
  constexpr endpoint(details::sockaddr_storage_t &&storage,
                     const details::socket_len_type len) noexcept {
    switch (static_cast<size_t>(len)) {
    case sizeof(details::sockaddr_in4_t):
      v4_ = reinterpret_cast<details::sockaddr_in4_t &&>(storage);
      break;
    case sizeof(details::sockaddr_in6_t):
      v6_ = reinterpret_cast<details::sockaddr_in6_t &&>(storage);
      break;
    default:
      AC_DEBUG_BREAK
    }
  }
  constexpr StatusOr<endpoint>
  from_native(details::sockaddr_storage_t &&storage,
              const details::socket_len_type len) noexcept {
    if (len != sizeof(details::sockaddr_in4_t) &&
        len != sizeof(details::sockaddr_in6_t))
      return UnavailableError("Unknown address family of the accepted socket.");
    else
      return endpoint(std::move(storage), len);
  }
  template <ip::family family = ip::family::v4>
  static constexpr auto unspecified(const port_type port = 0) noexcept {
    if constexpr (family == ip::family::v4)
      return endpoint(ip::address_v4::unspecified(), port);
    else
      return endpoint(ip::address_v6::unspecified(), port);
  }
  auto to_string(const FormatPolicy policy) const {
    if (is_v4())
      return Format("{}:{}", address().to_string(policy), port());
    else
      return Format("[{}]:{}", address().to_string(policy), port());
  }

public:
  constexpr auto family() const noexcept {
    return static_cast<ip::family>(base_.sa_family);
  }
  constexpr auto protocol() const noexcept {
    if (is_v4())
      return protocol_type::v4();
    else
      return protocol_type::v6();
  }
  constexpr auto is_v4() const noexcept { return family() == ip::family::v4; }
  constexpr auto is_v6() const noexcept { return family() == ip::family::v6; }
  constexpr decltype(auto) data(this auto &&self) noexcept {
    return &self.base_;
  }
  constexpr auto size() const noexcept {
    if (is_v4())
      return sizeof(v4_);
    else
      return sizeof(v6_);
  }
  constexpr ip::address address() const noexcept {
    if (is_v4())
      return {ip::address_v4(
          std::bit_cast<ip::address_v4::bytes_type>(v4_.sin_addr.s_addr))};
    else
      return {ip::address_v6(
          std::bit_cast<ip::address_v6::bytes_type>(v6_.sin6_addr.s6_addr),
          v6_.sin6_scope_id)};
  }
  constexpr port_type port() const noexcept {
    if (is_v4())
      return details::net2host(v4_.sin_port);
    else
      return details::net2host(v6_.sin6_port);
  }

public:
  constexpr auto operator==(const endpoint &that) const noexcept {
    if (family() != that.family())
      return false;
    if (is_v4()) {
      return v4_.sin_port == that.v4_.sin_port &&
             v4_.sin_addr.s_addr == that.v4_.sin_addr.s_addr;
    }
    return v6_.sin6_port == that.v6_.sin6_port &&
           v6_.sin6_scope_id == that.v6_.sin6_scope_id &&
           ::memcmp(v6_.sin6_addr.s6_addr,
                    that.v6_.sin6_addr.s6_addr,
                    sizeof(v6_.sin6_addr.s6_addr)) == 0;
  }
};
static_assert(std::is_trivially_destructible_v<endpoint<tcp>>);
consteval auto check_offset() noexcept {
  static_assert(offsetof(endpoint<tcp>, base_) == 0);
}
} // namespace auxilia::net
