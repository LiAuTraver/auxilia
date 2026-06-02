#pragma once

#ifdef _WIN32

#  define WIN32_LEAN_AND_MEAN
#  define NOMINMAX
#  include <WinSock2.h>
#  include <ws2ipdef.h>
#  include <io.h>
#  include <ws2tcpip.h>
#  include <winerror.h>
#  pragma comment(lib, "Ws2_32.lib")
#elif defined(__linux__)
#  include <netinet/in.h>
#  include <sys/socket.h>
#  include <sys/types.h>
#  include <unistd.h>

#endif

#include <type_traits>
#include <algorithm>
#include <string>
#include <utility>

#include "auxilia/base/macros.hpp"
#include "auxilia/status/Status.hpp"

namespace auxilia::net::ip {
enum class family : std::conditional_t<
    std::is_enum_v<decltype(AF_INET)>,
    std::underlying_type<decltype(AF_INET)>,
    std::type_identity<decltype(AF_INET)>>::type {
  v4 = AF_INET,
  v6 = AF_INET6
};
inline constexpr const char *to_string(const family family) noexcept {
  switch (family) {
  case family::v4:
    return "v4";
  case family::v6:
    return "v6";
  default:
    return "unknown";
  }
}
} // namespace auxilia::net::ip
namespace auxilia {
template <>
inline constexpr std::optional<net::ip::family>
from_string(const std::string_view str) noexcept {
  if (str == "v4")
    return net::ip::family::v4;
  else if (str == "v6")
    return net::ip::family::v6;
  else
    return std::nullopt;
}
} // namespace auxilia
namespace auxilia::net {
enum class socket_kind : std::conditional_t<
    std::is_enum_v<decltype(SOCK_STREAM)>,
    std::underlying_type<decltype(SOCK_STREAM)>,
    std::type_identity<decltype(SOCK_STREAM)>>::type {
  stream = SOCK_STREAM,
  datagram = SOCK_DGRAM,
  raw = SOCK_RAW
};
inline constexpr const char *to_string(const socket_kind kind) noexcept {
  switch (kind) {
  case socket_kind::stream:
    return "stream";
  case socket_kind::datagram:
    return "datagram";
  case socket_kind::raw:
    return "raw";
  default:
    return "unknown";
  }
}
using port_type = unsigned short;

} // namespace auxilia::net
namespace auxilia {
template <>
inline constexpr std::optional<net::socket_kind>
from_string(const std::string_view str) noexcept {
  if (str == "stream")
    return net::socket_kind::stream;
  else if (str == "datagram")
    return net::socket_kind::datagram;
  else if (str == "raw")
    return net::socket_kind::raw;
  else
    return std::nullopt;
}
} // namespace auxilia
namespace auxilia::net::details {
/// `htons` and `htonl`, but in a *constexpr* way.
template <typename Unsigned>
static inline constexpr Unsigned host2net(const Unsigned host) noexcept {
  static_assert(std::is_unsigned_v<Unsigned>);
  if constexpr (std::endian::native == std::endian::little)
    return std::byteswap(host);
  else
    return host;
}
/// `ntohs` and `ntohl`, in a *constexpr* way.
template <typename Unsigned>
static inline constexpr Unsigned net2host(const Unsigned net) noexcept {
  static_assert(std::is_unsigned_v<Unsigned>);
  if constexpr (std::endian::native == std::endian::little)
    return std::byteswap(net);
  else
    return net;
}

[[gnu::cold]] static inline Status make_ctor_error();
[[gnu::cold]] static inline Status make_bind_error();
[[gnu::cold]] static inline Status make_listen_error();
[[gnu::cold]] static inline Status make_connect_error();
[[gnu::cold]] static inline Status make_send_error();
[[gnu::cold]] static inline Status make_close_error();
[[gnu::cold]] static inline Status make_accept_error();
[[gnu::cold]] static inline Status make_recv_error();
} // namespace auxilia::net::details

#ifdef _WIN32
#  include "os/windows/windows.hpp" // IWYU pragma: export
#elif defined(__linux__)
#  include "os/linux/linux.hpp" // IWYU pragma: export
#else
#  error unsupported
#endif
