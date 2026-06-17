#pragma once

#include "os.hpp"
#include "ip.hpp"

namespace auxilia::net {
template <typename> class endpoint;
template <typename> class socket;
template <typename> class acceptor;
enum class protocol : std::underlying_type_t<socket_kind> {
  tcp = std::to_underlying(socket_kind::stream),
  udp = std::to_underlying(socket_kind::datagram)
};
inline const char *to_string(const protocol protocol) {
  switch (protocol) {
  case protocol::tcp:
    return "tcp";
  case protocol::udp:
    return "udp";
  default:
    return "unknown";
  }
};
} // namespace auxilia::net
namespace auxilia {
template <>
inline std::optional<net::protocol>
from_string(const std::string_view str) noexcept {
  if (str == "tcp")
    return net::protocol::tcp;
  if (str == "udp")
    return net::protocol::udp;
  return std::nullopt;
}
} // namespace auxilia
namespace auxilia::net {

template <typename Protocol>
concept InternetProtocol = requires {
  { Protocol::v4() } -> std::same_as<Protocol>;
  { Protocol::v6() } -> std::same_as<Protocol>;
  { Protocol::socket_kind() } -> std::same_as<socket_kind>;
};
struct tcp {
  static consteval auto v4() { return tcp(ip::family::v4); }
  static consteval auto v6() { return tcp(ip::family::v6); }
  static consteval auto socket_kind() { return socket_kind::stream; }

  using endpoint = endpoint<tcp>;
  using socket = socket<tcp>;
  using acceptor = acceptor<tcp>;

private:
  consteval tcp(const ip::family family) : family_(family) {}
  const ip::family family_;
};

struct udp {
  static consteval auto v4() { return udp(ip::family::v4); }
  static consteval auto v6() { return udp(ip::family::v6); }
  static consteval auto socket_kind() { return socket_kind::datagram; }

  using endpoint = endpoint<udp>;
  using socket = socket<udp>;

private:
  consteval udp(const ip::family family) : family_(family) {}
  const ip::family family_;
};
} // namespace auxilia::net
