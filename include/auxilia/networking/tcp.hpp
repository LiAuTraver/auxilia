#pragma once

#include "os.hpp"
#include "ip.hpp"

namespace auxilia::net {
template <typename> class endpoint;
template <typename> class socket;
template <typename Protocol>
concept InternetProtocol = requires {
  { Protocol::v4() } -> std::same_as<Protocol>;
  { Protocol::v6() } -> std::same_as<Protocol>;
  { Protocol::socket_type() } -> std::same_as<socket_type>;
};
struct tcp {
  static consteval auto v4() { return tcp(ip::family::v4); }
  static consteval auto v6() { return tcp(ip::family::v6); }
  static consteval auto socket_type() { return socket_type::stream; };

  using endpoint_type = endpoint<tcp>;
  using socket_t = socket<tcp>;

private:
  consteval tcp(const ip::family family) : family_(family) {}
  const ip::family family_;
};

struct udp {
  static consteval auto v4() { return udp(ip::family::v4); }
  static consteval auto v6() { return udp(ip::family::v6); }
  static consteval auto socket_type() { return socket_type::datagram; };

  using endpoint_type = endpoint<udp>;
  using socket_t = socket<udp>;

private:
  consteval udp(const ip::family family) : family_(family) {}
  const ip::family family_;
};
} // namespace auxilia::net
