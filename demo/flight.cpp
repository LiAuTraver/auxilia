#include <ostream>
#include <thread>

#include <spdlog/logger.h>
#include <spdlog/sinks/stdout_color_sinks.h>

#include "auxilia/networking/net.hpp"
#include "auxilia/networking/socket.hpp"

using namespace auxilia;
using namespace auxilia::net;
using namespace auxilia::net::ip;

io_context ctx;
constexpr auto v4 = address_v4::loopback();
constexpr auto tcp_ep = endpoint<tcp>(v4, 65432);
void tcp_client() {
  auto logger = spdlog::stdout_color_mt(__func__);
  std::this_thread::sleep_for(std::chrono::seconds(1));
  auto s = net::socket<tcp>(ctx, ip::family::v4);
  s.connect(tcp_ep).log_err();
  s.send_bytes("HIIII").log_err(logger);
  logger->info("sent!");
  std::this_thread::sleep_for(std::chrono::seconds(1));

  s.send_bytes("another msg").log_err(logger);
}
void tcp_server() {
  auto logger = spdlog::stdout_color_mt(__func__);
  auto s = net::socket<tcp>(ctx, ip::family::v4);
  s.bind(65432).log_err(logger);
  s.listen().log_err(logger);
  logger->info("listening!");
  auto maybe_bytes = s.accept()
                         .inspect([&](auto &&ns) {
                           logger->info("cached remote endpoint: {}",
                                        ns.remote_endpoint()
                                            .transform(Printable::Default)
                                            .value_or("none"));
                         })
                         .and_then<net::socket<tcp>::recv_>();
  if (maybe_bytes.ok()) {
    logger->info("recerived: {}", *maybe_bytes);
  } else {
    maybe_bytes.rvalue().log_err(logger);
  }
}
constexpr auto udp_ep = endpoint<udp>(v4, 65432);
void udp_client() {
  auto logger = spdlog::stdout_color_mt(__func__);
  std::this_thread::sleep_for(std::chrono::seconds(1));
  auto s = net::socket<udp>(ctx, ip::family::v4);
  s.send_bytes("HIIII", udp_ep).log_err(logger);
  logger->info("sent!");
  std::this_thread::sleep_for(std::chrono::seconds(1));

  s.send_bytes("another msg").log_err(logger);
  logger->info("sent!");
}
void udp_server() {
  auto logger = spdlog::stdout_color_mt(__func__);
  auto s = net::socket<udp>(ctx, ip::family::v4);
  s.bind(65432).log_err(logger);
  logger->info("listening!");

  auto maybe_bytes = s.recv();
  if (maybe_bytes.ok()) {
    logger->info("recerived: {}", *maybe_bytes);
  } else {
    maybe_bytes.rvalue().log_err(logger);
  }
}
int main() {
  AC_DEBUG_LOGGING_SETUP("flight.exe", info, "inited");
  ::spdlog::set_pattern("[%n: %^%l%$] %v");
  ctx.initialize().log_err();
  auto _server = std::jthread(tcp_server);
  auto _client = std::jthread(tcp_client);
  auto server_ = std::jthread(udp_server);
  auto client_ = std::jthread(udp_client);
}
