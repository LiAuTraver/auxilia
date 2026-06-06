#include <thread>

#include <spdlog/logger.h>
#include <spdlog/sinks/stdout_color_sinks.h>

#include "auxilia/networking/net.hpp"

using namespace auxilia;

using namespace std::chrono_literals;

net::io_context ctx;
constexpr auto v4 = net::ip::address_v4::loopback();
constexpr auto tcp_ep = net::endpoint<net::tcp>(v4, 65432);
void tcp_client() {
  auto logger = spdlog::stdout_color_mt(__func__);
  std::this_thread::sleep_for(1s);
  auto s = net::socket<net::tcp>(ctx, net::ip::family::v4);
  s.connect(tcp_ep).log_err();
  s.send_bytes("HIIII!").log_err(logger);
  logger->info("sent!");
  std::this_thread::sleep_for(1s);

  if (auto maybe_bytes = s.recv())
    logger->info("recerived: {}", *maybe_bytes);
  else
    maybe_bytes.log_err(logger);

  logger->info("closing!");
}
void tcp_server() {
  auto logger = spdlog::stdout_color_mt(__func__);
  auto s = net::socket<net::tcp>(ctx, net::ip::family::v4);
  s.bind(65432).log_err(logger);
  s.listen().log_err(logger);
  logger->info("listening!");

  if (auto maybe_bytes =
          s.accept()
              .transform([&](auto &&ns) {
                logger->info(
                    "cached remote endpoint: {}",
                    ns.remote_endpoint().transform(Printable::Default));
                ns.send_bytes("connected. give me your message!").log_err();
                return ns;
              })
              .and_then<net::socket<net::tcp>::recv_>()) {
    logger->info("received: {}", *maybe_bytes);
  } else {
    maybe_bytes.log_err(logger);
  }
  logger->info("closing!");
}
constexpr auto udp_ep = net::endpoint<net::udp>(v4, 65432);
void udp_client() {
  auto logger = spdlog::stdout_color_mt(__func__);
  std::this_thread::sleep_for(1s);
  auto s = net::socket<net::udp>(ctx, net::ip::family::v4);
  s.send_bytes("HIIII", udp_ep).log_err(logger);
  logger->info("sent!");
  std::this_thread::sleep_for(1s);

  s.send_bytes("another msg").log_err(logger);
  logger->info("sent another one!");
}
void udp_server() {
  auto logger = spdlog::stdout_color_mt(__func__);
  auto s = net::socket<net::udp>(ctx, net::ip::family::v4);
  s.bind(65432).log_err(logger);
  logger->info("listening!");

  if (auto maybe_bytes = s.recv()) {
    logger->info("recerived: {}", *maybe_bytes);
  } else {
    maybe_bytes.log_err(logger);
  }
}
int main() {
  auxilia::set_console_output_cp_utf8();
  AC_DEBUG_LOGGING_SETUP("flight.exe", info, "log framework initialized");
  ::spdlog::set_pattern("[%n: %^%l%$] %v");
  ctx.initialize().log_err();
  auto _server = std::jthread(tcp_server);
  auto _client = std::jthread(tcp_client);
  // auto server_ = std::jthread(udp_server);
  // auto client_ = std::jthread(udp_client);
}
