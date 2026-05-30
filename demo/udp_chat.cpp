#include <algorithm>
#include <array>
#include <cstddef>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <limits>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <thread>
#include <utility>
#include <vector>

#include <spdlog/logger.h>
#include <spdlog/sinks/stdout_color_sinks.h>

#include "auxilia/defines.hpp"
#include "auxilia/networking/net.hpp"

using namespace auxilia;
namespace net = auxilia::net;

struct options {
  bool server = false;
  std::string host = "127.0.0.1";
  uint16_t port = 65432;
  size_t workers = 4;
  std::string name = "client";
};

static void print_usage() {
  std::cout << "Usage:\n"
            << "  udp_chat server [--host 127.0.0.1] [--port 65432]"
               " [--workers 4]\n"
            << "  udp_chat client [--host 127.0.0.1] [--port 65432]"
               " [--workers 4] [--name NAME]\n";
}

static bool parse_u16(const std::string_view text, uint16_t &out) {
  if (text.empty())
    return false;
  auto copy = std::string(text);
  char *end = nullptr;
  const auto value = std::strtoul(copy.c_str(), &end, 10);
  if (!end || *end != '\0')
    return false;
  if (value > std::numeric_limits<uint16_t>::max())
    return false;
  out = static_cast<uint16_t>(value);
  return true;
}

static bool parse_size(const std::string_view text, size_t &out) {
  if (text.empty())
    return false;
  auto copy = std::string(text);
  char *end = nullptr;
  const auto value = std::strtoul(copy.c_str(), &end, 10);
  if (!end || *end != '\0')
    return false;
  if (value == 0)
    return false;
  out = static_cast<size_t>(value);
  return true;
}

static options parse_args(const int argc, char **argv, bool &ok) {
  ok = false;
  options opts{};
  if (argc < 2) {
    print_usage();
    return opts;
  }

  const auto mode = std::string_view(argv[1]);
  if (mode == "server")
    opts.server = true;
  else if (mode == "client")
    opts.server = false;
  else {
    print_usage();
    return opts;
  }

  for (int i = 2; i < argc; ++i) {
    const auto arg = std::string_view(argv[i]);
    if (arg == "--host" && i + 1 < argc) {
      opts.host = argv[++i];
    } else if (arg == "--port" && i + 1 < argc) {
      if (!parse_u16(argv[++i], opts.port)) {
        std::cout << "Invalid port.\n";
        return opts;
      }
    } else if (arg == "--workers" && i + 1 < argc) {
      if (!parse_size(argv[++i], opts.workers)) {
        std::cout << "Invalid worker count.\n";
        return opts;
      }
    } else if (arg == "--name" && i + 1 < argc) {
      opts.name = argv[++i];
    } else if (arg == "--help" || arg == "-h") {
      print_usage();
      return opts;
    } else {
      std::cout << "Unknown argument: " << arg << "\n";
      print_usage();
      return opts;
    }
  }

  ok = true;
  return opts;
}

struct server_state {
  net::socket<net::udp> socket;
  std::mutex mutex;
  std::vector<net::endpoint<net::udp>> clients;
  std::shared_ptr<spdlog::logger> logger;
};

static void start_server_receive(server_state &state) {
  constexpr size_t kMaxDatagram = 1400;
  state.socket
      .async_recv_from(
          kMaxDatagram,
          [&state](StatusOr<net::socket<net::udp>::bytes_type> result,
                   net::endpoint<net::udp> sender) {
            if (!result) {
              result.rvalue().log_err(state.logger);
              start_server_receive(state);
              return;
            }
            auto message = *std::move(result);
            state.logger->info("recv {} bytes from {}", message.size(), sender);

            std::vector<net::endpoint<net::udp>> peers;
            {
              std::scoped_lock lock(state.mutex);
              const auto it = std::ranges::find(state.clients, sender);
              if (it == state.clients.end())
                state.clients.push_back(sender);
              peers = state.clients;
            }

            for (const auto &peer : peers) {
              if (peer == sender)
                continue;
              state.socket
                  .async_send_to(
                      message,
                      peer,
                      [logger = state.logger](StatusOr<size_t> send_res) {
                        if (!send_res)
                          send_res.rvalue().log_err(logger);
                      })
                  .log_err(state.logger);
            }

            start_server_receive(state);
          })
      .log_err(state.logger);
}

struct client_state {
  net::socket<net::udp> socket;
  net::endpoint<net::udp> server;
  std::string name;
  std::shared_ptr<spdlog::logger> logger;
};

static void start_client_receive(client_state &state) {
  state.socket
      .async_recv_from(
          0x800,
          [&state](StatusOr<net::socket<net::udp>::bytes_type> result,
                   net::endpoint<net::udp> sender) {
            if (!result) {
              result.rvalue().log_err(state.logger);
              start_client_receive(state);
              return;
            }
            state.logger->info("{}", *std::move(result));
            start_client_receive(state);
          })
      .log_err(state.logger);
}

static std::vector<std::jthread> start_workers(net::io_context &ctx,
                                               const size_t count) {
  std::vector<std::jthread> workers;
  workers.reserve(count);
  for (size_t i = 0; i < count; ++i)
    workers.emplace_back([&ctx] { ctx.run(); });
  return workers;
}

static int run_server(net::io_context &ctx,
                      const options &opts,
                      const net::ip::address_v4 host) {
  auto logger = spdlog::stdout_color_mt("udp_server");
  auto sock = net::socket<net::udp>(ctx, net::ip::family::v4);
  auto bind_status = sock.bind(net::endpoint<net::udp>(host, opts.port));
  if (!bind_status) {
    bind_status.rvalue().log_err(logger);
    return 1;
  }

  server_state state{std::move(sock), {}, {}, logger};
  logger->info("server listening on {}:{}", opts.host, opts.port);
  start_server_receive(state);
  [[maybe_unused]] auto workers = start_workers(ctx, opts.workers);

  logger->info("type /quit to exit");
  std::string line;
  while (std::getline(std::cin, line)) {
    if (line == "/quit")
      break;
  }

  ctx.stop(opts.workers);
  return 0;
}

static int run_client(net::io_context &ctx,
                      const options &opts,
                      const net::ip::address_v4 host) {
  auto logger = spdlog::stdout_color_mt("udp_client");
  auto sock = net::socket<net::udp>(ctx, net::ip::family::v4);
  sock.bind(0).log_err(logger);

  client_state state{std::move(sock),
                     net::endpoint<net::udp>(host, opts.port),
                     opts.name,
                     logger};

  start_client_receive(state);
  [[maybe_unused]] auto workers = start_workers(ctx, opts.workers);

  // send a message to server to indicate that it's in.
  state.socket
      .async_send_to("Hello, server!",
                     state.server,
                     [logger](StatusOr<size_t> send_res) {
                       if (!send_res)
                         send_res.rvalue().log_err(logger);
                     })
      .log_err(logger);

  logger->info("client ready; target {}:{}", opts.host, opts.port);
  logger->info("type /quit to exit");
  std::string line;
  while (std::getline(std::cin, line)) {
    if (line == "/quit")
      break;
    auto payload = state.name.empty() ? line : ("[" + state.name + "] " + line);
    state.socket
        .async_send_to(std::move(payload),
                       state.server,
                       [logger](StatusOr<size_t> send_res) {
                         send_res.rvalue().log_err(logger);
                       })
        .log_err(logger);
  }

  ctx.stop(opts.workers);
  return 0;
}
void loginit() {
  auxilia::set_console_output_cp_utf8();
  spdlog ::set_level(spdlog ::level ::info);
  spdlog ::set_pattern("[%^%l%$] %v");
  spdlog ::info("\033[33m"
                "log framework initialized"
                "."
                "\033[0m");
  spdlog::set_pattern("[%n: %^%l%$] %v");
}
int main(int argc, char **argv) {
  loginit();

  bool ok = false;
  const auto opts = parse_args(argc, argv, ok);
  if (!ok)
    return 1;

  const auto host = net::ip::address_v4::from_str(opts.host);
  if (!host) {
    std::cout << "Invalid host: " << opts.host << "\n";
    return 1;
  }

  net::io_context ctx;
  auto base_logger = spdlog::stdout_color_mt("udp_chat");
  if (auto status = ctx.initialize(); !status) {
    status.rvalue().log_err(base_logger);
    return 1;
  }

  const auto rc =
      opts.server ? run_server(ctx, opts, *host) : run_client(ctx, opts, *host);
  ctx.shutdown().log_err(base_logger);
  return rc;
}
