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
#include "auxilia/networking/ip.hpp"
#include "auxilia/networking/net.hpp"
#include "auxilia/utility/program_options.hpp"
#include "auxilia/utility/numbers.hpp"

using namespace auxilia;
namespace net = auxilia::net;

struct options {
  bool server = false;
  net::protocol protocol = net::protocol::udp;
  net::ip::address_v4 host = net::ip::address_v4::loopback();
  uint16_t port = 65432;
  size_t workers = 4;
  std::string name = "client";
};

static std::optional<options> parse_args(const int argc, const char **argv) {
  auto parser = program_options::Global("chatroom", "0.0.1");
  parser->add_option("--protocol", "", "protocol type")
      .nargs(1)
      .default_value("udp");
  parser->add_option("--host", "", "server host")
      .nargs(1)
      .default_value("127.0.0.1");
  parser->add_option("--port", "", "server port")
      .nargs(1)
      .default_value("65432");
  parser->add_option("--workers", "", "number of worker threads")
      .nargs(1)
      .default_value("4");
  parser->add_option("--name", "", "client name")
      .nargs(1)
      .default_value("client");
  parser->add_option("--server", "", "run in server mode").nargs(0);
  parser->add_option("--client", "", "run in client mode").nargs(0);

  options opts{};

  if (!parser->parse(argc, argv)) {
    Println(parser->error_messages());
    return std::nullopt;
  }
  if (auto host = net::ip::address_v4::from_str(
          *parser->get_option("--host")->value())) {
    opts.host = *host;
  } else {
    std::move(host).log();
  }

  auto port = parser->get_option("--port");
  if (const auto num = to_integer<unsigned short>(*port->value()))
    opts.port = *num;
  else
    Println("Invalid port number '{0}': {1}",
            *port->value(),
            std::make_error_code(num.error()).message());

  auto workers = parser->get_option("--workers");
  if (const auto num = to_integer<size_t>(*workers->value()))
    opts.workers = *num;
  else
    Println("Invalid worker count '{0}': {1}",
            *workers->value(),
            std::make_error_code(num.error()).message());

  auto protocol = *parser->get_option("--protocol")->value();
  net::protocol pk;

  if (protocol == "tcp")
    pk = net::protocol::tcp;
  else if (protocol == "udp")
    pk = net::protocol::udp;
  else {
    Println("Invalid protocol '{0}': must be 'udp' or 'tcp'", protocol);
    return std::nullopt;
  }
  opts.protocol = pk;
  opts.name = *parser->get_option("--name")->value();

  auto is_server = parser->get_option("--server")->value();
  auto is_client = parser->get_option("--client")->value();
  if (is_server && is_client) {
    std::cout << "Cannot specify both --server and --client.\n";
    return std::nullopt;
  }
  if (!is_server && !is_client) {
    std::cout << "Must specify either --server or --client.\n";
    return std::nullopt;
  }
  opts.server = is_server.ok();
  return opts;
}

struct udp_server_state {
  net::socket<net::udp> socket;
  std::mutex mutex;
  std::vector<net::endpoint<net::udp>> clients;
  std::shared_ptr<spdlog::logger> logger;
};

static void start_server_receive(udp_server_state &state) {
  constexpr size_t kMaxDatagram = 1400;
  state.socket
      .async_recv_from(
          kMaxDatagram,
          [&state](StatusOr<net::socket<net::udp>::bytes_type> result,
                   net::endpoint<net::udp> sender) {
            if (!result) {
              result.log(state.logger);
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
                          send_res.log(logger);
                      })
                  .log(state.logger);
            }

            start_server_receive(state);
          })
      .log(state.logger);
}

struct udp_client_state {
  net::socket<net::udp> socket;
  net::endpoint<net::udp> server;
  std::string name;
  std::shared_ptr<spdlog::logger> logger;
};

static void start_client_receive(udp_client_state &state) {
  state.socket
      .async_recv_from(
          0x800,
          [&state](StatusOr<net::socket<net::udp>::bytes_type> result,
                   net::endpoint<net::udp> sender) {
            if (!result) {
              result.log(state.logger);
              start_client_receive(state);
              return;
            }
            state.logger->info("{}", *std::move(result));
            start_client_receive(state);
          })
      .log(state.logger);
}

struct tcp_session {
  net::socket<net::tcp> socket;
};

struct tcp_server_state {
  net::socket<net::tcp> listener;
  std::mutex mutex;
  std::vector<std::shared_ptr<tcp_session>> sessions;
  std::shared_ptr<spdlog::logger> logger;
};

static void remove_tcp_session(tcp_server_state &state,
                               const std::shared_ptr<tcp_session> &session) {
  session->socket.close().log(state.logger);
  std::scoped_lock lock(state.mutex);
  std::erase(state.sessions, session);
}

static void start_tcp_receive(tcp_server_state &state,
                              std::shared_ptr<tcp_session> session) {
  session->socket
      .async_recv(
          0x800,
          [&state,
           session](StatusOr<net::socket<net::tcp>::bytes_type> result) {
            if (!result) {
              result.log(state.logger);
              remove_tcp_session(state, session);
              return;
            }

            auto message = *std::move(result);
            if (message.empty()) {
              state.logger->info("tcp client disconnected");
              remove_tcp_session(state, session);
              return;
            }

            std::vector<std::shared_ptr<tcp_session>> peers;
            {
              std::scoped_lock lock(state.mutex);
              peers = state.sessions;
            }

            for (const auto &peer : peers) {
              if (peer == session)
                continue;
              peer->socket
                  .async_send(
                      net::socket<net::tcp>::bytes_type(message),
                      [logger = state.logger](StatusOr<size_t> send_res) {
                        if (!send_res)
                          send_res.log(logger);
                      })
                  .log(state.logger);
            }

            start_tcp_receive(state, session);
          })
      .log(state.logger);
}

struct tcp_client_state {
  net::socket<net::tcp> socket;
  std::string name;
  std::shared_ptr<spdlog::logger> logger;
};

static void start_tcp_client_receive(tcp_client_state &state) {
  state.socket
      .async_recv(0x800,
                  [&state](StatusOr<net::socket<net::tcp>::bytes_type> result) {
                    if (!result) {
                      result.log(state.logger);
                      return;
                    }

                    auto message = *std::move(result);
                    if (message.empty()) {
                      state.logger->info("server closed the connection");
                      return;
                    }
                    state.logger->info("{}", message);
                    start_tcp_client_receive(state);
                  })
      .log(state.logger);
}

static std::vector<std::jthread> start_workers(net::io_context &ctx,
                                               const size_t count) {
  std::vector<std::jthread> workers;
  workers.reserve(count);
  for (size_t i = 0; i < count; ++i)
    workers.emplace_back([&ctx] { ctx.run(); });
  return workers;
}

static int run_udp_server(net::io_context &ctx, const options &opts) {
  const auto host = opts.host;
  auto logger = spdlog::stdout_color_mt("udp_server");
  auto sock = net::socket<net::udp>(ctx, net::ip::family::v4);
  auto bind_status = sock.bind(net::endpoint<net::udp>(host, opts.port));
  if (!bind_status) {
    bind_status.log(logger);
    return 1;
  }

  udp_server_state state{std::move(sock), {}, {}, logger};
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

static int run_udp_client(net::io_context &ctx, const options &opts) {
  const auto host = opts.host;
  auto logger = spdlog::stdout_color_mt("udp_client");
  auto sock = net::socket<net::udp>(ctx, net::ip::family::v4);
  sock.bind(0).log(logger);

  udp_client_state state{std::move(sock),
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
                         send_res.log(logger);
                     })
      .log(logger);

  logger->info("client ready; target {}:{}", opts.host, opts.port);
  logger->info("type /quit to exit");
  std::string line;
  while (std::getline(std::cin, line)) {
    if (line == "/quit")
      break;
    auto payload = state.name.empty() ? line : ("[" + state.name + "] " + line);
    state.socket
        .async_send_to(
            std::move(payload),
            state.server,
            [logger](StatusOr<size_t> send_res) { send_res.log(logger); })
        .log(logger);
  }

  ctx.stop(opts.workers);
  return 0;
}

static int run_tcp_server(net::io_context &ctx, const options &opts) {
  const auto host = opts.host;
  auto logger = spdlog::stdout_color_mt("tcp_server");
  auto listener = net::socket<net::tcp>(ctx, net::ip::family::v4);

  if (auto status = listener.bind(net::endpoint<net::tcp>(host, opts.port));
      !status) {
    status.log(logger);
    return 1;
  }
  if (auto status = listener.listen(); !status) {
    status.log(logger);
    return 1;
  }

  tcp_server_state state{std::move(listener), {}, {}, logger};
  logger->info("tcp server listening on {}:{}", opts.host, opts.port);
  [[maybe_unused]] auto workers = start_workers(ctx, opts.workers);

  std::jthread accept_thread([&state](std::stop_token st) {
    while (!st.stop_requested()) {
      auto acc = state.listener.accept();
      if (!acc) {
        if (st.stop_requested())
          break;
        acc.log(state.logger);
        continue;
      }

      auto session =
          std::make_shared<tcp_session>(tcp_session{*std::move(acc)});
      {
        std::scoped_lock lock(state.mutex);
        state.sessions.push_back(session);
      }

      if (auto remote = session->socket.remote_endpoint())
        state.logger->info("tcp client connected {}", *remote);
      else
        state.logger->info("tcp client connected");

      start_tcp_receive(state, session);
    }
  });

  logger->info("type /quit to exit");
  std::string line;
  while (std::getline(std::cin, line)) {
    if (line == "/quit")
      break;
  }

  state.listener.close().log(state.logger);
  accept_thread.request_stop();
  ctx.stop(opts.workers);
  return 0;
}

static int run_tcp_client(net::io_context &ctx, const options &opts) {
  const auto host = opts.host;
  auto logger = spdlog::stdout_color_mt("tcp_client");
  auto sock = net::socket<net::tcp>(ctx, net::ip::family::v4);

  if (auto status = sock.connect(net::endpoint<net::tcp>(host, opts.port));
      !status) {
    status.log(logger);
    return 1;
  }

  tcp_client_state state{std::move(sock), opts.name, logger};

  start_tcp_client_receive(state);
  [[maybe_unused]] auto workers = start_workers(ctx, opts.workers);

  logger->info("tcp connected to {}:{}", opts.host, opts.port);
  logger->info("type /quit to exit");
  std::string line;
  while (std::getline(std::cin, line)) {
    if (line == "/quit")
      break;
    auto payload = state.name.empty() ? line : ("[" + state.name + "] " + line);
    state.socket
        .async_send(std::move(payload),
                    [logger](StatusOr<size_t> send_res) {
                      if (!send_res)
                        send_res.log(logger);
                    })
        .log(logger);
  }

  ctx.stop(opts.workers);
  return 0;
}
void loginit() {
  auxilia::set_console_output_cp_utf8();
  spdlog::set_level(spdlog::level::info);
  spdlog::info("\033[33mlog framework initialized.\033[0m");
  spdlog::set_pattern("[%n: %^%l%$] %v");
  spdlog::default_logger()->set_pattern("[%^%l%$] %v");
}
int main(int argc, const char **argv) {

  loginit();

  const auto opts = parse_args(argc, argv);
  if (!opts)
    return 1;

  net::io_context ctx;

  auto base_logger = spdlog::stdout_color_mt("chat");

  if (auto status = ctx.initialize(); !status) {
    status.log(base_logger);
    return 1;
  }

  if (opts->protocol == net::protocol::udp) {
    return opts->server ? run_udp_server(ctx, *opts)
                        : run_udp_client(ctx, *opts);
  } else {
    return opts->server ? run_tcp_server(ctx, *opts)
                        : run_tcp_client(ctx, *opts);
  }
}
