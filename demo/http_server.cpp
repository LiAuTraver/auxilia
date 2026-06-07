#include <algorithm>
#include <atomic>
#include <cctype>
#include <cstddef>
#include <deque>
#include <iostream>
#include <limits>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <thread>
#include <unordered_map>
#include <utility>
#include <vector>

#include <spdlog/logger.h>
#include <spdlog/sinks/stdout_color_sinks.h>

#include "auxilia/defines.hpp"
#include "auxilia/net.hpp"
#include "auxilia/utility/numbers.hpp"
#include "auxilia/utility/program_options.hpp"

using namespace auxilia;
namespace net = auxilia::net;

struct options {
  net::ip::address_v4 host = net::ip::address_v4::loopback();
  uint16_t port = 8080;
  size_t workers = 4;
  size_t max_header_bytes = 8192;
  size_t max_body_bytes = 1024 * 1024;
  size_t read_chunk_bytes = 4096;
};

static std::optional<options> parse_args(const int argc, char **argv) {
  auto parser = program_options::Global("http_server_demo", "0.1.0");
  parser->add_option("--host", "", "server host")
      .nargs(1)
      .default_value("127.0.0.1");
  parser->add_option("--port", "", "server port")
      .nargs(1)
      .default_value("8080");
  parser->add_option("--workers", "", "number of worker threads")
      .nargs(1)
      .default_value("4");
  parser->add_option("--max-header", "", "max header bytes")
      .nargs(1)
      .default_value("8192");
  parser->add_option("--max-body", "", "max body bytes")
      .nargs(1)
      .default_value("1048576");
  parser->add_option("--read-chunk", "", "read chunk bytes")
      .nargs(1)
      .default_value("4096");

  options opts{};

  if (!parser->parse(argc, argv)) {
    Println(parser->error_messages());
    return std::nullopt;
  }

  if (auto host = net::ip::address_v4::from_str(
          *parser->get_option("--host")->value())) {
    opts.host = *host;
  } else {
    std::move(host).log_err();
  }

  if (const auto num =
          to_integer<unsigned short>(*parser->get_option("--port")->value())) {
    opts.port = *num;
  } else {
    Println("Invalid port number '{0}'",
            *parser->get_option("--port")->value());
    return std::nullopt;
  }

  if (const auto num =
          to_integer<size_t>(*parser->get_option("--workers")->value())) {
    opts.workers = *num;
  } else {
    Println("Invalid worker count '{0}'",
            *parser->get_option("--workers")->value());
    return std::nullopt;
  }

  if (const auto num =
          to_integer<size_t>(*parser->get_option("--max-header")->value())) {
    opts.max_header_bytes = *num;
  } else {
    Println("Invalid max header '{0}'",
            *parser->get_option("--max-header")->value());
    return std::nullopt;
  }

  if (const auto num =
          to_integer<size_t>(*parser->get_option("--max-body")->value())) {
    opts.max_body_bytes = *num;
  } else {
    Println("Invalid max body '{0}'",
            *parser->get_option("--max-body")->value());
    return std::nullopt;
  }

  if (const auto num =
          to_integer<size_t>(*parser->get_option("--read-chunk")->value())) {
    opts.read_chunk_bytes = *num;
  } else {
    Println("Invalid read chunk '{0}'",
            *parser->get_option("--read-chunk")->value());
    return std::nullopt;
  }

  return opts;
}

static std::vector<std::jthread> start_workers(net::io_context &ctx,
                                               const size_t count) {
  std::vector<std::jthread> workers;
  workers.reserve(count);
  for (size_t i = 0; i < count; ++i)
    workers.emplace_back([&ctx] { ctx.run(); });
  return workers;
}

static std::string to_lower_ascii(std::string_view input) {
  std::string out;
  out.reserve(input.size());
  for (const char ch : input) {
    const auto uch = static_cast<unsigned char>(ch);
    out.push_back(static_cast<char>(std::tolower(uch)));
  }
  return out;
}

static std::string_view trim_ascii(std::string_view input) {
  size_t start = 0;
  size_t end = input.size();
  while (start < end && (input[start] == ' ' || input[start] == '\t'))
    ++start;
  while (end > start && (input[end - 1] == ' ' || input[end - 1] == '\t'))
    --end;
  return input.substr(start, end - start);
}

static bool contains_token(std::string_view value, std::string_view token) {
  auto lower = to_lower_ascii(value);
  std::string_view view = lower;
  size_t pos = 0;
  while (pos < view.size()) {
    auto next = view.find(',', pos);
    if (next == std::string_view::npos)
      next = view.size();
    auto part = trim_ascii(view.substr(pos, next - pos));
    if (part == token)
      return true;
    pos = next + 1;
  }
  return false;
}

static bool parse_size(std::string_view text, size_t &out) {
  if (text.empty())
    return false;
  size_t value = 0;
  for (char ch : text) {
    if (ch < '0' || ch > '9')
      return false;
    const auto digit = static_cast<size_t>(ch - '0');
    if (value > (std::numeric_limits<size_t>::max() - digit) / 10)
      return false;
    value = value * 10 + digit;
  }
  out = value;
  return true;
}

struct http_request {
  std::string method;
  std::string target;
  std::string version;
  std::unordered_map<std::string, std::string> headers;
  std::string body;
  bool keep_alive = true;
};

struct http_response {
  int status = 200;
  std::string reason = "OK";
  std::string version = "HTTP/1.1";
  std::vector<std::pair<std::string, std::string>> headers;
  std::string body;
  bool close = false;

  std::string serialize(const bool include_body) const {
    std::string out;
    out.reserve(128 + body.size());
    out.append(version)
        .append(" ")
        .append(std::to_string(status))
        .append(" ")
        .append(reason)
        .append("\r\n");

    out.append("Server: auxilia-http-demo\r\n");
    out.append("Content-Length: ")
        .append(std::to_string(body.size()))
        .append("\r\n");
    out.append("Connection: ")
        .append(close ? "close" : "keep-alive")
        .append("\r\n");

    for (const auto &header : headers) {
      out.append(header.first)
          .append(": ")
          .append(header.second)
          .append("\r\n");
    }

    out.append("\r\n");
    if (include_body)
      out.append(body);
    return out;
  }
};

static std::string reason_phrase(const int status) {
  switch (status) {
  case 200:
    return "OK";
  case 400:
    return "Bad Request";
  case 404:
    return "Not Found";
  case 405:
    return "Method Not Allowed";
  case 411:
    return "Length Required";
  case 413:
    return "Payload Too Large";
  case 431:
    return "Request Header Fields Too Large";
  case 500:
    return "Internal Server Error";
  case 501:
    return "Not Implemented";
  case 505:
    return "HTTP Version Not Supported";
  default:
    return "Error";
  }
}

static http_response make_error_response(const int status,
                                         const std::string &detail) {
  http_response resp;
  resp.status = status;
  resp.reason = reason_phrase(status);
  resp.headers.emplace_back("Content-Type", "text/plain; charset=utf-8");
  if (detail.empty())
    resp.body = resp.reason + "\n";
  else
    resp.body = resp.reason + ": " + detail + "\n";
  return resp;
}

struct parse_limits {
  size_t max_header_bytes = 8192;
  size_t max_body_bytes = 1024 * 1024;
};

enum class parse_state { ok, need_more, error };

struct parse_result {
  parse_state state = parse_state::need_more;
  size_t consumed = 0;
  http_request request{};
  int error_status = 0;
  std::string error_message;
};

static bool parse_request_line(std::string_view line,
                               std::string &method,
                               std::string &target,
                               std::string &version) {
  auto sp1 = line.find(' ');
  if (sp1 == std::string_view::npos)
    return false;
  auto sp2 = line.find(' ', sp1 + 1);
  if (sp2 == std::string_view::npos)
    return false;
  auto method_sv = trim_ascii(line.substr(0, sp1));
  auto target_sv = trim_ascii(line.substr(sp1 + 1, sp2 - sp1 - 1));
  auto version_sv = trim_ascii(line.substr(sp2 + 1));
  if (method_sv.empty() || target_sv.empty() || version_sv.empty())
    return false;
  if (version_sv.find(' ') != std::string_view::npos)
    return false;
  method.assign(method_sv.data(), method_sv.size());
  target.assign(target_sv.data(), target_sv.size());
  version.assign(version_sv.data(), version_sv.size());
  return true;
}

static bool request_keep_alive(const http_request &request) {
  bool keep_alive = request.version == "HTTP/1.1";
  if (auto it = request.headers.find("connection");
      it != request.headers.end()) {
    if (contains_token(it->second, "close"))
      keep_alive = false;
    else if (contains_token(it->second, "keep-alive"))
      keep_alive = true;
  }
  return keep_alive;
}

static parse_result parse_request(std::string_view buffer,
                                  const parse_limits &limits) {
  parse_result out{};
  auto header_end = buffer.find("\r\n\r\n");
  if (header_end == std::string_view::npos) {
    if (buffer.size() > limits.max_header_bytes) {
      out.state = parse_state::error;
      out.error_status = 431;
      out.error_message = "header size limit exceeded";
    }
    return out;
  }

  if (header_end > limits.max_header_bytes) {
    out.state = parse_state::error;
    out.error_status = 431;
    out.error_message = "header size limit exceeded";
    return out;
  }

  auto header_block = buffer.substr(0, header_end);
  auto line_end = header_block.find("\r\n");
  if (line_end == std::string_view::npos) {
    out.state = parse_state::error;
    out.error_status = 400;
    out.error_message = "missing request line";
    return out;
  }

  auto request_line = header_block.substr(0, line_end);
  if (!parse_request_line(request_line,
                          out.request.method,
                          out.request.target,
                          out.request.version)) {
    out.state = parse_state::error;
    out.error_status = 400;
    out.error_message = "invalid request line";
    return out;
  }

  if (out.request.version != "HTTP/1.1" && out.request.version != "HTTP/1.0") {
    out.state = parse_state::error;
    out.error_status = 505;
    out.error_message = "unsupported HTTP version";
    return out;
  }

  auto headers_block = header_block.substr(line_end + 2);
  size_t pos = 0;
  while (pos < headers_block.size()) {
    auto next = headers_block.find("\r\n", pos);
    if (next == std::string_view::npos)
      next = headers_block.size();
    auto line = headers_block.substr(pos, next - pos);
    if (!line.empty()) {
      auto colon = line.find(':');
      if (colon == std::string_view::npos) {
        out.state = parse_state::error;
        out.error_status = 400;
        out.error_message = "invalid header line";
        return out;
      }
      auto name = trim_ascii(line.substr(0, colon));
      auto value = trim_ascii(line.substr(colon + 1));
      if (name.empty()) {
        out.state = parse_state::error;
        out.error_status = 400;
        out.error_message = "invalid header name";
        return out;
      }
      auto key = to_lower_ascii(name);
      auto existing = out.request.headers.find(key);
      if (existing == out.request.headers.end())
        out.request.headers.emplace(std::move(key),
                                    std::string(value.data(), value.size()));
      else
        existing->second.append(", ").append(value.data(), value.size());
    }
    pos = next + 2;
  }

  if (auto it = out.request.headers.find("transfer-encoding");
      it != out.request.headers.end()) {
    auto value = to_lower_ascii(it->second);
    if (!value.empty() && value != "identity") {
      out.state = parse_state::error;
      out.error_status = 501;
      out.error_message = "transfer-encoding not supported";
      return out;
    }
  }

  bool has_content_length = false;
  size_t content_length = 0;
  if (auto it = out.request.headers.find("content-length");
      it != out.request.headers.end()) {
    has_content_length = true;
    if (!parse_size(trim_ascii(it->second), content_length)) {
      out.state = parse_state::error;
      out.error_status = 400;
      out.error_message = "invalid content-length";
      return out;
    }
    if (content_length > limits.max_body_bytes) {
      out.state = parse_state::error;
      out.error_status = 413;
      out.error_message = "body size limit exceeded";
      return out;
    }
  }

  if (out.request.method == "POST" && !has_content_length) {
    out.state = parse_state::error;
    out.error_status = 411;
    out.error_message = "content-length required";
    return out;
  }

  const size_t total_needed = header_end + 4 + content_length;
  if (buffer.size() < total_needed) {
    out.state = parse_state::need_more;
    return out;
  }

  if (content_length > 0) {
    out.request.body.assign(buffer.data() + header_end + 4, content_length);
  }

  out.consumed = total_needed;
  out.request.keep_alive = request_keep_alive(out.request);
  out.state = parse_state::ok;
  return out;
}

static http_response handle_request(const http_request &request) {
  http_response resp;
  resp.version = request.version;
  resp.close = !request.keep_alive;

  const bool is_get = request.method == "GET";
  const bool is_head = request.method == "HEAD";
  const bool is_post = request.method == "POST";

  if (!is_get && !is_head && !is_post) {
    resp = make_error_response(405, "method not allowed");
    resp.headers.emplace_back("Allow", "GET, HEAD, POST");
    resp.version = request.version;
    resp.close = !request.keep_alive;
    return resp;
  }

  std::string path = request.target;
  if (auto pos = path.find('?'); pos != std::string::npos)
    path.resize(pos);

  if (path == "/" && (is_get || is_head)) {
    resp.headers.emplace_back("Content-Type", "text/html; charset=utf-8");
    resp.body = "<!doctype html>\n"
                "<html><head><title>auxilia http demo</title></head>\n"
                "<body><h1>auxilia http demo</h1>\n"
                "<p>Try <code>/health</code> or <code>/echo</code>.</p>\n"
                "</body></html>\n";
    return resp;
  }

  if (path == "/health" && (is_get || is_head)) {
    resp.headers.emplace_back("Content-Type", "text/plain; charset=utf-8");
    resp.body = "ok\n";
    return resp;
  }

  if (path == "/echo" && is_post) {
    resp.headers.emplace_back("Content-Type", "text/plain; charset=utf-8");
    resp.body = request.body;
    return resp;
  }

  resp = make_error_response(404, "route not found");
  resp.version = request.version;
  resp.close = !request.keep_alive;
  return resp;
}

struct server_config {
  parse_limits limits{};
  size_t read_chunk_bytes = 4096;
};

class http_session : public std::enable_shared_from_this<http_session> {
public:
  http_session(net::socket<net::tcp> socket,
               server_config config,
               std::shared_ptr<spdlog::logger> logger)
      : socket_(std::move(socket)), config_(config),
        logger_(std::move(logger)) {}

  void start() { do_read(); }

private:
  void do_read() {
    if (closing_.load() || read_in_flight_.exchange(true))
      return;
    auto self = shared_from_this();
    auto status = socket_.async_recv(
        config_.read_chunk_bytes,
        [self](StatusOr<net::socket<net::tcp>::bytes_type> result) {
          self->read_in_flight_.store(false);
          self->on_read(std::move(result));
        });
    if (!status) {
      read_in_flight_.store(false);
      status.log_err(logger_);
      close();
    }
  }

  void on_read(StatusOr<net::socket<net::tcp>::bytes_type> result) {
    if (!result) {
      result.log_err(logger_);
      close();
      return;
    }

    auto data = *std::move(result);
    if (data.empty()) {
      close();
      return;
    }

    {
      std::scoped_lock lock(mutex_);
      in_buffer_.append(data);
      process_buffer_locked();
    }

    do_read();
  }

  void queue_response_locked(http_response response, bool include_body) {
    write_queue_.push_back(response.serialize(include_body));
  }

  void process_buffer_locked() {
    while (!closing_.load()) {
      auto parsed = parse_request(in_buffer_, config_.limits);
      if (parsed.state == parse_state::need_more)
        break;
      if (parsed.state == parse_state::error) {
        auto response =
            make_error_response(parsed.error_status, parsed.error_message);
        response.version = "HTTP/1.1";
        response.close = true;
        queue_response_locked(std::move(response), true);
        closing_.store(true);
        in_buffer_.clear();
        break;
      }

      auto request = std::move(parsed.request);
      auto response = handle_request(request);
      const bool include_body = request.method != "HEAD";
      queue_response_locked(std::move(response), include_body);
      in_buffer_.erase(0, parsed.consumed);

      if (!request.keep_alive) {
        closing_.store(true);
        in_buffer_.clear();
        break;
      }
    }

    if (!write_in_flight_)
      start_write_locked();
  }

  void start_write_locked() {
    if (write_in_flight_ || write_queue_.empty())
      return;

    write_in_flight_ = true;
    auto payload = std::move(write_queue_.front());
    write_queue_.pop_front();

    auto self = shared_from_this();
    auto status =
        socket_.async_send(std::move(payload), [self](StatusOr<size_t> result) {
          self->on_write(std::move(result));
        });
    if (!status) {
      status.log_err(logger_);
      write_in_flight_ = false;
      close();
    }
  }

  void on_write(StatusOr<size_t> result) {
    if (!result) {
      result.log_err(logger_);
      close();
      return;
    }

    bool should_close = false;
    {
      std::scoped_lock lock(mutex_);
      write_in_flight_ = false;
      if (!write_queue_.empty()) {
        start_write_locked();
        return;
      }
      should_close = closing_.load();
    }

    if (should_close)
      close();
  }

  void close() {
    closing_.store(true);
    if (closed_.exchange(true))
      return;
    socket_.close().log_err(logger_);
  }

  net::socket<net::tcp> socket_;
  server_config config_;
  std::shared_ptr<spdlog::logger> logger_;
  std::mutex mutex_;
  std::string in_buffer_;
  std::deque<std::string> write_queue_;
  bool write_in_flight_ = false;
  std::atomic<bool> closing_{false};
  std::atomic<bool> read_in_flight_{false};
  std::atomic<bool> closed_{false};
};

static int run_http_server(net::io_context &ctx, const options &opts) {
  auto logger = spdlog::stdout_color_mt("http_server");

  auto listener = net::socket<net::tcp>(ctx, net::ip::family::v4);
  if (auto status =
          listener.bind(net::endpoint<net::tcp>(opts.host, opts.port));
      !status) {
    status.log_err(logger);
    return 1;
  }
  if (auto status = listener.listen(); !status) {
    status.log_err(logger);
    return 1;
  }

  server_config config{};
  config.limits.max_header_bytes = opts.max_header_bytes;
  config.limits.max_body_bytes = opts.max_body_bytes;
  config.read_chunk_bytes = opts.read_chunk_bytes;

  logger->info("http server listening on {}:{}", opts.host, opts.port);
  logger->info("type /quit to exit");

  [[maybe_unused]] auto workers = start_workers(ctx, opts.workers);

  std::jthread accept_thread([&](std::stop_token st) {
    while (!st.stop_requested()) {
      auto accepted = listener.accept();
      if (!accepted) {
        if (st.stop_requested())
          break;
        accepted.log_err(logger);
        continue;
      }

      auto socket = std::move(accepted).value();
      if (auto remote = socket.remote_endpoint(); remote)
        logger->info("client connected {}", *remote);
      else
        logger->info("client connected");

      auto session =
          std::make_shared<http_session>(std::move(socket), config, logger);
      session->start();
    }
  });

  std::string line;
  while (std::getline(std::cin, line)) {
    if (line == "/quit")
      break;
  }

  listener.close().log_err(logger);
  accept_thread.request_stop();
  ctx.stop(opts.workers);
  return 0;
}

static void loginit() {
  auxilia::set_console_output_cp_utf8();
  spdlog::set_level(spdlog::level::info);
  spdlog::set_pattern("[%n: %^%l%$] %v");
  spdlog::default_logger()->set_pattern("[%^%l%$] %v");
}

int main(int argc, char **argv) {
  loginit();

  const auto opts = parse_args(argc, argv);
  if (!opts)
    return 1;

  net::io_context ctx;
  auto base_logger = spdlog::stdout_color_mt("http");
  if (auto status = ctx.initialize(); !status) {
    status.log_err(base_logger);
    return 1;
  }

  return run_http_server(ctx, *opts);
}
