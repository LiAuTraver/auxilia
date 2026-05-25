
#include <thread>
#include "auxilia/networking/net.hpp"
#include "auxilia/networking/socket.hpp"

using namespace auxilia;
using namespace auxilia::net;
using namespace auxilia::net::ip;
using enum FormatPolicy;

io_context ctx;

constexpr auto v4 = address_v4::loopback();
constexpr auto e = endpoint<tcp>{v4, 65432};
void client() {
  std::this_thread::sleep_for(std::chrono::seconds(1));
  ctx.initialize().log_if_err();
  auto s = net::socket<tcp>(ctx, ip::family::v4);
  s.connect(e).log_if_err();
  s.send_bytes("HIIIIIIIIIIIIIIIII").log_if_err();
}
void server() {
  ctx.initialize().log_if_err();
  auto s = net::socket<tcp>(ctx, ip::family::v4);
  s.bind(e).log_if_err();
  s.listen().log_if_err();
  std::cout << "received: " << s.accept().and_then<net::socket<tcp>::recv_>();
}
int main() {
  Println(e);
  auto server = std::jthread(::server);
  auto client = std::jthread(::client);
}
