#include <auxilia/auxilia.hpp>
#include "auxilia/networking/net.hpp"

int main() {
  constexpr auto v4 = auxilia::net::ip::address_v4::loopback();
  auxilia::Println(v4.to_string(auxilia::FormatPolicy::kBrief));
  auxilia::Println(v4.to_string(auxilia::FormatPolicy::kDefault));
  auxilia::Println(v4.to_string(auxilia::FormatPolicy::kDetailed));
  constexpr auto v6 = auxilia::net::ip::address_v6::loopback();
  auxilia::Println(v6.to_string(auxilia::FormatPolicy::kBrief));
  auxilia::Println(v6.to_string(auxilia::FormatPolicy::kDefault));
  auxilia::Println(v6.to_string(auxilia::FormatPolicy::kDetailed));

  constexpr auxilia::net::ip::address addr{v4};
  constexpr auto addr2 = auxilia::net::ip::address(v6);
  static_assert(addr.is_ipv4());
  static_assert(addr2.is_ipv6());
}
