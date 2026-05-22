#pragma once

#include <type_traits>
#include "os.hpp"
#include "ip.hpp"

namespace auxilia::net {
template <typename Protocol> class endpoint {
  // private:
public:
  union {
    details::sockaddr_t base;
    details::sockaddr_in4_t v4;
    details::sockaddr_in6_t v6;
  };
};
static_assert(std::is_trivially_destructible_v<endpoint<void>>);
} // namespace auxilia::net
