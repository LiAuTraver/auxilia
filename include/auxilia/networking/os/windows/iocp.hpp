#pragma once

#ifdef _WIN32

#  include <WinSock2.h>

#  if __INTELLISENSE__
#    include "auxilia/base/config.hpp"
#  endif

namespace auxilia::net::details::iocp {
struct operation {
  using complete_fn = void (*)(operation *, DWORD, DWORD) noexcept;
  constexpr explicit operation(const complete_fn complete = nullptr) noexcept
      : overlapped({}), complete(complete) {}
  OVERLAPPED overlapped;
  complete_fn complete;
};

struct completion {
  operation *op;
  DWORD bytes;
  DWORD error;
};
} // namespace auxilia::net::details::iocp

#endif
