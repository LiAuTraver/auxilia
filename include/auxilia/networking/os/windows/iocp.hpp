#pragma once

#ifdef _WIN32

#  if __INTELLISENSE__
#    include "auxilia/networking/os.hpp"
#  endif

namespace auxilia::net::details {
struct iocp_operation {
  OVERLAPPED overlapped{};
  using complete_fn = void (*)(iocp_operation *, DWORD, DWORD) noexcept;
  complete_fn complete = nullptr;
};

struct iocp_completion {
  iocp_operation *op = nullptr;
  DWORD bytes = 0;
  DWORD error = ERROR_SUCCESS;
};
} // namespace auxilia::net::details

#endif
