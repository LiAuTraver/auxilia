#pragma once

#ifdef _WIN32

#  if __INTELLISENSE__
#    include "auxilia/networking/os.hpp"
#  endif

namespace auxilia::net::details {
struct AC_EMPTY_BASES AC_NOVTABLE iocp_operation {
  using complete_fn = void (*)(iocp_operation *, DWORD, DWORD) noexcept;
  constexpr iocp_operation(complete_fn complete = nullptr) noexcept
      : overlapped({}), complete(complete) {}
  OVERLAPPED overlapped;
  complete_fn complete;
};

struct AC_EMPTY_BASES AC_NOVTABLE iocp_completion {
  iocp_operation *op = nullptr;
  DWORD bytes = 0;
  DWORD error = ERROR_SUCCESS;
};
} // namespace auxilia::net::details

#endif
