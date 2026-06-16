#pragma once

#include <mutex>
#include <shared_mutex>

namespace auxilia::concurrency {
template <typename ContainerType> class ProxyMixin {

  template <bool IsConst> class AccessorImpl {
    using LockType = std::conditional_t<IsConst,
                                        std::shared_lock<std::shared_mutex>,
                                        std::unique_lock<std::shared_mutex>>;
    using ContType =
        std::conditional_t<IsConst, const ContainerType, ContainerType>;

    LockType lock;
    ContType &container;

  public:
    AccessorImpl(std::shared_mutex &mutex, ContType &c)
        : lock(mutex), container(c) {}

    auto *operator->(this auto &&self) { return &self.container; }
    auto &&operator*(this auto &&self) { return self.container; }

    decltype(auto) begin(this auto &&self) { return self.container.begin(); }
    decltype(auto) end(this auto &&self) { return self.container.end(); }
    decltype(auto) cbegin(this auto &&self) { return self.container.cbegin(); }
    decltype(auto) cend(this auto &&self) { return self.container.cend(); }
  };

public:
  using proxy = AccessorImpl<false>;
  using const_proxy = AccessorImpl<true>;

  proxy lock(this auto &&self) {
    return proxy(self.get_mutex(), self.get_raw());
  }

  const_proxy lock_shared(this const auto &self) {
    return const_proxy(self.get_mutex(), self.get_raw());
  }
};
} // namespace auxilia::concurrency
