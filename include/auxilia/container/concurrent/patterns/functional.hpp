#pragma once

#include <utility>
#include <algorithm>
#include <mutex>
#include <shared_mutex>

namespace auxilia::concurrency {
template <typename ContainerType> class FunctionalMixin {
private:
  using container_type = ContainerType;
  using value_type = container_type::value_type;

public:
  /// User should:
  /// 1. avoid returning reference/pointer from the lambda. (dangling reference)
  /// 2. use non-blocking operation inside the lambda.
  /// 3. don't call `read`/`write`/`for_each' recursively inside. (deadlock)
  template <typename Func>
  inline decltype(auto) read(this const auto &self, Func &&func) {
    std::shared_lock lock(self.get_mutex());
    return std::invoke(std::forward<Func>(func), self.get_raw());
  }
  /// @copydoc read
  template <typename Func>
  inline decltype(auto) write(this auto &self, Func &&func) {
    std::unique_lock lock(self.get_mutex());
    return std::invoke(std::forward<Func>(func), self.get_raw());
  }
  /// @copydoc read
  template <typename Func>
  inline decltype(auto) for_each(this const auto &self, Func &&f) {
    static_assert(
        std::invocable<Func, typename container_type::const_reference>);
    std::shared_lock lock(self.get_mutex());
    return std::ranges::for_each(self.get_raw(), std::forward<Func>(f));
  }
  /// @copydoc read
  inline decltype(auto) remove(this auto &self, const value_type &value) {
    std::unique_lock lock(self.get_mutex());
    return std::erase(self.get_raw(), value);
  }
  /// @copydoc read
  template <typename Func>
  inline decltype(auto) remove_if(this auto &self, Func &&f) {
    std::unique_lock lock(self.get_mutex());
    return std::erase_if(self.get_raw(), std::forward<Func>(f));
  }

  /// @copydoc read
  inline decltype(auto) push_back(this auto &self, const value_type &value) {
    std::unique_lock lock(self.get_mutex());
    return self.get_raw().push_back(value);
  }
  /// @copydoc read
  inline decltype(auto) push_back(this auto &self, value_type &&value) {
    std::unique_lock lock(self.get_mutex());
    return self.get_raw().push_back(std::move(value));
  }
  /// @copydoc read
  template <typename... Args>
  inline decltype(auto) emplace_back(this auto &self, Args &&...args) {
    std::unique_lock lock(self.get_mutex());
    return self.get_raw().emplace_back(std::forward<Args>(args)...);
  }
};
} // namespace auxilia::concurrency
