#pragma once

#include <memory>
#include <mutex>
#include <shared_mutex>
#include <utility>
#include <vector>

#include "auxilia/base/config.hpp"
#include "auxilia/base/format.hpp"

#include "./patterns/proxy.hpp"
#include "./patterns/functional.hpp"

namespace auxilia::concurrency {
/// A thin wrapper around `std::vector` with `std::shared_mutex`. the
/// performance is not good of course, but may use when `tbb`/`ppl` library is
/// not available; it's just an wrapper essentially.
///
/// WIP.
///
/// @note this is BAD for most cases...
template <typename Ty, typename AllocTy = std::allocator<Ty>>
class AC_EMPTY_BASES vector : public Printable,
                              public ProxyMixin<std::vector<Ty, AllocTy>>,
                              public FunctionalMixin<std::vector<Ty, AllocTy>> {
  std::vector<Ty, AllocTy> myVec;
  mutable std::shared_mutex myMutex;

public:
  using container_type = std::vector<Ty, AllocTy>;
  using mutex_type = std::shared_mutex;
  using base_type = ProxyMixin<std::vector<Ty, AllocTy>>;

public:
  using value_type = typename container_type::value_type;
  using allocator_type = typename container_type::allocator_type;
  using size_type = typename container_type::size_type;
  using difference_type = typename container_type::difference_type;
  using reference = typename container_type::reference;
  using const_reference = typename container_type::const_reference;
  using pointer = typename container_type::pointer;
  using const_pointer = typename container_type::const_pointer;

public:
  using iterator = typename container_type::iterator;
  using const_iterator = typename container_type::const_iterator;
  using reverse_iterator = typename container_type::reverse_iterator;
  using const_reverse_iterator =
      typename container_type::const_reverse_iterator;

public:
  AC_CONSTEXPR20 vector(auto &&...args) noexcept(
      noexcept(container_type(std::forward<decltype(args)>(args)...)))
      : myVec(std::forward<decltype(args)>(args)...) {}
  AC_CONSTEXPR20 ~vector() noexcept = default;

public:
  auto to_string(const FormatPolicy) const {
    std::shared_lock lock(myMutex);
    return Format(myVec);
  }

public:
  auto &&get_raw(this auto &&self) noexcept { return self.myVec; }
  auto &&get_mutex(this auto &&self) { return self.myMutex; }
};
} // namespace auxilia::concurrency
