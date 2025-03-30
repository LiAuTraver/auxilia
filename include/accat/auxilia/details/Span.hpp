#pragma once
#include <cstddef>
#include <span>
#include <vector>
#include <cassert>
#include <initializer_list>

#include "./config.hpp"

namespace accat::auxilia {
/// @brief A span that can be used to access elements across multiple spans
/// @remark has little usage
template <typename T, size_t = std::dynamic_extent> class incontiguous_span {
public:
  using value_type = T;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using reference = T &;
  using const_reference = const T &;
  using pointer = T *;
  using const_pointer = const T *;

public:
  incontiguous_span() = default;
  incontiguous_span(std::initializer_list<std::span<value_type>> spans)
      : spans_{spans} {}
  reference operator[](size_type idx) {
    for (auto &span : spans_) {
      if (idx < span.size()) {
        return span[idx];
      }
      idx -= span.size();
    }
#if __cpp_exceptions
    throw std::out_of_range("Index is out of bounds in incontiguous span");
#else
    contract_assert(false, "Index is out of bounds in incontiguous span");
    return nullptr;
#endif
  }

  // Get the total size across all spans
  size_type size() const {
    size_type total_size = 0;
    for (const auto &span : spans_) {
      total_size += span.size();
    }
    return total_size;
  }

private:
  std::vector<std::span<value_type>> spans_;
};
}; // namespace accat::auxilia
