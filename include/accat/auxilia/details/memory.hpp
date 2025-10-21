#pragma once

#include "./macros.hpp"

namespace accat::auxilia {
[[noreturn]] [[gnu::cold]] inline void *OnMemAllocFailed() {
  AC_THROW_OR_DIE_("memory allocation failed");
}
template <typename T = void>
[[using gnu: malloc, returns_nonnull]] inline T *alloc(const size_t size)
    [[clang::allocating]] {
  if (auto ptr = malloc(size))
    return static_cast<T *>(ptr);

  [[unlikely]] return OnMemAllocFailed();
}
template <typename T = void>
[[using gnu: malloc, returns_nonnull]]
inline T *dynamic_alloc(const size_t size) [[clang::allocating]] {
  if (auto ptr =
#if defined(_malloca)
          _malloca(size)
#elif defined(alloca)
          alloca(size)
#else
          malloc(size)
#endif
  )
    return static_cast<T *>(ptr);
  [[unlikely]] return OnMemAllocFailed();
}

EXPORT_AUXILIA
class MemoryPool : public std::pmr::memory_resource {
private:
  void *const buffer;
  size_t remaining_size;
  const size_t total_size;

public:
  MemoryPool(void *t, const size_t size) noexcept
      : buffer(t), remaining_size(size), total_size(size) {}
  static MemoryPool FromSize(const size_t size) noexcept {
    return {dynamic_alloc(size), size};
  }

private:
  virtual void *do_allocate(const size_t bytes, const size_t align) override {
    auto current =
        reinterpret_cast<uintptr_t>(buffer) + total_size - remaining_size;
    auto aligned = (current + align - 1) & ~(align - 1);
    auto aligned_offset = aligned - current;
    if (aligned_offset + bytes > remaining_size) {
      AC_RUNTIME_ASSERT(0, "out of memory")
      return nullptr;
    }
    remaining_size -= (aligned_offset + bytes);
    return reinterpret_cast<void *>(aligned);
  };

  virtual void
  do_deallocate(void *, size_t, size_t) noexcept override { /* do nothing */ }

  virtual bool
  do_is_equal(const std::pmr::memory_resource &other) const noexcept override {
    return this == &other;
  }
};
} // namespace accat::auxilia
