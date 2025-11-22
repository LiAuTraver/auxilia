#pragma once

#include <coroutine>
#include <cstddef>
#include <iterator>
#include <ranges>
#include <utility>
#include <optional>
#include <exception>
#include <type_traits>
#include <memory>

#include "config.hpp"

namespace accat::auxilia {
/// @brief A generator class that can be used to generate values
/// lazily, and can be used in a range-based for loop.
/// @tparam YieldType the type of the value to be yielded
/// @tparam ReturnType the type of the value to be returned, usually
/// its 'void' for no return value(infinite sequence generator)
/// @tparam AllocatorType the allocator type to be used for the generator
/// @note I used this in some cases where std::geneartor isn't available yet.
/// Currently there's no plan for me to implement standard-complient features
/// like recursive generators, type-erasure, polymorphic
/// resources(std::pmr::meow), and reference types(not sure of this though).
template <typename YieldType,
          typename ReturnType = void,
          typename AllocatorType = std::allocator<char>>
class [[using clang:
        // coro_return_type,
        coro_lifetimebound]] Generator
    : public std::ranges::view_interface<
          Generator<YieldType, ReturnType, AllocatorType>> {
  static_assert(
      std::conjunction_v<
          std::is_same<std::remove_reference_t<YieldType>, YieldType>,
          std::is_same<std::remove_reference_t<ReturnType>, ReturnType>>,
      "This custom generator cannot gurantee if the coro returned is a "
      "reference type; use std::generator instead if possible.");
  static_assert(std::is_default_constructible_v<YieldType>,
                "YieldType must be default constructible");
  static_assert(std::is_nothrow_move_constructible_v<YieldType>,
                "YieldType must be nothrow move constructible");
  static_assert(!std::is_same_v<YieldType, void>,
                "YieldType must not be void; why use a generator for void?");

  template <typename R, bool = std::is_void_v<R>> struct promise_type_impl;

public:
  struct iterator;

public:
  using promise_type = promise_type_impl<ReturnType>;

private:
  template <typename Derived> struct promise_type_base {
    friend struct iterator;
    friend class Generator;
    ~promise_type_base() {}
#if defined(__cpp_exceptions) || defined(_CPPUNWIND)
    std::exception_ptr exception;
#endif

    Generator get_return_object() {
      return Generator{std::coroutine_handle<promise_type>::from_promise(
          *static_cast<Derived *>(this))};
    }

    std::suspend_always initial_suspend() noexcept { return {}; }
    std::suspend_always final_suspend() noexcept { return {}; }

    std::suspend_always yield_value(const YieldType &value) noexcept {
      static_cast<Derived *>(this)->current_value = std::addressof(value);
      return {};
    }

#if defined(__cpp_exceptions) || defined(_CPPUNWIND)
    void unhandled_exception() /* non-const */ noexcept {
      exception = std::current_exception();
    }

  private:
    void _rethrow_if_exception() {
      if (exception)
        std::rethrow_exception(exception);
    }

  public:
#else
    void unhandled_exception()/* const, but for ABI compatibility, we don't add it */ noexcept {}
#endif

    using CharAlloc = typename std::allocator_traits<
        AllocatorType>::template rebind_alloc<char>;
    static void *operator new(size_t size) {
      CharAlloc my_alloc{};
      return std::allocator_traits<CharAlloc>::allocate(my_alloc, size);
    }

    static void operator delete(void *ptr, size_t size) noexcept {
      CharAlloc my_alloc{};
      std::allocator_traits<CharAlloc>::deallocate(
          my_alloc, static_cast<char *>(ptr), size);
    }
  };

private:
  template <typename R>
  struct AC_EMPTY_BASES
      promise_type_impl<R, false> : promise_type_base<promise_type> {
    static_assert(std::is_nothrow_move_constructible_v<R>,
                  "ReturnType must be nothrow move constructible");
    friend struct iterator;
    union {
      const YieldType *current_value = nullptr;
      R final_return_value;
    };
    void return_value(const R &value) noexcept { final_return_value = value; }
  };
  template <typename R>
  struct AC_EMPTY_BASES
      promise_type_impl<R, true> : promise_type_base<promise_type> {
    friend struct iterator;
    const YieldType *current_value = nullptr;
    void return_void() noexcept {}
  };

public:
  struct iterator {
    using iterator_category = std::input_iterator_tag;
    using value_type = YieldType;
    using difference_type = std::ptrdiff_t;
    using pointer = YieldType *;
    using const_pointer = const YieldType *;
    using reference = YieldType &;
    using const_reference = const YieldType &;

    iterator() = default;
    iterator(std::coroutine_handle<promise_type> handle) : handle_(handle) {
      if (handle_) {
        handle_.resume();
        if (handle_.done()) {
#if defined(__cpp_exceptions) || defined(_CPPUNWIND)
          handle_.promise()._rethrow_if_exception();
#endif
          handle_ = nullptr;
        }
      }
    }

    iterator &operator++() {
      handle_.resume();
      if (handle_.done()) {
#if defined(__cpp_exceptions) || defined(_CPPUNWIND)
        std::exchange(handle_, nullptr).promise()._rethrow_if_exception();
#else
        handle_ = nullptr;
#endif
      }
      return *this;
    }

    void operator++(int) { ++*this; }

    reference operator*() AC_NOEXCEPT {
      return *const_cast<YieldType *>(handle_.promise().current_value);
    }
    pointer operator->() AC_NOEXCEPT { return handle_.promise().current_value; }
    const_reference operator*() const AC_NOEXCEPT {
      return *handle_.promise().current_value;
    }
    const_pointer operator->() const AC_NOEXCEPT {
      return handle_.promise().current_value;
    }

    bool operator==(const iterator &other) const noexcept {
      return handle_ == other.handle_;
    }
    auto operator<=>(const iterator &other) const noexcept {
      return *this <=> other;
    }
    friend bool operator==(const iterator &it,
                           const std::default_sentinel_t) noexcept {
      return it.handle_ == nullptr; // or it.handle_.done()?
    }

  private:
    std::coroutine_handle<promise_type> handle_ = nullptr;
  };
  explicit Generator(std::coroutine_handle<promise_type> handle)
      : handle_(handle) {}
  Generator(Generator &&other) noexcept
      : handle_(std::exchange(other.handle_, nullptr)) {}

  Generator &operator=(Generator &&other) noexcept {
    handle_ = std::exchange(other.handle_, nullptr);
    return *this;
  }

  ~Generator() noexcept {
    if (handle_)
      handle_.destroy();
  }

  iterator begin() { return {handle_}; }
  iterator begin() const { return {handle_}; }
  std::default_sentinel_t end() const noexcept { return std::default_sentinel; }
  /// @note not a shared resource, like a plain `future::get()`,
  /// which cannot be called multiple times
  auto get() {
    AC_PRECONDITION(handle_, "Generator::get() called twice")

    AC_DEFER {
      handle_.destroy();
      handle_ = nullptr;
    };

    while (!handle_.done()) {
      handle_.resume();
    }

#if defined(__cpp_exceptions) || defined(_CPPUNWIND)
    handle_.promise()._rethrow_if_exception();
#endif
    // return the final value (RVO and NRVO only suited for local variables. we
    // didn't return a local variable, nor do we expect the `get` called
    // multiple times)
    return std::move(handle_.promise().final_return_value);
  }

  // next yield value
  auto next() -> std::optional<YieldType> {
    AC_PRECONDITION(handle_, "handle has already been destroyed")
    if (!handle_ || handle_.done())
      return std::nullopt;

    handle_.resume();

    if (handle_.done()) {
      return std::nullopt;
    }
    return std::make_optional(
        std::move(*const_cast<YieldType *>(handle_.promise().current_value)));
  }

private:
  std::coroutine_handle<promise_type> handle_ = nullptr;
};
} // namespace accat::auxilia
