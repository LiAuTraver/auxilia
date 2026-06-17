#pragma once

#include <coroutine>
#include <exception>
#include <utility>
#include <functional>
#include <atomic>
#include <optional>

#include "auxilia/base/macros.hpp"
#include "auxilia/base/config.hpp"
#include "auxilia/status/Status.hpp"
#include "auxilia/status/StatusOr.hpp"

namespace auxilia::net {

/// @note This task is **VERY** dangerous. it's just for my primitive
/// use/workaround; it may cause lifetime and use-after-free issues.
class detached_task {
public:
  struct promise_type {
    static constexpr detached_task get_return_object() noexcept { return {}; }
    static constexpr std::suspend_never initial_suspend() noexcept {
      return {};
    }
    static constexpr std::suspend_never final_suspend() noexcept { return {}; }
    static constexpr void return_void() noexcept {}
    [[noreturn]] static void unhandled_exception() {
      std::rethrow_exception(std::current_exception());
    }
  };
};
/// @brief A simple awaitable type that wraps the async operation. It is
/// designed specifically for inter-thread async operations, like network io
/// here.
///
/// @note There's a callback captures `this` - hence the awaiter needs one of
/// the following contracts:
/// - The coroutine frame is guaranteed to outlive the operation.
/// - Destroying the coroutine cancels the operation and guarantees no later
/// callback. (cancellation is WIP, though. :(
/// - The operation state is heap-owned/shared, so the callback does not capture
/// a raw pointer into the coroutine frame.
///
/// @bug per the item 2 described above, if the coroutine ended unexpectedly,
/// smth bad may happen.
///
/// @todo:
/// - Cancellation support.
/// - It enforces eager initiation. May add a lazy version.
template <typename Ty> class awaitable {
public:
  using value_type = Ty;
  using result_type = StatusOr<value_type>;
  /// calls when results are ready.
  using handler_type = std::move_only_function<void(result_type)>;
  using start_fn = std::move_only_function<Status(handler_type)>;

private:
  /// @remarks The state machine:
  /// @code
  ///             idle ----
  ///              |       \ (error path)
  ///           pending -----------------------
  ///          /        \                     |
  ///       suspended   |                     |
  ///          \        /                     |
  ///          completed  <--------------------
  /// @endcode
  enum state : uint8_t {
    /// Awaiter has not started yet.
    idle = 0,
    /// `await_suspend` is currently starting the operation.
    ///
    /// If completion happens now, the handler should store the result but not
    /// resume the coroutine.
    pending = 1,
    /// `await_suspend` has committed to suspension.
    ///
    /// If completion happens now, the handler must resume the coroutine.
    suspended = 2,
    /// The result is available and can be retrieved by `await_resume`.
    completed = 3
  };

public:
  explicit awaitable(start_fn start) noexcept : start_(std::move(start)) {}
  awaitable(awaitable &&that) noexcept {
    // my brain got jammed and this may not be correct;
    // if the state is not idle, the lamda still captures the old `this` object.
    AC_RUNTIME_ASSERT(that.state_.load(std::memory_order::relaxed) == idle,
                      "cannot move awaitable after it has started");

    start_ = std::move(that.start_);
    result_ = std::move(that.result_);
    coro_ = std::exchange(that.coro_, {});
    state_.store(idle, std::memory_order::relaxed);
  }
  awaitable &operator=(awaitable &&) noexcept = delete;
  awaitable(const awaitable &) = delete;
  awaitable &operator=(const awaitable &) = delete;

  static constexpr bool await_ready() noexcept { return false; }

  bool await_suspend(const std::coroutine_handle<> coro) {
    coro_ = std::move(coro);

    if (auto expected = idle;
        !state_.compare_exchange_strong(expected,
                                        pending,
                                        std::memory_order::release,
                                        std::memory_order::relaxed))
        [[unlikely]] {
      result_.emplace(FailedPreconditionError("awaitable already used."));
      state_.store(completed, std::memory_order::release);
      return false;
    }
    if (!start_) [[unlikely]] {
      result_.emplace(FailedPreconditionError("async operation is empty."));
      state_.store(completed, std::memory_order::release);
      return false;
    }

    // fire the async operation.
    auto status = start_(handler_type([this](result_type result) mutable {
      result_.emplace(std::move(result));
      if (state_.exchange(completed, std::memory_order::acq_rel) == suspended)
        coro_.resume();
    }));

    if (!status) {
      // 1): failed to start the coroutine immediately.
      result_.emplace(std::move(status));
      state_.store(completed, std::memory_order::release);
      return false;
    }

    auto expected = pending;

    // if the CAS succeeds, it means 2): the async operation completed
    // synchronously hence no need to suspend the coroutine;
    // if it fails, 3): the async operation is still pending and the
    // coroutine should be suspended until the handler is called.
    return state_.compare_exchange_strong(
        expected,
        suspended,
        /* succeeds */ std::memory_order::release,
        /* fails */ std::memory_order::acquire
        // ^^^ the combined `memory_order` in MSVC is `acq_rel`;
        // found inside function `_Combine_cas_memory_orders`.
    );
  }

  result_type await_resume() {
    AC_RUNTIME_ASSERT(state_.load(std::memory_order::acquire) == completed,
                      "async operation resumed before completion.");
    // [[maybe_unused]] const auto observed =
    //     state_.load(std::memory_order::acquire);
    // ^^^ / vvv (mostly) same
    // the assert above is not always on, so fence is required.
    std::atomic_thread_fence(std::memory_order::acquire);
    AC_RUNTIME_ASSERT(result_.has_value(), "async operation did not complete.");
    return *std::move(result_);
  }

private:
  std::optional<result_type> result_;
  start_fn start_;
  std::coroutine_handle<> coro_;
  std::atomic<state> state_ = idle;
};

} // namespace auxilia::net
