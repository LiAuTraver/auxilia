#pragma once

#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <ranges>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <accat/auxilia/defines.hpp>
#include <accat/auxilia/auxfwd.hpp>

#include "accat/auxilia/base/format.hpp"
#include "accat/auxilia/container/chars.hpp"

namespace accat::cp::details {
class _automaton_base : auxilia::Printable {
protected:
  _automaton_base() noexcept = default;
  _automaton_base(_automaton_base &&) noexcept = default;
  _automaton_base &operator=(_automaton_base &&) noexcept = default;

  // we don't add const here for move-ctor/assignment,
  // but it's logically immutable once it is assigned.
  // also: const member field serves no purpose.
  struct Transition {
    /* const */ size_t target_id = auxilia::npos;
    /* const */ char symbol = '\0';
    [[nodiscard]] bool is_epsilon() const noexcept { return symbol == '\0'; }
    string_type to_string(
        auxilia::FormatPolicy policy = auxilia::FormatPolicy::kDefault) const;
  };

  // partial NFA, used during construction
  struct Fragment {
    /* const */ size_t start = auxilia::npos;
    /* const */ size_t end = auxilia::npos;
  };
  AC_STATIC_ASSERT(std::conjunction_v<std::is_aggregate<Transition>,
                                      std::is_aggregate<Fragment>,
                                      std::is_trivially_copyable<Transition>,
                                      std::is_trivially_copyable<Fragment>>);
  struct State : Printable {
    using edges_t = std::unordered_set<
        Transition,
        decltype([](const Transition &t) {
          const auto h1 = std::hash<size_t>{}(t.target_id);
          const auto h2 = std::hash<char>{}(t.symbol);
          return h1 ^ (h2 + auxilia::hash_magic_number_32bit + (h1 << 6) +
                       (h1 >> 2));
        }),
        decltype([](const Transition &lhs, const Transition &rhs) {
          return lhs.target_id == rhs.target_id && lhs.symbol == rhs.symbol;
        })>;

    enum struct [[clang::flag_enum]] Type : unsigned short {
      kIntermediate = 0b00, // state neither start nor accept
      kStart = 0b01,
      kAccept = 0b10,
      kStartAndAccept = kStart | kAccept // 0b11
    } type = Type::kIntermediate;
    const char *type_string() const;
    edges_t edges;
    State() noexcept = default;
    State(const State &) = delete;
    State(State &&) noexcept = default;
    State &operator=(const State &) = delete;
    State &operator=(State &&) noexcept = default;

    explicit State(const Type type) noexcept : type(type) {}
    string_type to_string(
        auxilia::FormatPolicy policy = auxilia::FormatPolicy::kDefault) const;
  };
  /// we automatically infer input alphabet from a given regex.
  ///
  /// note: currently only english letters, digits and common symbols are
  /// supported.
  std::string input_alphabet;
  std::unordered_map<size_t, State> states;
  // conceptually we can have multiple start states,
  // but they are just elements in the same epsilon closure.
  size_t start_id = auxilia::npos;
  // in Thompson's construction, only one accept state
  // size_t accept_id = npos;
  std::unordered_set<size_t> accept_ids;

  using StatesTy = std::unordered_map<size_t, State>;

  static string_type _dot_states(const StatesTy &states);

  size_t new_state(this auto &&self,
                   State::Type type = State::Type::kIntermediate) {
    auto id = self.states.size();
    self.states.emplace(id, type);
    return id;
  }
  void
  add_transition(const size_t from, const size_t to, const char symbol = '\0') {
    precondition(from < states.size() && to < states.size(), "out of range")
    states[from].edges.emplace(to, symbol);
  }
  void closure(std::unordered_set<size_t> &state_set, char ch) const;
  void epsilon_closure(std::unordered_set<size_t> &state_set) const {
    closure(state_set, '\0');
  }
  auto epsilon_closure(const size_t state_id) const {
    std::unordered_set state_set = {state_id};
    epsilon_closure(state_set);
    return state_set;
  }
  [[nodiscard]] bool empty() const noexcept {
    return states.empty() && start_id == auxilia::npos && accept_ids.empty();
  }
  [[nodiscard]]
  Fragment from_char(char c);
  /// @brief Concatenate two fragments: f = ab
  [[nodiscard]]
  Fragment concat(Fragment &&a, Fragment &&b);
  /// @brief Union of two fragments: f = a | b
  [[nodiscard]]
  Fragment union_operation(Fragment &&a, Fragment &&b);
  /// @brief Kleene star of a fragment: f = a* = aaaa...
  [[nodiscard]]
  Fragment kleene_star(Fragment &&f);
  [[nodiscard]] auto _dot_transitions() const -> std::string;
  [[nodiscard]] bool test(std::string_view input);
  [[nodiscard]] auto
  to_dot(this const auto &self,
         auxilia::FormatPolicy policy = auxilia::FormatPolicy::kDefault)
      -> std::string;
  [[nodiscard]] auto to_string(
      auxilia::FormatPolicy policy = auxilia::FormatPolicy::kDefault) const
      -> std::string;
  static constexpr auto unformatted_header = auxilia::raw(R"(
digraph {} {{
  rankdir=LR;
  node [shape=circle, fontsize=12];
  node [shape=point]; start;
  start -> {};
)");
  static constexpr auto operators = auxilia::as_chars("|*.+?()");
  static constexpr bool is_operator(const char c) {
    return operators.find(c) != auxilia::npos;
  }
  static constexpr bool is_regex_operator(const char c) {
    return c != '(' && c != ')' && is_operator(c);
  }
};
auto _automaton_base::to_dot(this const auto &self,
                             const auxilia::FormatPolicy policy)
    -> std::string {
  if (self.empty())
    return auxilia::raw(R"(
digraph Automaton {
  // empty
}
                )");

  return self._to_dot_impl(policy);
}
} // namespace accat::cp::details
