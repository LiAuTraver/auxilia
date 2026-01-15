#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <ranges>
#include <stack>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "Automaton.hpp"

#include "accat/auxilia/container/chars.hpp"
#include "accat/auxilia/status/Status.hpp"

namespace accat::cp {
using auxilia::as_chars;
using auxilia::epsilon;
using auxilia::Format;
using auxilia::FormatPolicy;
using auxilia::InternalError;
using auxilia::InvalidArgumentError;
using auxilia::npos;
using auxilia::OkStatus;
using auxilia::Print;
using auxilia::Println;
using auxilia::raw;
using auxilia::Status;
using auxilia::StatusOr;
using auxilia::UnimplementedError;
using auxilia::widen;
} // namespace accat::cp

#pragma region Automaton
namespace accat::cp::details {
auto _automaton_base::Transition::to_string(const FormatPolicy policy) const
    -> string_type {
  if (policy == FormatPolicy::kDefault)
    return Format(
        "--{}->{}", is_epsilon() ? epsilon : widen(symbol), target_id);
  if (policy == FormatPolicy::kBrief)
    return Format("{}: {}", is_epsilon() ? epsilon : widen(symbol), target_id);
  if (policy == FormatPolicy::kDetailed)
    return Format("Transition{{target_id={}, symbol={}}}",
                  target_id,
                  is_epsilon() ? epsilon : widen(symbol));
  AC_UNREACHABLE()
}
const char *_automaton_base::State::type_string() const {
  switch (type) {
  case Type::kIntermediate:
    return "";
  case Type::kStart:
    return "Start";
  case Type::kAccept:
    return "Accept";
  case Type::kStartAndAccept:
    return "Start & Accept";
  default:
    break;
  }
  AC_UNREACHABLE("Unknown State Type: {}", (type))
  return nullptr;
}
auto _automaton_base::State::to_string(const FormatPolicy policy) const
    -> string_type {
  std::string result;

  if (policy == FormatPolicy::kDetailed) {
    result += Format("{{type={}, edges=[", type_string());
    for (const auto &e : edges)
      result += Format("{} ,", e.to_string(policy));
    result += "]}";
    return result;
  }

  if (type == Type::kIntermediate)
    result += Format("{}\n", type_string());

  if (policy == FormatPolicy::kDefault)
    for (const auto &e : edges)
      result += Format("{}\n", e.to_string(policy));
  else if (policy == FormatPolicy::kBrief)
    result += Format("[{} edges]", edges.size());

  return result;
}
auto _automaton_base::_dot_states(const StatesTy &states) -> string_type {
  std::string dot;
  // mark start and accept distinctly
  for (const auto &[id, s] : states) {
    const auto isAccept = (s.type == State::Type::kAccept or
                           s.type == State::Type::kStartAndAccept);

    // doublecircle for accept states, single for others
    dot += Format(R"~~~({0}[label="{0}\n{1}", shape={2}circle];)~~~"
                  " \n",
                  id,
                  s.type_string(),
                  isAccept ? "double" : "");
  }
  return dot;
}
void _automaton_base::closure(std::unordered_set<size_t> &state_set,
                              const char ch) const {

  // FIXME: poor use of vec
  // R1:    changed vec to stack
  std::stack stack(std::from_range, state_set);
  while (!stack.empty()) {
    auto current = stack.top();
    stack.pop();
    std::ranges::for_each(
        states.at(current).edges | std::views::as_const, [&](auto &&edge) {
          edge.symbol == ch &&
                             // whether the target state is already in the set
                             state_set.emplace(edge.target_id)
                                 .second
                                     // second is true if insertion took place
                                     &&stack.emplace(edge.target_id);
        });
  }
}
auto _automaton_base::from_char(const char c) -> Fragment {
  const auto beg = new_state();
  const auto end = new_state();
  add_transition(beg, end, c);
  return {beg, end};
}
auto _automaton_base::concat(Fragment &&a, Fragment &&b) -> Fragment {
  add_transition(a.end, b.start);
  return {a.start, b.end};
}
auto _automaton_base::union_operation(Fragment &&a, Fragment &&b) -> Fragment {
  const auto beg = new_state();
  const auto end = new_state();
  // ... beg a|b end ...
  add_transition(beg, a.start);
  add_transition(beg, b.start);

  add_transition(a.end, end);
  add_transition(b.end, end);
  return {beg, end};
}
auto _automaton_base::kleene_star(Fragment &&f) -> Fragment {
  const auto beg = new_state();
  const auto end = new_state();

  // ... beg a* end ...
  add_transition(beg, f.start);
  add_transition(f.end, end);

  // 0 occurrence
  add_transition(beg, end);

  // more occurrences
  add_transition(f.end, f.start);

  return {beg, end};
}
auto _automaton_base::_dot_transitions() const -> std::string {
  using auxilia::literals::operator""_raw;
  std::string dot;
  for (const auto &[id, s] : states) {
    for (const auto &e : s.edges) {
      dot += Format(R"(  {} -> {} [label="{}"];)"
                    "\n",
                    id,
                    e.target_id,
                    e.is_epsilon() ? epsilon : widen(e.symbol));
    }
  }
  return dot;
}
bool _automaton_base::test(const std::string_view input) {
  if (empty())
    return input.empty();

  if (!input_alphabet.empty() &&
      std::ranges::any_of(input, std::not_fn([this](const char c) {
                            return (input_alphabet.contains(c) &&
                                    !is_operator(c));
                          })))
    return false;

  auto current_states = epsilon_closure(start_id);

  for (const char c : input) {
    decltype(current_states) next_states;
    for (const auto state_id : current_states)
      for (const auto &edge : states[state_id].edges)
        if (!edge.is_epsilon() && edge.symbol == c)
          next_states.emplace(edge.target_id);

    if (next_states.empty())
      return false;
    epsilon_closure(next_states);
    current_states = std::move(next_states);
  }

  return std::ranges::any_of(accept_ids, [&](const size_t accept_id) {
    return current_states.contains(accept_id);
  });
}
auto _automaton_base::to_string(const FormatPolicy policy) const
    -> std::string {
  if (empty())
    return "Automaton <empty>";

  std::string result = "Automaton: [\n";
  for (const auto &[id, s] : states)
    result += Format("State {}: {}\n", id, s.to_string(policy));

  result.replace(result.end() - 1, result.end(), "]");
  result += "\n";
  return result;
}
} // namespace accat::cp::details
#pragma endregion Automaton
