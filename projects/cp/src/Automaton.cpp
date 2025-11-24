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

#include <accat/auxilia/auxilia.hpp>

#include "Automaton.hpp"

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

#pragma region Common
namespace accat::cp {
static constexpr auto operators = as_chars("|*.+?()");
static constexpr auto unformatted_header = raw(R"(
digraph {} {{
  rankdir=LR;
  node [shape=circle, fontsize=12];
  node [shape=point]; start;
  start -> {};
)");
static constexpr bool is_operator(const char c) {
  return operators.find(c) != npos;
}
static constexpr bool is_regex_operator(const char c) {
  return c != '(' && c != ')' && is_operator(c);
}
constexpr size_t precedence(const char op) {
  switch (op) {
  case '*':
  case '+':
  case '?':
    return 3;
  case '.':
    return 2; // explicit concat
  case '|':
    return 1;
  default:
    return 0;
  }
}
} // namespace accat::cp
#pragma endregion Common
#pragma region Automaton
namespace accat::cp::details {
auto AutomatonMixin::Transition::to_string(const FormatPolicy policy) const
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
const char *AutomatonMixin::State::type_string() const {
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
auto AutomatonMixin::State::to_string(const FormatPolicy policy) const
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
auto AutomatonMixin::_dot_states(const StatesTy &states) -> string_type {
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
void AutomatonMixin::closure(std::unordered_set<size_t> &state_set,
                             const char ch) const {

  // FIXME: poor use of vec
  std::vector stack(state_set.begin(), state_set.end());
  while (!stack.empty()) {
    size_t current = stack.back();
    stack.pop_back();
    for (const auto &edge : states.at(current).edges) {
      if (edge.symbol == ch && state_set.emplace(edge.target_id).second) {
        // second is true if insertion took place
        stack.emplace_back(edge.target_id);
      }
    }
  }
}
AutomatonMixin::Fragment AutomatonMixin::from_char(const char c) {
  const auto s = new_state(State::Type::kIntermediate);
  const auto a = new_state(State::Type::kIntermediate);
  add_transition(s, a, c);
  return {s, a};
}
AutomatonMixin::Fragment AutomatonMixin::concat(Fragment &&lhs,
                                                Fragment &&rhs) {
  add_transition(lhs.end, rhs.start, '\0');
  return {lhs.start, rhs.end};
}
AutomatonMixin::Fragment AutomatonMixin::union_operation(Fragment &&a,
                                                         Fragment &&b) {
  const auto s = new_state(State::Type::kIntermediate);
  const auto acc = new_state(State::Type::kIntermediate);

  add_transition(s, a.start, '\0');
  add_transition(s, b.start, '\0');

  add_transition(a.end, acc, '\0');
  add_transition(b.end, acc, '\0');
  return {s, acc};
}
AutomatonMixin::Fragment AutomatonMixin::kleene_star(Fragment &&f) {
  const auto s = new_state(State::Type::kIntermediate);
  const auto acc = new_state(State::Type::kIntermediate);

  add_transition(s, f.start, '\0');
  add_transition(s, acc, '\0');

  add_transition(f.end, f.start, '\0');
  add_transition(f.end, acc, '\0');
  return {s, acc};
}
auto AutomatonMixin::_dot_transitions() const -> std::string {
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
bool AutomatonMixin::test(std::string_view input) {
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
auto AutomatonMixin::to_string(const FormatPolicy policy) const -> std::string {
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
namespace accat::cp {
#pragma region NFA
std::string NFA::preprocess_regex(const std::string_view regex) {
  std::string result;
  // insert `.` for concatenation between
  //        char or `)` `*` `+` `?` followed by char or `(`
  // (here char means non-op char ^^^)
  result.resize_and_overwrite(
      regex.size() * 2,
      [&](char *const out, [[maybe_unused]] const size_t capacity) {
        constexpr auto is_in = [](const char c, const std::string_view chars) {
          return chars.find(c) != npos;
        };
        auto j = 0ull;
        for (auto i = 0ull; i < regex.size(); ++i) {
          const char c = regex[i];
          out[j++] = c;

          if (i + 1 < regex.size() && !is_in(c, "|(") &&
              !is_in(regex[i + 1], "|)*+?"))
            out[j++] = '.';
        }
        return j; // final length
      });
  return result;
}
StatusOr<std::string> NFA::to_postfix(const std::string_view regex) {
  std::string postfix;
  std::string op_stack;
  auto lvl = 0u; // workaround, detect parentheses level
  for (const auto c : regex) {
    if (c == '(') {
      op_stack.push_back(c);
      ++lvl;
      continue;
    }
    if (c == ')') {
      while (!op_stack.empty() && op_stack.back() != '(') {
        postfix += op_stack.back();
        op_stack.pop_back();
      }
      if (op_stack.empty()) {
        return InvalidArgumentError(
            "Unfinished parentheses in regex: missing '('");
      }
      if (op_stack.back() != '(') {
        return InvalidArgumentError(
            "Mismatched parentheses in regex: expected '(', got '{}'",
            op_stack.back());
      }
      op_stack.pop_back(); // pop '('
      --lvl;
      continue;
    }
    if (is_regex_operator(c)) {
      while (!op_stack.empty() && op_stack.back() != '(' &&
             precedence(op_stack.back()) >= precedence(c)) {
        postfix += op_stack.back();
        op_stack.pop_back();
      }
      op_stack.push_back(c);
      continue;
    }

    // normal character
    postfix += c;
  }
  if (lvl != 0) {
    return InvalidArgumentError("Unfinished parentheses in regex: missing ')'");
  }

  // while (!op_stack.empty()) {
  //   postfix += op_stack.back();
  //   op_stack.pop_back();
  // }
  // ^^^ native way
  // vvv failed and non-conforming
  // postfix += std::ranges::reverse(op_stack)._Unwrapped();

  // worked, and fancy :P vvv
  std::ranges::reverse_copy(op_stack, std::back_inserter(postfix));

  return {postfix};
}
auto NFA::_to_dot_impl(const FormatPolicy) const -> std::string {
  auto dot = Format(unformatted_header, "NFA", start_id);

  dot += _dot_states(states);

  dot += "\n";
  dot += _dot_transitions();
  dot += "}\n";
  return dot;
}
void NFA::init_input_alphabet(const std::string_view sv) {
  input_alphabet =
      sv | std::views::filter([](const char c) { return !is_operator(c); }) |
      std::ranges::to<std::string>();
  std::ranges::sort(input_alphabet);

  auto r = std::ranges::unique(input_alphabet);
  input_alphabet.erase(r.begin(), r.end());
}
StatusOr<details::AutomatonMixin::Fragment>
NFA::build_graph(const std::string_view postfix) {
  std::stack<Fragment> stack;
  const auto top_and_pop_stack = [&stack]() {
    AC_PRECONDITION(!stack.empty(), "Stack underflow")
    const auto val = stack.top();
    stack.pop();
    return val;
  };
  for (char c : postfix) {
    if (!is_operator(c)) {
      // regular character
      stack.emplace(from_char(c));
      continue;
    }
    if (c == '|') {
      if (stack.size() < 2) {
        return InvalidArgumentError(
            "Invalid regex at position {}: not enough operands for |",
            postfix.find(c));
      }
      auto b = top_and_pop_stack();
      auto a = top_and_pop_stack();
      stack.emplace(union_operation(std::move(a), std::move(b)));
      continue;
    }
    if (c == '.') {
      if (stack.size() < 2) {
        return InvalidArgumentError("Invalid regex at position {}: not "
                                    "enough operands for concatenation",
                                    postfix.find(c));
      }
      auto b = top_and_pop_stack();
      auto a = top_and_pop_stack();
      stack.emplace(concat(std::move(a), std::move(b)));
      continue;
    }
    if (c == '*') {
      if (stack.empty()) {
        return InvalidArgumentError(
            "Invalid regex at position {}: not enough operands for *",
            postfix.find(c));
      }
      auto f = top_and_pop_stack();
      stack.emplace(kleene_star(std::move(f)));
      continue;
    }
    if (c == '+') {
      // a+ = aa*
      if (stack.empty()) {
        return InvalidArgumentError(
            "Invalid regex at position {}: not enough operands for +",
            postfix.find(c));
      }
      auto f = top_and_pop_stack();
      auto f_copy = // reuse same states
          Fragment{.start = f.start, .end = f.end};
      auto star = kleene_star(std::move(f_copy));
      stack.emplace(concat(std::move(f), std::move(star)));
      continue;
    }
    if (c == '?') {
      // a? = (a|ε)
      if (stack.empty()) {
        return InvalidArgumentError(
            "Invalid regex at position {}: not enough operands for ?",
            postfix.find(c));
      }
      auto [fStart, fEnd] = top_and_pop_stack();
      auto s = new_state(State::Type::kIntermediate);
      auto acc = new_state(State::Type::kIntermediate);
      add_transition(s, fStart, '\0');
      add_transition(s, acc, '\0');
      add_transition(fEnd, acc, '\0');
      stack.emplace(s, acc);
      continue;
    }
    // stack.emplace(from_char(c)); // fallback
    AC_UNREACHABLE("Unimplemented operator in regex: '{}'", c);
    return UnimplementedError("Unhandled character in postfix regex: '{}'", c);
  }
  if (stack.size() != 1) {
    AC_UNREACHABLE(
        "Internal Error: expression not fully reduced, stack size {}",
        stack.size());
    return InternalError("Internal Error: expression not fully reduced");
  }
  return {top_and_pop_stack()};
}
void NFA::finalize(Fragment &&frag) {
  const auto &[sid, eid] = frag;
  start_id = sid;
  accept_ids.emplace(eid);
  states[sid].type = State::Type::kStart;
  // only one accept state in this algo
  states[eid].type = State::Type::kAccept;
}
auto NFA::construxt_from_regex(const std::string_view sv) {
  init_input_alphabet(sv);

  const auto preprocessed = preprocess_regex(sv);
  auto maybe_postfix = to_postfix(preprocessed);
  if (!maybe_postfix)
    return maybe_postfix.as_status();

  auto maybe_frag = build_graph(*maybe_postfix);
  if (!maybe_frag)
    return maybe_frag.as_status();

  finalize(*std::move(maybe_frag));

  return OkStatus();
}
StatusOr<NFA> NFA::FromRegex(const std::string_view sv) {
  if (sv.empty())
    return {};

  NFA nfa;

  auto s = nfa.construxt_from_regex(sv);

  if (!s)
    return s;
  return {std::move(nfa)};
}
#pragma endregion NFA
#pragma region DFA
auto DFA::_dot_transition_details() const {
  std::string dot;
  for (const auto &[dfa_id, nfa_states] : mapping) {
    std::string nfa_states_str;
    for (const auto nfa_state_id : nfa_states) {
      nfa_states_str += Format("{}, ", nfa_state_id);
    }
    if (!nfa_states_str.empty()) {
      nfa_states_str.erase(nfa_states_str.size() - 2); // remove last ", "
    }
    dot += Format("{0} [label=\"{0}\n"
                  "({1})\"];  \n",
                  dfa_id,
                  nfa_states_str);
  }
  return dot;
}
auto DFA::_to_dot_impl(const FormatPolicy policy) const -> std::string {
  auto dot = Format(unformatted_header, "DFA", start_id);
  dot += _dot_states(states);

  // not only output, but also add notation for the NFA states represented
  if (policy == FormatPolicy::kDetailed)
    dot += _dot_transition_details();

  dot += "\n";
  dot += _dot_transitions();
  dot += "}\n";
  return dot;
}
void DFA::process_start_state(const NFA &nfa, Key2IdTy &key_to_id) {
  auto start_subset = nfa.epsilon_closure(nfa.start_id);

  start_id = 0;
  states.emplace(0, State::Type::kStart);
  key_to_id.emplace(start_subset, 0);

  mapping.emplace(0, std::move(start_subset));
}
void DFA::process_transitions(const NFA &nfa, Key2IdTy &key_to_id) {
  for (size_t cur = 0; cur < mapping.size(); ++cur) {
    for (const char a : input_alphabet) {
      std::unordered_set<size_t> move_set;
      for (const auto s : mapping[cur]) {
        const auto it = nfa.states.find(s);
        AC_RUNTIME_ASSERT(
            it != nfa.states.end(),
            "Internal Error: NFA state {} not found during DFA construction",
            s)
        for (const auto e : it->second.edges) {
          if (/* !e.is_epsilon() && */ e.symbol == a)
            move_set.emplace(e.target_id);
        }
      }
      if (move_set.empty())
        continue;

      nfa.epsilon_closure(move_set);
      size_t to_id;
      if (auto [it, inserted] = key_to_id.emplace(move_set, npos); inserted) {
        to_id = new_state(State::Type::kIntermediate);
        it->second = to_id;
        mapping.emplace(to_id, std::move(move_set));
      } else {
        to_id = it->second;
      }
      add_transition(cur, to_id, a);
    }
  }
}
void DFA::finalize(const NFA &nfa) {
  for (auto dfa_id = 0ull; dfa_id < mapping.size(); ++dfa_id) {
    if (std::ranges::none_of(nfa.accept_ids, [&](const size_t nfa_accept) {
          return mapping[dfa_id].contains(nfa_accept);
        }))
      continue;

    accept_ids.emplace(dfa_id);
    states[dfa_id].type =
        (dfa_id == start_id)
            ? State::Type::kStartAndAccept // both start and accept
            : State::Type::kAccept;
  }
}
void DFA::construct_from_nfa(const NFA &nfa) {

  Key2IdTy key_to_id;

  process_start_state(nfa, key_to_id);
  process_transitions(nfa, key_to_id);
  finalize(nfa);

  AC_DEBUG_ONLY(
      // check determinism(each edge symbol unique per state)
      for (const auto &s : states | std::views::values) {
        std::unordered_set<char> seen;
        for (const auto &[to_id, symbol] : s.edges) {
          AC_RUNTIME_ASSERT(!seen.contains(symbol),
                            "Non-deterministic DFA found! Should not happen.");
          seen.emplace(symbol);
        }
      })
}
StatusOr<DFA> DFA::FromNFA(const NFA &nfa) {
  DFA dfa;
  dfa.input_alphabet = nfa.input_alphabet;

  if (nfa.empty())
    return {std::move(dfa)};

  dfa.construct_from_nfa(nfa);

  return {std::move(dfa)};
}
auto DFA::initial_partition(const StatesTy &states) -> PartitionsTy {
  PartitionsTy partitions;
  PartitionTy accept_states, non_accept_states;

  for (const auto &[sid, state] : states) {
    if (state.type == State::Type::kAccept)
      accept_states.emplace(sid);
    else
      non_accept_states.emplace(sid);
  }
  AC_RUNTIME_ASSERT(!accept_states.empty(), "");
  partitions.emplace_back(std::move(accept_states));

  if (!non_accept_states.empty())
    partitions.emplace_back(std::move(non_accept_states));

  return partitions;
}
void DFA::rebuild_from_partitions(const PartitionsTy &partitions) {
  // mapping: old state -> partition it belongs to
  std::unordered_map<size_t, size_t> oldstate_to_partition;
  for (auto i = 0ull; i < partitions.size(); ++i) {
    for (const auto sid : partitions[i]) {
      oldstate_to_partition[sid] = i;
    }
  }

  StatesTy new_states;
  std::unordered_set<size_t> new_accept_ids;
  auto new_start_id = npos;

  for (size_t pid = 0; pid < partitions.size(); ++pid) {
    const auto &part = partitions[pid];
    // randomly choose one
    const auto chosen_id = *part.begin();
    const auto &chosen_state = states[chosen_id];

    const auto isKeyContained = [&part](auto &&keyval) constexpr {
      return part.contains(keyval);
    };

    State newState; // id is pid in new states

    AC_DEBUG_ONLY(auto oneshot = false;)
    if (isKeyContained(start_id)) {
      AC_DEBUG_ONLY(AC_RUNTIME_ASSERT(oneshot == false,
                                      "should only have one start state")
                        oneshot = true);
      new_start_id = pid;
      newState.type = chosen_state.type;
    }

    if (std::ranges::any_of(accept_ids, isKeyContained)) {
      new_accept_ids.emplace(pid);
      if (pid == new_start_id) {
        newState.type = State::Type::kStartAndAccept;
      } else {
        newState.type = State::Type::kAccept;
      }
    } else if (pid == new_start_id) {
      newState.type = State::Type::kStart;
    }

    std::unordered_map<char, size_t> symbol_to_target;
    for (const auto edge : chosen_state.edges) {
      // target partition id is the new target state id.
      const auto target_partition_id = oldstate_to_partition[edge.target_id];
      newState.edges.emplace(target_partition_id, edge.symbol);
    }

    new_states.emplace(pid, std::move(newState));
  }

  states = std::move(new_states);
  start_id = new_start_id;
  accept_ids = std::move(new_accept_ids);
}
void DFA::hopcroft(PartitionsTy &partitions) const {
  // init W to { F, Q / F }.
  // NOTE:
  // initialize only to { F } according to some references is WRONG here(?)
  std::vector<size_t> worklist;
  worklist.emplace_back(0);
  worklist.emplace_back(1);

  while (!worklist.empty()) {
    const size_t A_idx = worklist.back();
    worklist.pop_back();
    const auto A = partitions[A_idx]; // copy, we need the snapshot of A

    for (const auto c : input_alphabet) {
      PartitionTy S;

      // S = { q \in Q | \delta(q, c) \in A }
      for (const auto &[sid, q] : states) {
        const auto &edges = q.edges;
        const auto it = std::ranges::find(edges, c, &Transition::symbol);
        if (it == edges.end())
          continue;
        if (A.contains(it->target_id))
          S.emplace(sid);
      }
      if (S.empty())
        continue;

      // modify partitions safely via interating through index.
      // precondition see below
      for (auto Y_idx = 0ull; Y_idx < partitions.size(); ++Y_idx) {
        const auto &Y = partitions[Y_idx];

        // Y1 = Y \cap S, Y2 = Y \setminus S ( cap: ∩ set minus/diff: \ )
        PartitionTy Y1;
        PartitionTy Y2;
        Y1.reserve(std::ranges::min(Y.size(), S.size()));
        Y2.reserve(Y.size());

        for (const auto state_idx : Y) {
          if (S.contains(state_idx))
            Y1.emplace(state_idx);
          else
            Y2.emplace(state_idx);
        }

        if (Y1.empty() || Y2.empty())
          continue; // no split, precondition not satisfied

        // partitions[i] := Y1(replaces Y)
        partitions[Y_idx] = std::move(Y1);
        const size_t new_index = partitions.size();
        //  partition append Y2
        partitions.emplace_back(std::move(Y2));

        // If Y (index i) is in worklist, replace that occurrence with both i
        // and new_index. Otherwise, add the smaller of the two to worklist.
        if (const auto it_w = std::ranges::find(worklist, Y_idx);
            it_w != worklist.end()) {
          // replace the first occurrence of i with i (already) and add
          // new_index (Y should be unique, currently it is, but not sure in the
          // future.)
          AC_DEBUG_BLOCK {
            const auto it_last =
                std::ranges::find_last(worklist, Y_idx).begin();
            AC_RUNTIME_ASSERT(it_last == it_w, "id shall be unique")
            *it_w = Y_idx; // no-op, just keeps intent clear
          };
          worklist.emplace_back(new_index);
        } else {
          // add the smaller of the two sets
          if (partitions[Y_idx].size() < partitions[new_index].size())
            worklist.emplace_back(Y_idx);
          else
            worklist.emplace_back(new_index);
        }
      }
    }
  }
}
DFA::IndexSetTy DFA::get_transition_signature(const PartitionsTy &partitions,
                                              const State &s) const {
  // signature of the state sid based on its transitions
  IndexSetTy dest_set;
  dest_set.reserve(s.edges.size());

  for (const auto symbol : input_alphabet) {

    // find the transition for the current symbol
    const auto edge_it =
        std::ranges::find(s.edges, symbol, &Transition::symbol);

    if (edge_it == s.edges.end())
      // no transition exists for this symbol, skip to the next symbol
      continue;

    // found a transition from states[sid] to states[target_id]
    const auto destination_id = edge_it->target_id;

    // this variable denotes which partition states[target_id] belongs to.
    const auto dest_pid =
        std::ranges::find_if(partitions, [destination_id](const auto &p) {
          return p.contains(destination_id);
        });

    AC_RUNTIME_ASSERT(dest_pid != partitions.end(), "should not happen")

    // add the partition index to the signature of the current state.
    dest_set.emplace(std::ranges::distance(partitions.begin(), dest_pid));
  }

  return dest_set;
}
auto DFA::split(const PartitionsTy &partitions, const PartitionTy &part) const {
  // map to group states by their transition signatures
  std::unordered_map<IndexSetTy, PartitionTy, IndexSetHasher> groups;

  // add the current state to the group corresponding to its signature.
  std::ranges::for_each(part, [&](auto &&sid) {
    const auto &s = states.at(sid);
    auto sig = get_transition_signature(partitions, s);
    groups[std::move(sig)].emplace(sid);
  });

  return groups;
}
void DFA::moore(PartitionsTy &partitions) const {
  bool changed;
  do {
    changed = false;
    PartitionsTy new_partitions;

    for (const auto &part : partitions) {
      if (part.size() == 1) {
        // can't split an atom set
        new_partitions.emplace_back(part);
        continue;
      }

      // mapping: states -> partition
      auto groups = split(partitions, part);

      // a part was splited, so it's changed
      if (groups.size() > 1)
        changed = true;

      // this part -> several parts or unchanged
      new_partitions.append_range(std::move(groups) | std::views::values);
    }

    partitions = std::move(new_partitions);
  } while (changed);
}
#pragma endregion DFA
} // namespace accat::cp
