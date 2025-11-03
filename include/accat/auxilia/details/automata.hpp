#pragma once

#include <stack>
#include "accat/auxilia/details/format.hpp"
#include "macros.hpp"

namespace accat::auxilia {
class NFA : Printable {
public:
  using size_type = size_t;
  using symbol_type = char;

  static constexpr inline auto npos = static_cast<size_type>(-1);

private:
  struct State;
  struct Transition;
  struct Fragment;

  static consteval symbol_type widen(const char c) {
    return static_cast<symbol_type>(c);
  }

  struct State {
    enum struct Type : unsigned short {
      kNone = 0, // intermediate state
      kStart = 1,
      kAccept = 2,
      kEmpty = kStart | kAccept
    } type = Type::kNone;
    using enum Type;
    size_type id = npos;
    std::vector<Transition> edges;
  };
  struct Transition {
    size_type target_id = npos;
    symbol_type symbol = widen('\0');
    bool is_epsilon() const { return symbol == widen('\0'); }
  };

  // partial NFA, used during construction
  struct Fragment {
    size_type start = npos;
    size_type end = npos;
  };
  static_assert(__is_aggregate(Transition) && __is_aggregate(State) &&
                    __is_aggregate(Fragment),
                "Shall be aggregate struct");

  std::vector<State> states;
  size_type start_id = npos;
  size_type accept_id = npos;

  size_type new_state(State::Type type = State::kNone) {
    size_type id = states.size();
    states.emplace_back(type, id);
    return id;
  }
  void add_transition(size_type from, size_type to, symbol_type symbol = '\0') {
    AC_PRECONDITION(from < states.size() && to < states.size(), "out of range")
    states[from].edges.emplace_back(to, symbol);
  }

  void epsilon_closure(std::unordered_set<size_type> &state_set) const {
    // todo: poor use of vec
    std::vector<size_type> stack(state_set.begin(), state_set.end());
    while (!stack.empty()) {
      size_type current = stack.back();
      stack.pop_back();
      for (const auto &edge : states[current].edges) {
        if (edge.is_epsilon() &&
            state_set.find(edge.target_id) == state_set.end()) {
          state_set.insert(edge.target_id);
          stack.push_back(edge.target_id);
        }
      }
    }
  }

private:
  NFA() {}

public:
  NFA(NFA &&other) { this->operator=(std::move(other)); }
  NFA &operator=(NFA &&other) {
    states = std::move(other.states);
    start_id = std::exchange(other.start_id, npos);
    accept_id = std::exchange(other.accept_id, npos);
    return *this;
  }

private:
  Fragment from_char(const symbol_type c) {
    size_type s = new_state(State::kNone);
    size_type a = new_state(State::kNone);
    add_transition(s, a, c);
    return {s, a};
  }

  Fragment concat(Fragment &&lhs, Fragment &&rhs) {
    add_transition(lhs.end, rhs.start, '\0');
    return {lhs.start, rhs.end};
  }

  Fragment union_operation(Fragment &&a, Fragment &&b) {
    size_type s = new_state(State::kNone);
    size_type acc = new_state(State::kNone);

    add_transition(s, a.start, '\0');
    add_transition(s, b.start, '\0');

    add_transition(a.end, acc, '\0');
    add_transition(b.end, acc, '\0');
    return {s, acc};
  }

  Fragment kleene_star(Fragment &&f) {
    size_type s = new_state(State::kNone);
    size_type acc = new_state(State::kNone);

    add_transition(s, f.start, '\0');
    add_transition(s, acc, '\0');

    add_transition(f.end, f.start, '\0');
    add_transition(f.end, acc, '\0');
    return {s, acc};
  }
  void finalize(Fragment &&frag) {
    AC_PRECONDITION(start_id == std::numeric_limits<size_type>::max() &&
                        accept_id == std::numeric_limits<size_type>::max(),
                    "NFA already finalized")
    AC_PRECONDITION(frag.start != npos && frag.end != npos,
                    "Invalid fragment to finalize;"
                    "if it's from empty regex, handle separately")
    start_id = frag.start;
    accept_id = frag.end;
    states[start_id].type = State::Type::kStart;
    states[accept_id].type = State::Type::kAccept;
  }

public:
  auto to_string(FormatPolicy = FormatPolicy::kDefault) const -> string {
    if (start_id == std::numeric_limits<size_type>::max() &&
        accept_id == std::numeric_limits<size_type>::max())
      return "<empty>";

#ifdef _WIN32
    SetConsoleOutputCP(65001);
#endif

    string result;
    for (const auto &s : states) {
      result += format("State {}", s.id);
      if (s.type == State::Type::kStart)
        result += " (start)";
      if (s.type == State::Type::kAccept)
        result += " (accept)";
      result += ":\n";
      for (const auto &e : s.edges) {
        if (e.is_epsilon())
          result += format("  --ε--> {}\n", e.target_id);
        else
          result += format("  --{}--> {}\n", e.symbol, e.target_id);
      }
    }
    return result;

    // // depth-first traversal to print reachable states only
    // string result;
    // std::unordered_set<size_type> visited;

    // [&](this auto &&self, size_type state_id) {
    //   if (visited.find(state_id) != visited.end())
    //     return;
    //   visited.insert(state_id);
    //   const auto &s = states[state_id];
    //   result += format("State {}", s.id);
    //   if (s.type == State::Type::kStart)
    //     result += " (start)";
    //   if (s.type == State::Type::kAccept)
    //     result += " (accept)";
    //   result += ":\n";
    //   for (const auto &e : s.edges) {
    //     if (e.is_epsilon())
    //       result += format("  --ε--> {}\n", e.target_id);
    //     else
    //       result += format("  --{}--> {}\n", e.symbol, e.target_id);
    //     self(e.target_id);
    //   }
    // }(start_id);

    // return result;
  }
  bool test(std::string_view input) {
    if (start_id == std::numeric_limits<size_type>::max() &&
        accept_id == std::numeric_limits<size_type>::max())
      return input.empty() ? true : false;

    std::unordered_set<size_type> current_states = {start_id};
    epsilon_closure(current_states);

    for (const char c : input) {
      std::unordered_set<size_type> next_states;
      for (size_type state_id : current_states) {
        for (const auto &edge : states[state_id].edges) {
          if (!edge.is_epsilon() && edge.symbol == c) {
            next_states.insert(edge.target_id);
          }
        }
      }
      if (next_states.empty())
        return false;
      epsilon_closure(next_states);
      current_states = std::move(next_states);
    }

    return current_states.find(accept_id) != current_states.end();
  }

private:
  static std::string preprocess_regex(std::string_view regex) {
    std::string result;
    // insert `.` for concatenation between
    //        char or `)` `*` `+` `?` followed by char or `(`
    // (here char means non-op char ^^^)
    result.resize_and_overwrite(
        regex.size() * 2, [&](char *out, size_t capacity) {
          constexpr auto match = [](char c, string_view chars) {
            return chars.find(c) != string_view::npos;
          };
          size_t j = 0;
          for (size_t i = 0; i < regex.size(); ++i) {
            char c = regex[i];
            out[j++] = c;

            if (i + 1 < regex.size()) {
              char next = regex[i + 1];
              if (!match(c, "|(") && !match(next, "|)*+?")) {
                out[j++] = '.';
              }
            }
          }
          return j; // final length
        });
    return result;
  }

  // Shunting-Yard Algorithm
  static std::string to_postfix(std::string_view regex) {
    std::string postfix;
    std::string op_stack;
    constexpr auto is_operator = [](symbol_type c) {
      return c == '|' || c == '*' || c == '.' || c == '+' || c == '?';
    };
    constexpr auto precedence = [](symbol_type op) {
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
    };
    for (auto c : regex) {
      if (c == '(') {
        op_stack.push_back(c);
        continue;
      }
      if (c == ')') {
        while (!op_stack.empty() && op_stack.back() != '(') {
          postfix += op_stack.back();
          op_stack.pop_back();
        }
        if (!op_stack.empty())
          op_stack.pop_back(); // pop '('
        continue;
      }
      if (is_operator(c)) {
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

    // while (!op_stack.empty()) {
    //   postfix += op_stack.back();
    //   op_stack.pop_back();
    // }
    // ^^^ native way
    // vvv failed and non-conforming
    // postfix += std::ranges::reverse(op_stack)._Unwrapped();

    // worked, and fancy :P vvv
    std::ranges::reverse_copy(op_stack, std::back_inserter(postfix));

    return postfix;
  }

public:
  // McNaughton-Yamada-Thompson algorithm
  static NFA FromRegex(std::string_view sv) {
    NFA nfa;
    if (sv.empty()) {
      nfa.states.emplace_back(State::Type::kEmpty, 0);
      return nfa;
    }

    auto preprocessed = preprocess_regex(sv);
    auto postfix = to_postfix(preprocessed);

    std::stack<Fragment> stack;
    auto top_and_pop_stack = [&stack]() {
      AC_RUNTIME_ASSERT(!stack.empty(), "Stack underflow")
      auto val = stack.top();
      stack.pop();
      return val;
    };

    for (char c : postfix) {
      if (isalnum(c)) {
        // regular character
        stack.emplace(nfa.from_char(c));
        continue;
      }
      if (c == '|') {
        AC_PRECONDITION(stack.size() >= 2,
                        "Invalid regex: not enough operands for |")
        auto b = top_and_pop_stack();
        auto a = top_and_pop_stack();
        stack.emplace(nfa.union_operation(std::move(a), std::move(b)));
        continue;
      }
      if (c == '.') {
        AC_PRECONDITION(stack.size() >= 2,
                        "Invalid regex: not enough operands for concat")
        auto b = top_and_pop_stack();
        auto a = top_and_pop_stack();
        stack.emplace(nfa.concat(std::move(a), std::move(b)));
        continue;
      }
      if (c == '*') {
        AC_PRECONDITION(!stack.empty(),
                        "Invalid regex: not enough operands for *")
        auto f = top_and_pop_stack();
        stack.emplace(nfa.kleene_star(std::move(f)));
        continue;
      }
      if (c == '+') {
        // a+ = aa*
        AC_PRECONDITION(!stack.empty(),
                        "Invalid regex: not enough operands for +")
        auto f = top_and_pop_stack();
        auto f_copy = // reuse same states
            Fragment{.start = f.start, .end = f.end};
        auto star = nfa.kleene_star(std::move(f_copy));
        stack.emplace(nfa.concat(std::move(f), std::move(star)));
        continue;
      }
      if (c == '?') {
        // a? = (a|ε)
        AC_PRECONDITION(!stack.empty(),
                        "Invalid regex: not enough operands for ?")
        auto f = top_and_pop_stack();
        size_type s = nfa.new_state(State::kNone);
        size_type acc = nfa.new_state(State::kNone);
        nfa.add_transition(s, f.start, '\0');
        nfa.add_transition(s, acc, '\0');
        nfa.add_transition(f.end, acc, '\0');
        stack.emplace(s, acc);
        continue;
      }
      AC_TODO_()
    }

    AC_RUNTIME_ASSERT(stack.size() == 1,
                      "Invalid regex: expression not fully reduced")

    nfa.finalize(std::move(stack.top()));

    return nfa;
  }
};
} // namespace accat::auxilia
