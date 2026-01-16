#include <stack>
#include <string>

#include "Automaton.hpp"
#include "NFA.hpp"

#include "accat/auxilia/status/Status.hpp"
#include "accat/auxilia/status/StatusOr.hpp"

#pragma region Common

namespace accat::cp {
using auxilia::Format;
using auxilia::FormatPolicy;
using auxilia::InternalError;
using auxilia::InvalidArgumentError;
using auxilia::npos;
using auxilia::OkStatus;
using auxilia::Status;
using auxilia::StatusOr;
using auxilia::UnimplementedError;
} // namespace accat::cp
namespace accat::cp {
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

#pragma region NFA
namespace accat::cp {

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

  return OkStatus(std::move(postfix));
}
auto NFA::_to_dot_impl(const FormatPolicy) const {
  auto dot = Format(unformatted_header, "NFA", start_id);

  dot += _dot_states(states);

  dot += "\n";
  dot += _dot_transitions();
  dot += "}\n";
  return dot;
}
void NFA::init_input_alphabet(const std::string_view sv) {
  input_alphabet = sv | std::views::filter(std::not_fn(MyBase::is_operator)) |
                   std::ranges::to<std::string>();
  std::ranges::sort(input_alphabet);

  auto r = std::ranges::unique(input_alphabet);
  input_alphabet.erase(r.begin(), r.end());
}
auto NFA::build_graph(const std::string_view postfix) -> StatusOr<Fragment> {
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
    ac_unreachable("Unimplemented operator in regex: '{}'", c);
    return UnimplementedError("Unhandled character in postfix regex: '{}'", c);
  }
  if (stack.size() != 1) {
    ac_unreachable(
        "Internal Error: expression not fully reduced, stack size {}",
        stack.size());
    return InternalError("Internal Error: expression not fully reduced");
  }
  return OkStatus(std::move(stack.top()));
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
    return maybe_postfix.rvalue().as_status();

  auto maybe_frag = build_graph(*maybe_postfix);
  if (!maybe_frag)
    return maybe_frag.rvalue().as_status();
  finalize(*std::move(maybe_frag));

  return OkStatus();
}
StatusOr<NFA> NFA::FromRegex(const std::string_view sv) {
  if (sv.empty())
    return {};

  NFA nfa;

  if (auto s = nfa.construxt_from_regex(sv); !s)
    return {std::move(s)};

  return OkStatus(std::move(nfa));
}
} // namespace accat::cp
#pragma endregion NFA
