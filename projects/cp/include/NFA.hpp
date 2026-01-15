#pragma once

#include <accat/auxilia/defines.hpp>
#include <accat/auxilia/auxfwd.hpp>

#include "Automaton.hpp"

namespace accat::auxilia {
enum class FormatPolicy : uint8_t;
}

EXPORT_AUXILIA
namespace accat::cp {
/// @brief Nondeterministic Finite Automaton
///
/// Similar to DFA, but allows multiple transitions for the same input symbol
/// (in other words, the transition function δ: Q × (Σ ∪ {ε}) → P(Q) can have
/// ε-transitions)
///
/// @copydoc DFA
class NFA : details::_automaton_base {
  using MyBase = details::_automaton_base;
  friend MyBase;
  friend class DFA;

public:
  using MyBase::test;
  using MyBase::to_dot;
  using MyBase::to_string;

private:
  static std::string preprocess_regex(std::string_view);

  // Shunting-Yard Algorithm, ref:
  // https://en.wikipedia.org/wiki/Shunting_yard_algorithm#The_algorithm_in_detail
  // I choose not to use recursive descent here,
  // for I'm used it too often
  static auxilia::StatusOr<std::string> to_postfix(std::string_view);
  auto _to_dot_impl(auxilia::FormatPolicy) const;
  void init_input_alphabet(std::string_view);
  auto build_graph(std::string_view) -> auxilia::StatusOr<Fragment>;
  void finalize(Fragment &&);
  auto construxt_from_regex(std::string_view);

public:
  // McNaughton-Yamada-Thompson algorithm
  static auxilia::StatusOr<NFA> FromRegex(std::string_view sv);
};

} // namespace accat::cp
