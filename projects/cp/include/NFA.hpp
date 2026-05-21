#pragma once

#include <auxilia/defines.hpp>
#include <auxilia/auxfwd.hpp>

#include "Automaton.hpp"

namespace auxilia {
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
class NFA : details::AutomatonBase {
  using MyBase = details::AutomatonBase;
  friend MyBase;
  friend class DFA;

public:
  using MyBase::test;
  using MyBase::to_dot;
  using MyBase::to_string;

private:
  inline static std::string preprocess_regex(std::string_view);

  // Shunting-Yard Algorithm, ref:
  // https://en.wikipedia.org/wiki/Shunting_yard_algorithm#The_algorithm_in_detail
  // I choose not to use recursive descent here,
  // for I'm used it too often
  inline static auxilia::StatusOr<std::string> to_postfix(std::string_view);
  inline std::string _to_dot_impl(auxilia::FormatPolicy) const;
  inline void init_input_alphabet(std::string_view);
  inline auto build_graph(std::string_view) -> auxilia::StatusOr<Fragment>;
  inline void finalize(Fragment &&);
  inline auto construxt_from_regex(std::string_view);

public:
  // McNaughton-Yamada-Thompson algorithm
  static auxilia::StatusOr<NFA> FromRegex(std::string_view sv);
};

} // namespace accat::cp
