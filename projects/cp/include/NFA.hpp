#pragma once

#include <accat/auxilia/defines.hpp>
#include <accat/auxilia/auxfwd.hpp>

#include "Automaton.hpp"

namespace accat::auxilia {
enum class FormatPolicy : uint8_t;
}

EXPORT_AUXILIA
namespace accat::cp {
class NFA : details::_automaton_base {
  using MyBase = details::_automaton_base;
  friend MyBase;
  friend class DFA;

public:
  using MyBase::test;
  using MyBase::to_dot;
  using MyBase::to_string;

private:
  static std::string preprocess_regex(std::string_view regex);

  // Shunting-Yard Algorithm, ref:
  // https://en.wikipedia.org/wiki/Shunting_yard_algorithm#The_algorithm_in_detail
  // I choose not to use recursive descent here,
  // for I'm used it too often
  static auxilia::StatusOr<std::string> to_postfix(std::string_view regex);
  auto _to_dot_impl(auxilia::FormatPolicy) const -> std::string;
  void init_input_alphabet(std::string_view sv);
  auxilia::StatusOr<Fragment> build_graph(std::string_view postfix);
  void finalize(Fragment &&frag);
  auto construxt_from_regex(std::string_view sv);

public:
  // McNaughton-Yamada-Thompson algorithm
  static auxilia::StatusOr<NFA> FromRegex(std::string_view sv);
};

} // namespace accat::cp
