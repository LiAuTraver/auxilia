#include "Grammar.hpp"

#include "accat/auxilia/base/format.hpp"

namespace accat::cp {
namespace sr = std::ranges;
namespace rv = std::ranges::views;
using auxilia::FormatPolicy;
} // namespace accat::cp
#pragma region Piece
namespace accat::cp {
bool Grammar::Piece::nullable(Grammar *myGrammar) {
  // Only Piece::nullable_ should be modifiable
  if (nullable_)
    return *nullable_;

  cache_rhsElemNullable.reserve(rhs_.size());

  const auto isSymbolNullable = [&](auto &&sym) {
    auto &&ptrPiece = myGrammar->non_terminal(sym);
    AC_RUNTIME_ASSERT(ptrPiece != this, "infinite loop?")
    return ptrPiece && ptrPiece->nullable(myGrammar);
  };

  const auto isRhsElemNullable = [&](auto &&rhsElem) {
    if (rhsElem.front() == NilMarker) {
      AC_RUNTIME_ASSERT(rhsElem.size() == 1, "shall not happen")
      cache_rhsElemNullable.emplace_back(true);
      return true;
    }
    // bitref -> bool
    return static_cast<bool>(cache_rhsElemNullable.emplace_back(
        sr::all_of(rhsElem | rv::as_const, isSymbolNullable)));
  };
  // not sure, subtle bug? any_of would eagerly return once one of the elem
  // satisfies the cond
  return nullable_.emplace(sr::any_of(rhs_ | rv::as_const, isRhsElemNullable));
}
bool Grammar::Piece::endable() const { return follow_set_.contains(EndMarker); }
auto Grammar::Piece::to_string(FormatPolicy policy) const -> string_type {
  return (lhs_ + (" -> "))
      .append_range(
          rhs_ //
          | std::ranges::views::transform([](auto &&alt) {
              return alt | std::ranges::views::join_with(' ');
            })                                                     //
          | std::ranges::views::join_with(std::string_view(" | ")) //
          // ^^^ string_view is a workaround, pass const char* seems cause issue
      );
}
} // namespace accat::cp
#pragma endregion Piece
