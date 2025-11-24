#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <optional>
#include <ranges>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

#include <accat/auxilia/auxilia.hpp>

#include "Lexing.hpp"
#include "accat/auxilia/details/config.hpp"
#include "Grammar.hpp"

namespace accat::cp {
using auxilia::epsilon;
using auxilia::Format;
using auxilia::FormatPolicy;
using auxilia::InvalidArgumentError;
using auxilia::OkStatus;
using auxilia::Println;
using auxilia::ResourceExhaustedError;
using auxilia::Status;
using auxilia::StatusOr;
using auxilia::UnimplementedError;
} // namespace accat::cp

namespace accat::cp {
#pragma region Piece
bool Grammar::Piece::nullable(Grammar *myGrammar) {
  // Only Piece::nullable_ should be modifiable
  if (nullable_)
    return *nullable_;

  namespace sr = std::ranges;
  namespace rv = std::ranges::views;

  cache_rhsElemNullable.reserve(rhs_.size());

  const auto isSymbolNullable = [&](auto &&sym) {
    auto &&ptrPiece = myGrammar->non_terminal(sym);
    AC_RUNTIME_ASSERT(ptrPiece != this, "infinite loop?")
    const auto res = ptrPiece && ptrPiece->nullable(myGrammar);
    cache_rhsElemNullable.emplace_back(res);
    return res;
  };

  const auto isRhsElemNullable = [&](auto &&rhsElem) {
    if (rhsElem.front() == epsilon) {
      AC_RUNTIME_ASSERT(rhsElem.size() == 1, "shall not happen")
      cache_rhsElemNullable.emplace_back(true);
      return true;
    }
    return sr::all_of(rhsElem | rv::as_const, isSymbolNullable);
  };
  return nullable_.emplace(sr::any_of(rhs_ | rv::as_const, isRhsElemNullable));
}
auto Grammar::Piece::to_string(FormatPolicy policy) const -> string_type {
  return (lhs_ + (" -> "))
      .append_range(rhs_ //
                    | std::ranges::views::transform([](auto &&alt) {
                        return alt | std::ranges::views::join_with(' ');
                      })                                                     //
                    | std::ranges::views::join_with(std::string_view(" | ")) //
                    // ^^^ workaround, pass const char* seems cause issue
      );
}
#pragma endregion Piece
#pragma region Helper
auto Grammar::_new_unique_non_terminal_name(const std::string_view origName,
                                            const char *prime) const
    -> string_type {
  string_type newName = origName.data();
  do {
    newName += prime;
  } while (non_terminal(newName));
  return newName;
}
#pragma endregion Helper
#pragma region Recurse
void Grammar::_immediate_left_recursion(Piece &A,
                                        Piece::rhs_t &&recRhsElems,
                                        Piece::rhs_t &&nonRecRhsElems) {
  auto prime = _new_unique_non_terminal_name(A.lhs_, "'");
  // nonTerminals_[prime] = pieces.size();

  // create new piece for A'
  Piece newPiece;
  newPiece.lhs_ = (prime);

  // A -> beta A'
  Piece::rhs_t new_A_rhs;
  for (auto &&beta : nonRecRhsElems | std::ranges::views::as_rvalue) {
    auto &betaAprime = new_A_rhs.emplace_back();
    betaAprime.reserve(beta.size() + 1);
    betaAprime.append_range(beta | std::ranges::views::as_rvalue);
    betaAprime.emplace_back(newPiece.lhs_);
  }
  A.rhs_ = std::move(new_A_rhs);

  // A' -> alpha A' | epsilion
  for (auto &&alpha : recRhsElems | std::ranges::views::as_rvalue) {
    auto &alphaAprime = newPiece.rhs_.emplace_back();
    alphaAprime.reserve(alpha.size() + 1);
    alphaAprime.assign_range(alpha | std::ranges::views::as_rvalue);
    alphaAprime.emplace_back(newPiece.lhs_);
  }
  newPiece.rhs_.emplace_back().emplace_back(epsilon);
  terminals_.emplace(epsilon);

  pieces_.emplace_back(std::move(newPiece));
}
bool Grammar::_analyze_left_recursion(Piece &A) {
  Piece::rhs_t nonRecRhsElems;
  Piece::rhs_t recRhsElems; // store alpha (without leading A)
  // for readability I choose not to remove braces and got alpha and beta as
  // aliases (, though I really want to write them one line).
  for (auto &&rhsElem : std::move(A.rhs_) | std::ranges::views::as_rvalue) {
    if (rhsElem.front() == A.lhs_) {
      // has left recursion
      auto &alpha = recRhsElems.emplace_back();
      alpha.assign_range(rhsElem | std::ranges::views::drop(1) |
                         std::ranges::views::as_rvalue);
    } else {
      auto &beta = nonRecRhsElems.emplace_back();
      beta.assign_range(rhsElem | std::ranges::views::as_rvalue);
    }
  }
  if (recRhsElems.empty()) {
    // no direct left recursion
    // note: A.rhs invalid for we marked it as xvalue previously,
    // so we shall move it back here.
    A.rhs_.assign_range(std::move(nonRecRhsElems) |
                        std::ranges::views::as_rvalue);
    return true;
  }

  if (nonRecRhsElems.empty())
    // A -> A
    return false;

  // create A'
  _immediate_left_recursion(
      A, std::move(recRhsElems), std::move(nonRecRhsElems));
  return true;
}
void Grammar::_indirect_left_recursion(Piece &A, const Piece &B) const {
  Piece::rhs_t new_rhs;
  for (auto &&rhsElem : std::move(A.rhs_) | std::ranges::views::as_rvalue) {
    AC_RUNTIME_ASSERT(!rhsElem.empty(), "should not happen")
    AC_STATIC_ASSERT(std::is_rvalue_reference_v<decltype(rhsElem)>);

    if ((rhsElem.front() == B.lhs_)) {
      // A -> B gamma  =>  substitute B -> delta into A
      for (const auto &delta : B.rhs_) {

        auto &combined = new_rhs.emplace_back();

        combined.reserve(delta.size() + (rhsElem.size() - 1));
        std::ranges::copy(delta, std::back_inserter(combined));

        std::ranges::copy(rhsElem                            //
                              | std::ranges::views::as_const //
                              | std::ranges::views::drop(1),
                          std::back_inserter(combined));
        // ^^^ as_const is for readability:
        // we prevent/didn't perform move here from xvalue `rhsElem`
        // (we may use rhsElem again during next iteration of delta)
      }
    } else {
      // keep untouched
      new_rhs.emplace_back(std::move(rhsElem));
    }
  }
  A.rhs_ = std::move(new_rhs);
}
#pragma endregion Recurse
#pragma region Parse
Status Grammar::preprocess(const std::vector<Token> &tokens) {
  if (tokens.size() == 1) {
    AC_RUNTIME_ASSERT(tokens.back().is_type(Token::Type::kEndOfFile))
    Println("nothing to do");
    return OkStatus();
  }
  if (string_type str; std::ranges::any_of(tokens, [&](const Token &token) {
        // allowed type in this Left Recursion Grammar.
        using enum Token::Type;
        const auto invalid = false; // workaround
                                    // std::ranges::all_of(Grammar::allowed,
                                    // [&token](auto &&type) {
                                    //   return !token.is_type(type);
                                    // });
        if (invalid)
          str.append(Format(R"('{1}' Contains non-allowed type '{0}')",
                            token.type_str(),
                            token.to_string(FormatPolicy::kBrief)));
        return invalid;
      })) {
    return UnimplementedError(std::move(str).append(
        "Grammar contains non-allowed token types; Validation failure."));
  }
  return OkStatus();
}
void Grammar::postprocess(std::ranges::common_range auto &&lines) {

  for (auto &&l : lines) {
    AC_STATIC_ASSERT(std::is_rvalue_reference_v<decltype(l)>);
    auto &piece = pieces_.emplace_back();
    piece.lhs_ = l.front().front().lexeme();

    for (auto &&chunk_view : l                                 // already rvalue
                                 | std::ranges::views::drop(1) //
                                 | std::ranges::views::as_rvalue) {
      piece.rhs_.emplace_back().assign_range(
          chunk_view | std::ranges::views::transform(
                           [](auto &&token) { return token.lexeme().data(); }));
    }
  }
  // 1. use for_each to enable possible vectorization(not really in this case)
  // 2. todo: improve this: scan through again, performance heavy?
  std::ranges::for_each(pieces_, [this](auto &&piece) {
    std::ranges::for_each(piece.rhs_, [this](auto &&rhsElem) {
      std::ranges::for_each(rhsElem, [this](auto &&str) {
        if (!this->non_terminal(str))
          this->terminals_.emplace(str);
      });
    });
  });
}
auto Grammar::do_parse(std::vector<Token> &&tokens) {
  using enum Token::Type;

  // I admit it's a bit messy here, but as the saying goes:
  // "If it works, don't touch it".

  string_type errorMsg_lazy;

  constexpr auto stmtSeperator = [](auto &&a, auto &&b) {
    return a.line() == b.line() // same line
                                // or different line but same statement
           || ((a.line() < b.line()) && (a.is_type(kBitwiseOr, kLeftArrow) ||
                                         b.is_type(kBitwiseOr, kLeftArrow)));
  };

  // first should be a single Identifier.
  const auto is_first_chunk_valid = [&](auto &&chunk) -> bool {
    if (chunk.size() != 1) {
      errorMsg_lazy += Format(
          "line {}: First chunk should only contain a single Identifier\n",
          chunk.front().line());
      return false;
    }
    if (chunk.front().is_type(kLeftArrow)) {
      errorMsg_lazy +=
          Format("line {}: First chunk should be an Identifier"
                 "(special characters as non-terminal is not accepted).\n",
                 chunk.front().line());
      return false;
    }
    return true;
  };

  // Second should be a single LeftArrow.
  // We don't include this chunk in the final output, so always return false.
  // Only append an error if it's malformed.
  const auto is_second_chunk_valid = [&](auto &&chunk) -> bool {
    if (chunk.size() != 1 || !chunk.front().is_type(kLeftArrow)) {
      errorMsg_lazy += Format(
          "line {}: Second chunk should only contain a single LeftArrow.\n",
          chunk.front().line());
    }
    return false;
  };

  // following chunks must not contain additional LeftArrow tokens.
  const auto is_following_chunk_valid = [&](auto &&chunk) -> bool {
    if (std::ranges::any_of(chunk,
                            [](auto &&t) { return t.is_type(kLeftArrow); })) {
      errorMsg_lazy += Format("line {}: Unexpected LeftArrow in rhs.\n",
                              chunk.front().line());
      return false;
    }
    // BitwiseOr indicates an alternative separator;
    // if present, it must be the only token in this chunk.
    if (std::ranges::any_of(chunk,
                            [](auto &&t) { return t.is_type(kBitwiseOr); })) {
      if (chunk.size() != 1) {
        errorMsg_lazy += Format("line {}: Unexpected BitwiseOr in rhs.\n",
                                chunk.front().line());
        return false;
      }
      // valid BitwiseOr, but we don't need it as a chunk to keep.
      return false;
    }
    // valid elem of rhs.
    return true;
  };

  const auto validSep = [&](auto &&pair) {
    auto &&[index, chunk] = pair;
    if (index == 0)
      // lhs
      return is_first_chunk_valid(chunk);
    if (index == 1)
      // the LeftArrow
      return is_second_chunk_valid(chunk);
    // rhs
    return is_following_chunk_valid(chunk);
  };
  constexpr auto segmentsSep = [](auto &&lhs, auto &&rhs) {
    return (lhs.is_type(kLeftArrow) == rhs.is_type(kLeftArrow)) &&
           (lhs.is_type(kBitwiseOr) == rhs.is_type(kBitwiseOr));
  };

  // is this necessary??? seems not during testing
  const auto tokSize = tokens.size() - 1;

  // transform_view<chunk_by_view<take_view<as_rvalue_view<owning_view<vector<Token>>>>,(l)>,(l)>
  auto lines =
      std::move(tokens) // xvalue to form a owning view rather than a ref view
      | std::ranges::views::as_rvalue     // mark token in tokens as rvalue
      | std::ranges::views::take(tokSize) // drop that kEndOfFile
      | std::ranges::views::chunk_by(stmtSeperator) // split by line
      | std::ranges::views::transform([&](auto &&line) {
          return line // now: lhs kLeftArrow rhs_elem1 kBitwiseOr rhs_elem2 ...
                 | std::ranges::views::chunk_by(segmentsSep) // result ^^^
                 | std::ranges::views::enumerate             // [index, chunk]
                 | std::ranges::views::filter(validSep) // extract lhs and rhs.
                 | std::ranges::views::values // drop views::enum's key.
                 | std::ranges::views::common // idk, prevent compat issues
              ;                    // returned: lhs rhs_elem1 rhs_elem2 ...
        })                         //
      | std::ranges::views::common // ditto
      ;

  postprocess(std::move(lines));

  // IMPORTANT: ranges::views are lazy-evaluated, so the validation is done
  // during the post_parse call(where we actually iterate through the views).
  // Thus we should check errorMsg after that;
  // otherwise errorMsg will always be empty here.
  if (!errorMsg_lazy.empty()) {
    return InvalidArgumentError(std::move(errorMsg_lazy));
  }

  return OkStatus();
}
#pragma endregion Parse
#pragma region Factor
void Grammar::do_factoring(const size_t index) {
  auto &piece = pieces_[index];
  using rhs_elem_t = Piece::rhs_elem_t;
  using rhs_t = Piece::rhs_t;

  auxilia::Trie<rhs_elem_t> trie;
  trie.assign_range(piece.rhs_);

  auto [lcpPath, lcpNode] = trie.longest_common_prefix();

  if (lcpPath.empty())
    // nothing to factor for this piece
    return;

  // collect suffixes of all productions sharing bestPrefix.
  auto APrimeRhs = trie.collect(lcpNode, epsilon);

  // filter original rhs: keep those not starting with bestPrefix.
  rhs_t newARhs;
  for (auto &&rhsElem : std::move(piece.rhs_) | std::ranges::views::as_rvalue) {
    AC_STATIC_ASSERT(std::is_rvalue_reference_v<decltype(rhsElem)>);
    if (!std::ranges::equal(
            lcpPath, rhsElem | std::ranges::views::take(lcpPath.size()))) {
      // deos not have the lcp
      newARhs.emplace_back(rhsElem);
    }
  }
  auto newName = _new_unique_non_terminal_name(piece.lhs_, "@");

  // add factored prefix + newName.
  // auto factoredPrefix = lcpPath;
  // factoredPrefix.emplace_back(newName);
  // newRhs.emplace_back(std::move(factoredPrefix));

  // ^^^ equivalent to vvv
  newARhs.emplace_back(std::move(lcpPath)).emplace_back(newName);
  piece.rhs_ = std::move(newARhs);

  // new piece for factored suffixes
  auto &newPiece = pieces_.emplace_back();
  newPiece.lhs_ = std::move(newName);

  // if suffix is single epsilon token, keep as-is;
  // else just the sequence.
  // Remove standalone epsilon marker if prefer empty production;
  // here we just keep it.
  newPiece.rhs_.append_range(std::move(APrimeRhs) |
                             std::ranges::views::as_rvalue);
}
#pragma endregion Factor
#pragma region FirstSet
Grammar::Piece::set_t
Grammar::_first_set_from_rhs_elem(std::ranges::common_range auto &&rhsElem) {
  namespace sr = std::ranges;

  AC_RUNTIME_ASSERT(!sr::empty(rhsElem))

  Piece::set_t partialFirstSet;

  auto f = sr::cbegin(rhsElem);

  // A -> ε
  if (*f == epsilon) {
    partialFirstSet.emplace(*f);
    // else, continue search?
    AC_RUNTIME_ASSERT(sr::next(f) == sr::cend(rhsElem),
                      "epsilon should be alone in production")
    return partialFirstSet;
  }

  if (terminal(*f)) {
    partialFirstSet.emplace(*f);
    AC_RUNTIME_ASSERT(*f != epsilon, "epsilon should have been handled earlier")
    return partialFirstSet;
  }

  for (; f != sr::cend(rhsElem); sr::advance(f, 1L)) {
    // terminal, add and stop
    if (terminal(*f)) {
      partialFirstSet.emplace(*f);
      AC_RUNTIME_ASSERT(*f != epsilon,
                        "epsilon should have been handled earlier")
      break;
    }

    // non-terminal, recursively compute its first set if needed
    auto piecePtr = non_terminal(*f);
    AC_RUNTIME_ASSERT(piecePtr != nullptr,
                      "symbol must be terminal or non-terminal")

    if (piecePtr->first_set_.empty())
      // if not empty it's calculated before
      _first_set_from_piece(*piecePtr);

    // add non-terminal's first set, excluding epsilon
    Piece::set_t setCopy = piecePtr->first_set_;
    setCopy.erase(auxilia::epsilon);
    partialFirstSet.insert_range(std::move(setCopy));

    if (!piecePtr->nullable(this))
      // not nullable, stop
      break;

    // nullable and the last symbol, add epsilon
    if (sr::next(f) == sr::cend(rhsElem)) {
      partialFirstSet.emplace(epsilon);
    }
  }
  return partialFirstSet;
}
void Grammar::_first_set_from_piece(Piece &A) {
  std::ranges::for_each(
      A.rhs_ | std::ranges::views::as_const, [&](auto &&rhsElem) {
        auto rhsElemFirstSet = _first_set_from_rhs_elem(rhsElem);
        A.cache_rhsElemFirst2Select.emplace_back(rhsElemFirstSet);
        A.first_set_.insert_range(std::move(rhsElemFirstSet));
      });
}
void Grammar::_compute_first_set() {
  // both lambda and bind_front is _Ugly :(
  std::ranges::for_each(pieces_,
                        std::bind_front(&Grammar::_first_set_from_piece, this));
}
#pragma endregion FirstSet
#pragma region FollowSet
void Grammar::_compute_follow_set() {
  // the first added non-terminal as start of the grammar
  pieces_.front().follow_set_.emplace("$");

  namespace rv = std::ranges::views;
  namespace sr = std::ranges;

  for (auto changed = true; changed;) {
    changed = false;

    for (auto &A : pieces_) {
      for (const auto &rhsElem : A.rhs_) {
        for (auto it = sr::begin(rhsElem); it != sr::end(rhsElem);
             sr::advance(it, 1L)) {
          const auto Bptr = non_terminal(*it);

          if (!Bptr)
            continue;

          Piece::set_t toAdd;

          if (sr::next(it) == sr::end(rhsElem)) {
            // no suffix, add follow(A) to follow(B)
            toAdd.insert_range(A.follow_set_ | rv::as_const);
          } else {

            // FIXME: do we really need to calculate it again?
            // build the suffix vector
            auto firstSuffix = _first_set_from_rhs_elem(
                sr::subrange(sr::next(it), sr::end(rhsElem)));

            if (auto maybeEpsilonIt = sr::find(firstSuffix, auxilia::epsilon);
                maybeEpsilonIt != sr::end(firstSuffix)) {
              // has epsilon, add follow(A)
              toAdd.insert_range(A.follow_set_ | rv::as_const);
              // add first(suffix) \ {epsilon}
              firstSuffix.erase(std::move(maybeEpsilonIt));
            }

            toAdd.insert_range(std::move(firstSuffix));
          }

          const auto oldSize = Bptr->follow_set_.size();
          Bptr->follow_set_.insert_range(std::move(toAdd));
          const auto newSize = Bptr->follow_set_.size();

          if (newSize != oldSize)
            changed = true;
        }
      }
    }
  }
}
#pragma endregion FollowSet
#pragma region Interface
auxilia::Printable::string_type Grammar::to_string(FormatPolicy) const {
  return pieces_                                                      //
         | std::ranges::views::transform(auxilia::Printable::Default) //
         | std::ranges::views::join_with('\n')                        //
         | std::ranges::to<string_type>()                             //
      ;
}
StatusOr<Grammar> Grammar::parse(std::vector<Token> &&tokens) {
  if (auto status = preprocess(tokens); !status)
    return {status};

  Grammar grammar;

  if (auto status = grammar.do_parse(std::move(tokens)); !status) {
    return {status};
  }

  return {std::move(grammar)};
}
Status Grammar::eliminate_left_recursion() {
  // only need to examine the original grammar, no need to inspect newly
  // generated one; newly generated is appended after the originals.
  const auto mySize = pieces_.size();
  for (size_t i = 0; i < mySize; ++i) {
    auto &A = pieces_[i];
    for (size_t j = 0; j < i; ++j) {
      auto &B = pieces_[j];
      _indirect_left_recursion(A, B);
    }
    if (!_analyze_left_recursion(A))
      return ResourceExhaustedError("infinite loop");
  }
  return OkStatus();
}
Grammar &Grammar::apply_left_factorization() {

  for (auto changed = true; changed;) {
    changed = false;
    // here I just append new piece, hence it's the same `piece` with
    // `piece[index]`, however, to avoid dangling reference problem here we
    // did not use range-based for loop.
    for (auto index = 0ull; index < pieces_.size(); ++index) {
      const auto before = pieces_[index].rhs_.size();
      do_factoring(index);
      if (pieces_[index].rhs_.size() != before) {
        changed = true;
      }
    }
  }

  return *this;
}
Grammar &Grammar::calculate_set() {
  _compute_first_set();
  _compute_follow_set();

  return *this;
}
bool Grammar::isLL1() {
  // only cache_rhsElemFirst2Select shall be edited
  for (auto &piece : pieces_) {
    if (piece.nullable_ == std::nullopt) {
      // not calculated
      piece.nullable(this);
    }
    Piece::set_t acc;
    for (auto index = 0ull; index < piece.rhs_.size(); ++index) {
      auto &select = piece.cache_rhsElemFirst2Select[index];
      select.erase(epsilon);
      if (piece.cache_rhsElemNullable[index] == true) {
        select.insert_range(piece.follow_set_);
      }
      if (std::ranges::any_of(select,
                              [&](auto &&sym) { return acc.contains(sym); }))
        return false;
      else
        acc.insert_range(select);
    }
  }
  return true;
}
#pragma endregion Interface
} // namespace accat::cp
