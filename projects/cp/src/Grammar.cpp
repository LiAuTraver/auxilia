#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <optional>
#include <ranges>
#include <stack>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

#include <accat/auxilia/auxilia.hpp>
#include <accat/auxilia/defines.hpp>

#include "Lexing.hpp"
#include "Grammar.hpp"

#pragma region Helper
namespace accat::cp {
namespace sr = std::ranges;
namespace rv = std::ranges::views;
using auxilia::Format;
using auxilia::FormatPolicy;
using auxilia::InvalidArgumentError;
using auxilia::LexError;
using auxilia::OkStatus;
using auxilia::Println;
using auxilia::ResourceExhaustedError;
using auxilia::Status;
using auxilia::StatusOr;
using auxilia::UnavailableError;
using auxilia::UnimplementedError;
static constexpr auto isNil(const std::string &ptr) noexcept {
  return auxilia::is_epsilon(ptr.data());
}
static constexpr auto NotNil(const std::string &ptr) noexcept {
  return not auxilia::is_epsilon(ptr.data());
}
auto Grammar::_new_unique_non_terminal_name(const std::string_view origName,
                                            const char *prime) const
    -> string_type {
  string_type newName = origName.data();
  do {
    newName += prime;
  } while (non_terminal(newName));
  return newName;
}
} // namespace accat::cp
#pragma endregion Helper

namespace accat::cp {
#pragma region Recurse
void Grammar::_immediate_left_recursion(Piece &A,
                                        Piece::rhs_t &&recRhsElems,
                                        Piece::rhs_t &&nonRecRhsElems) {
  // create new piece for A'
  Piece newPiece;
  newPiece.lhs_ = _new_unique_non_terminal_name(A.lhs_, "'");

  // A -> beta A'
  Piece::rhs_t new_A_rhs;
  for (auto &&beta : nonRecRhsElems | rv::as_rvalue) {
    auto &betaAprime = new_A_rhs.emplace_back();
    betaAprime.reserve(beta.size() + 1);
    betaAprime.append_range(beta | rv::as_rvalue);
    betaAprime.emplace_back(newPiece.lhs_);
  }
  A.rhs_ = std::move(new_A_rhs);

  // A' -> alpha A' | epsilion
  for (auto &&alpha : recRhsElems | rv::as_rvalue) {
    auto &alphaAprime = newPiece.rhs_.emplace_back();
    alphaAprime.reserve(alpha.size() + 1);
    alphaAprime.assign_range(alpha | rv::as_rvalue);
    alphaAprime.emplace_back(newPiece.lhs_);
  }
  newPiece.rhs_.emplace_back().emplace_back(NilMarker);
  terminals_.emplace(NilMarker);

  pieces_.emplace_back(std::move(newPiece));
}
bool Grammar::_analyze_left_recursion(Piece &A) {
  Piece::rhs_t nonRecRhsElems;
  Piece::rhs_t recRhsElems; // store alpha (without leading A)
  // for readability I choose not to remove braces and got alpha and beta as
  // aliases (, though I really want to write them one line).
  for (auto &&rhsElem : std::move(A.rhs_) | rv::as_rvalue) {
    if (rhsElem.front() == A.lhs_) {
      // has left recursion
      auto &alpha = recRhsElems.emplace_back();
      alpha.assign_range(rhsElem | std::ranges::views::drop(1) | rv::as_rvalue);
    } else {
      auto &beta = nonRecRhsElems.emplace_back();
      beta.assign_range(rhsElem | rv::as_rvalue);
    }
  }
  if (recRhsElems.empty()) {
    // no direct left recursion
    // note: A.rhs invalid for we marked it as xvalue previously,
    // so we shall move it back here.
    A.rhs_.assign_range(std::move(nonRecRhsElems) | rv::as_rvalue);
    return true;
  }

  if (nonRecRhsElems.empty())
    // A -> A
    return false;

  // TODO: this is hard coded...
  // special case: A -> ε | A α
  // which represents zero or more repetitions of α
  // transform to right recursion: A -> α A | ε
  if (nonRecRhsElems.size() == 1 && nonRecRhsElems[0].size() == 1 &&
      isNil(nonRecRhsElems[0][0])) {
    // A -> ε | A α  =>  A -> α A | ε
    Piece::rhs_t new_rhs;
    for (auto &&alpha : recRhsElems | rv::as_rvalue) {
      auto &alphaA = new_rhs.emplace_back();
      alphaA.reserve(alpha.size() + 1);
      alphaA.assign_range(alpha | rv::as_rvalue);
      alphaA.emplace_back(A.lhs_); // right recursion
    }
    new_rhs.emplace_back().emplace_back(NilMarker);

    A.rhs_ = std::move(new_rhs);
    terminals_.emplace(NilMarker);
    return true;
  }

  // create A'
  _immediate_left_recursion(
      A, std::move(recRhsElems), std::move(nonRecRhsElems));
  return true;
}
void Grammar::_indirect_left_recursion(Piece &A, const Piece &B) const {
  Piece::rhs_t new_rhs;
  for (auto &&rhsElem : std::move(A.rhs_) | rv::as_rvalue) {
    contract_assert(!rhsElem.empty(), "should not happen")
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
#pragma region Process
Status Grammar::_preprocess(const std::vector<Token> &tokens) {
  if (tokens.size() == 1) {
    contract_assert(tokens.back().is_type(Token::Type::kEndOfFile))
    return auxilia::UnavailableError("nothing to do");
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
void Grammar::_postprocess(std::ranges::common_range auto &&lines) {

  for (auto &&l : lines) {
    AC_STATIC_ASSERT(std::is_rvalue_reference_v<decltype(l)>);
    auto &piece = pieces_.emplace_back();
    piece.lhs_ = l.front().front().lexeme();

    for (auto &&chunk_view : l                                 // already rvalue
                                 | std::ranges::views::drop(1) //
                                 | rv::as_rvalue) {
      piece.rhs_.emplace_back().assign_range(
          chunk_view                                          //
          | std::ranges::views::transform(&Token::lexeme_str) //
      );
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
auto Grammar::_do_process(std::vector<Token> &&tokens) {
  using enum Token::Type;

  string_type errorMsg_lazy;

  // Statements are separated by semicolons.
  // Everything before a semicolon is part of the same statement.
  // TODO: legacy, no need to use `chunk_by` now.
  constexpr auto stmtSep = [](auto &&a, auto &&b) {
    return !a.is_type(kSemicolon);
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
    // semicolon is the statement terminator
    if (std::ranges::any_of(chunk,
                            [](auto &&t) { return t.is_type(kSemicolon); })) {
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
           (lhs.is_type(kBitwiseOr) == rhs.is_type(kBitwiseOr)) &&
           (lhs.is_type(kSemicolon) == rhs.is_type(kSemicolon));
  };

  // is this necessary??? seems not during testing
  const auto tokSize = tokens.size() - 1;

  auto lines =
      std::move(tokens) // xvalue to form a owning view rather than a ref view
      | rv::as_rvalue   // mark token in tokens as rvalue
      | std::ranges::views::take(tokSize)     // drop that kEndOfFile
      | std::ranges::views::chunk_by(stmtSep) // split by statements
      | std::ranges::views::transform([&](auto &&line) {
          return line // now: lhs kLeftArrow rhs_elem1 kBitwiseOr rhs_elem2 ...;
                 | std::ranges::views::chunk_by(segmentsSep) // result ^^^
                 | std::ranges::views::enumerate             // [index, chunk]
                 | std::ranges::views::filter(validSep) // extract lhs and rhs.
                 | std::ranges::views::values // drop views::enum's key.
                 | std::ranges::views::common // idk, prevent compat issues
              ;                    // returned: lhs rhs_elem1 rhs_elem2 ...
        })                         //
      | std::ranges::views::common // ditto
      ;

  _postprocess(std::move(lines));

  // IMPORTANT: ranges::views are lazy-evaluated, so the validation is done
  // during the post_parse call(where we actually iterate through the views).
  // Thus we should check errorMsg after that;
  // otherwise errorMsg will always be empty here.
  if (!errorMsg_lazy.empty()) {
    return InvalidArgumentError(std::move(errorMsg_lazy));
  }

  return OkStatus();
}
#pragma endregion Process
#pragma region EBNF
void Grammar::_expand_ebnf_constructs() {
  // expand EBNF constructs to BNF before left recursion elimination
  // [X] -> X | ε
  // {X} -> X A' | ε where A' -> X A' | ε
  // (equivalent to A -> ε | A X after transformation)

  using enum Token::Type;

  const size_t originalSize = pieces_.size();

  // collect new pieces to add after iteration to avoid reallocation issues
  std::vector<Piece> newPieces;

  for (auto pieceIdx = 0ull; pieceIdx < originalSize; ++pieceIdx) {
    Piece::rhs_t newRhs;

    for (auto &&rhsElem : std::move(pieces_[pieceIdx].rhs_) | rv::as_rvalue) {
      // scan for EBNF constructs in this production
      bool hasEbnf = false;
      Piece::rhs_elem_t expandedProduction;

      for (auto i = 0ull; i < rhsElem.size(); ++i) {
        const auto &elem = rhsElem[i];

        // [X]
        if (elem == "[") {
          hasEbnf = true;
          // find matching ]
          auto j = i + 1;
          auto bracketDepth = 1ull;
          while (j < rhsElem.size() && bracketDepth > 0) {
            if (rhsElem[j] == "[")
              bracketDepth++;
            else if (rhsElem[j] == "]")
              bracketDepth--;
            j++;
          }

          if (bracketDepth != 0) {
            contract_assert(
                "unmatched brackets. should handle in the prev stage"
                "(unimplemented currently)")
            expandedProduction.emplace_back(elem);
            continue;
          }

          // extract content between [ and ]
          Piece::rhs_elem_t optionalContent;
          for (size_t k = i + 1; k < j - 1; ++k) {
            optionalContent.emplace_back(rhsElem[k]);
          }

          auto newName =
              _new_unique_non_terminal_name(pieces_[pieceIdx].lhs_, "_Opt");
          expandedProduction.emplace_back(newName);

          // A_Opt -> X | ε
          auto &newPiece = newPieces.emplace_back();
          newPiece.lhs_ = newName;
          newPiece.rhs_.emplace_back(std::move(optionalContent));
          newPiece.rhs_.emplace_back().emplace_back(NilMarker);
          terminals_.emplace(NilMarker);

          i = j - 1; // Skip past the ]
        }
        // {X} - zero or more
        else if (elem == "{") {
          hasEbnf = true;
          // Find matching }
          size_t j = i + 1;
          size_t braceDepth = 1;
          while (j < rhsElem.size() && braceDepth > 0) {
            if (rhsElem[j] == "{")
              braceDepth++;
            else if (rhsElem[j] == "}")
              braceDepth--;
            j++;
          }

          if (braceDepth != 0) {
            expandedProduction.emplace_back(elem);
            continue;
          }

          // extract content between { and }
          Piece::rhs_elem_t repeatContent;
          for (size_t k = i + 1; k < j - 1; ++k) {
            repeatContent.emplace_back(rhsElem[k]);
          }

          auto newName =
              _new_unique_non_terminal_name(pieces_[pieceIdx].lhs_, "_Rep");
          expandedProduction.emplace_back(newName);

          // new: A_Rep -> ε | A_Rep X
          // this will be transformed to right recursion: A_Rep -> X A_Rep | ε
          // by _analyze_left_recursion
          auto &newPiece = newPieces.emplace_back();
          newPiece.lhs_ = newName;
          newPiece.rhs_.emplace_back().emplace_back(NilMarker);
          auto &recursiveRhs = newPiece.rhs_.emplace_back();
          recursiveRhs.emplace_back(newName);
          recursiveRhs.append_range(repeatContent);
          terminals_.emplace(NilMarker);

          i = j - 1; // skip the }
        } else {
          expandedProduction.emplace_back(elem);
        }
      }

      if (hasEbnf) {
        newRhs.emplace_back(std::move(expandedProduction));
      } else {
        newRhs.emplace_back(std::move(rhsElem));
      }
    }

    pieces_[pieceIdx].rhs_ = std::move(newRhs);
  }

  pieces_.append_range(std::move(newPieces) | rv::as_rvalue);
}
#pragma endregion EBNF
#pragma region Factor
void Grammar::_do_factoring(const size_t index) {
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
  auto APrimeRhs = trie.collect(lcpNode, NilMarker);

  // filter original rhs: keep those not starting with bestPrefix.
  rhs_t newARhs;
  for (auto &&rhsElem : std::move(piece.rhs_) | rv::as_rvalue) {
    AC_STATIC_ASSERT(std::is_rvalue_reference_v<decltype(rhsElem)>);
    if (!std::ranges::equal(
            lcpPath, rhsElem | std::ranges::views::take(lcpPath.size()))) {
      // deos not have the lcp
      newARhs.emplace_back(rhsElem);
    }
  }
  auto newName = _new_unique_non_terminal_name(piece.lhs_, "@");

  // add factored prefix + newName.
  newARhs.emplace_back(std::move(lcpPath)).emplace_back(newName);
  piece.rhs_ = std::move(newARhs);

  // new piece for factored suffixes
  auto &newPiece = pieces_.emplace_back();
  newPiece.lhs_ = std::move(newName);

  // if suffix is single epsilon token, keep as-is;
  // else just the sequence.
  // here we just keep the standalone epsilon marker.
  newPiece.rhs_.append_range(std::move(APrimeRhs) | rv::as_rvalue);
}
#pragma endregion Factor
#pragma region FirstSet
auto Grammar::_first_set_from_rhs_elem(std::ranges::common_range auto &&rhsElem)
    -> Piece::set_t {

  contract_assert(!sr::empty(rhsElem))

  Piece::set_t partialFirstSet;

  auto f = sr::cbegin(rhsElem);

  // A -> ε
  if (*f == NilMarker) {
    partialFirstSet.emplace(*f);
    // else, continue search?
    contract_assert(sr::next(f) == sr::cend(rhsElem),
                    "epsilon should be alone in production")
    return partialFirstSet;
  }

  if (terminal(*f)) {
    partialFirstSet.emplace(*f);
    contract_assert(*f != NilMarker, "epsilon should have been handled earlier")
    return partialFirstSet;
  }

  for (; f != sr::cend(rhsElem); sr::advance(f, 1L)) {
    // terminal, add and stop
    if (terminal(*f)) {
      partialFirstSet.emplace(*f);
      contract_assert(*f != NilMarker,
                      "epsilon should have been handled earlier")
      break;
    }

    // non-terminal, recursively compute its first set if needed
    auto piecePtr = non_terminal(*f);
    contract_assert(piecePtr != nullptr,
                    "symbol must be terminal or non-terminal")

    // if not empty it's calculated before, handled in the function already
    _first_set_from_piece(*piecePtr);

    // add non-terminal's first set, excluding epsilon
    Piece::set_t setCopy = piecePtr->first_set_;
    setCopy.erase(NilMarker);
    partialFirstSet.insert_range(std::move(setCopy));

    if (!piecePtr->nullable(this))
      // not nullable, stop
      break;

    // nullable and the last symbol, add epsilon
    if (sr::next(f) == sr::cend(rhsElem)) {
      partialFirstSet.emplace(NilMarker);
    }
  }
  return partialFirstSet;
}
void Grammar::_first_set_from_piece(Piece &A) {
  // if already computed (cache_selectSet_ has entries), skip
  if (!A.first_set_.empty())
    return;

  std::ranges::for_each(A.rhs_ | rv::as_const, [&](auto &&rhsElem) {
    auto rhsElemFirstSet = _first_set_from_rhs_elem(rhsElem);
    A.cache_selectSet_.emplace_back(rhsElemFirstSet);
    A.first_set_.insert_range(std::move(rhsElemFirstSet));
  });
}
void Grammar::compute_first_set() {
  // both lambda and bind_front is _Ugly :(
  std::ranges::for_each(pieces_,
                        std::bind_front(&Grammar::_first_set_from_piece, this));
}
#pragma endregion FirstSet
#pragma region FollowSet
bool Grammar::_follow_set_from_rhs_elem(
    const Piece &A, std::ranges::common_range auto &&rhsElem) {
  auto changed = false;

  // use `std::ranges::meow` instead of member function is just for
  // consistency with `next`, `advance`, `subrange` etc.
  for (auto it = sr::begin(rhsElem); it != sr::end(rhsElem);
       sr::advance(it, 1L)) {

    const auto Bptr = non_terminal(*it);

    if (!Bptr)
      continue;

    Piece::set_t newBFollowSet;

    if (sr::next(it) == sr::end(rhsElem)) {
      // no suffix, add follow(A) to follow(B)
      newBFollowSet.insert_range(A.follow_set_ | rv::as_const);
    } else {

      // FIXME: do we really need to calculate it again?
      // build the suffix vector
      auto firstSuffix = _first_set_from_rhs_elem(
          sr::subrange(sr::next(it), sr::end(rhsElem)));

      if (auto maybeEpsilonIt = sr::find(firstSuffix, NilMarker);
          maybeEpsilonIt != sr::end(firstSuffix)) {
        // has epsilon, add follow(A)
        newBFollowSet.insert_range(A.follow_set_ | rv::as_const);
        // add first(suffix) \ {epsilon}
        firstSuffix.erase(std::move(maybeEpsilonIt));
      }

      newBFollowSet.insert_range(std::move(firstSuffix));
    }

    const auto oldSize = Bptr->follow_set_.size();
    Bptr->follow_set_.insert_range(std::move(newBFollowSet));
    const auto newSize = Bptr->follow_set_.size();

    if (newSize != oldSize)
      changed = true;
  }
  return changed;
}

void Grammar::compute_follow_set() {
  // the first added non-terminal as start of the grammar
  pieces_.front().follow_set_.emplace(EndMarker);

  for (auto changed = true; changed;) {
    changed = false;

    std::ranges::for_each(pieces_ | rv::as_const, [&](auto &&A) {
      std::ranges::for_each(A.rhs_ | rv::as_const, [&](auto &&rhsElem) {
        if (_follow_set_from_rhs_elem(A, rhsElem))
          changed = true;
      });
    });
  }
}
#pragma endregion FollowSet
#pragma region Interface
auto Grammar::to_string(FormatPolicy) const -> string_type {
  return pieces_                                                      //
         | std::ranges::views::transform(auxilia::Printable::Default) //
         | std::ranges::views::join_with('\n')                        //
         | std::ranges::to<string_type>()                             //
      ;
}
StatusOr<Grammar> Grammar::Process(string_type &&str) {
  auto maybeGrammar = Grammar::FromStr(std::forward<string_type>(str));
  if (!maybeGrammar)
    return {std::move(maybeGrammar).as_status()};

  auto grammar = *std::move(maybeGrammar);

  if (auto status = grammar.eliminate_left_recursion(); !status)
    return {std::move(status)};

  grammar.apply_left_factorization();
  grammar.compute_first_set();
  grammar.compute_follow_set();

  return {std::move(grammar)};
}
StatusOr<Grammar> Grammar::FromStr(string_type &&str) {
  auto maybeTokens = Lexer{std::forward<string_type>(str)}.lexAll_or_error();
  if (!maybeTokens)
    return InvalidArgumentError(std::move(maybeTokens).error());

  auto tokens = *std::move(maybeTokens);

  if (auto status = _preprocess(tokens); !status)
    return {status};

  Grammar grammar;

  if (auto status = grammar._do_process(std::move(tokens)); !status)
    return {status};

  grammar._expand_ebnf_constructs();

  return {std::move(grammar)};
}

Status Grammar::eliminate_left_recursion() {
  const auto mySize = pieces_.size();
  // only need to examine the original grammar, no need to inspect newly
  // generated one; newly generated is appended after the originals.
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
void Grammar::apply_left_factorization() {
  for (auto changed = true; changed;) {
    changed = false;
    // here I just append new piece, hence it's the same `piece` with
    // `piece[index]`, however, to avoid dangling reference problem here we
    // did not use range-based for loop.
    for (auto index = 0ull; index < pieces_.size(); ++index) {
      const auto before = pieces_[index].rhs_.size();
      _do_factoring(index);
      if (pieces_[index].rhs_.size() != before) {
        changed = true;
      }
    }
  }
}
bool Grammar::isLL1() {
  if (is_ll1_.has_value())
    return *is_ll1_;

  // only cache_selectSet_ shall be edited
  for (auto &piece : pieces_) {
    if (!piece.nullable_) {
      // not calculated
      piece.nullable(this);
    }
    Piece::set_t acc;
    for (auto index = 0ull; index < piece.rhs_.size(); ++index) {
      auto &select = piece.cache_selectSet_[index];

      select.erase(NilMarker);

      if (piece.cache_rhsElemNullable[index] == true)
        select.insert_range(piece.follow_set_);

      if (std::ranges::any_of(select,
                              [&](auto &&sym) { return acc.contains(sym); }))
        return is_ll1_.emplace(false);
      else
        acc.insert_range(select);
    }
  }
  return is_ll1_.emplace(true);
}
#pragma endregion Interface
#pragma region Parse
auto Grammar::_do_parse(std::ranges::common_range auto &&elems) const {
  std::stack<string_type> myStack;
  myStack.push(pieces_.front().lhs_);

  contract_assert(pieces_.front().follow_set_.contains(EndMarker))

  // construct a table for LL1 parsing
  static std::unordered_map<string_type,
                            std::unordered_map<string_type, Piece::rhs_elem_t>>
      table;
  if (table.empty())
    std::ranges::for_each(pieces_ | rv::as_const, [&](auto &&piece) {
      std::ranges::for_each(
          rv::zip(piece.cache_selectSet_, piece.rhs_), [&](auto &&pair) {
            auto &&[selectSet, rhs] = pair;
            std::ranges::for_each(selectSet, [&](auto &&terminalSymbol) {
              table[piece.lhs_].emplace(terminalSymbol, rhs);
            });
          });
    });

  for (const auto &token : elems) {
    using enum Token::Type;
    const string_type *identPtr;
    if constexpr (std::is_same_v<std::decay_t<decltype(token)>, elem_t>) {
      identPtr = &token;
    } else {
      if (token.is_type(kLexError))
        return LexError(token.error_message());

      if (token.is_type(kNumber))
        return UnavailableError(
            "Number as identifier is not available: at line {}, number {}",
            token.line(),
            token.number());

      if (token.is_type(kEndOfFile)) {
        if (myStack.empty())
          // parsed completely and successed.
          return OkStatus();
        // workaround for empty input
        if (myStack.size() == 1 && non_terminal(myStack.top()) &&
            *non_terminal(myStack.top())->nullable_)
          return OkStatus();

        return InvalidArgumentError("Unexpected end of input, expected '{}'",
                                    myStack.top());
      }

      identPtr = &token.lexeme_str();
    }
    auto &&ident = *identPtr;

    if (non_terminal(ident))
      return InvalidArgumentError(
          "Non-terminal identifier {} "
          "should not appear in the final input string.",
          ident);

    if (!terminal(ident)) {
      return InvalidArgumentError("Unrecognized identifier '{}'", ident);
    }

    while (!myStack.empty()) {
      const auto top = myStack.top();
      myStack.pop();

      if (terminal(top)) {
        if (top == ident)
          // matched, continue with next token
          break;
        else
          return InvalidArgumentError(
              "Unexpected terminal symbol '{}', expected '{}'", ident, top);
      }

      // non-terminal
      const auto rowIt = table.find(top);
      if (rowIt == table.end())
        return InvalidArgumentError(
            "No production found for non-terminal symbol '{}'", top);

      const auto &row = rowIt->second;

      const auto colIt = row.find(ident);
      if (colIt == row.end())
        return InvalidArgumentError(
            "No production found for non-terminal symbol '{}' "
            "with terminal lookahead symbol '{}'",
            top,
            ident);

      const auto &rhsElem = colIt->second;

      // push rhsElem into stack in reverse order
      myStack.push_range(rhsElem              //
                         | rv::reverse        //
                         | rv::filter(NotNil) //
      );
    }
  }
  AC_UNREACHABLE()
}

auto Grammar::parse(string_type &&str) -> Status {
  if (!isLL1())
    return UnimplementedError(
        "Not implemented for the grammar that is not LL1. ");

  auto coro = Lexer::LexAsync(std::forward<string_type>(str));

  return _do_parse(std::move(coro) | std::ranges::views::common);
}
auto Grammar::parse(Piece::rhs_elem_view_t &&elems) -> auxilia::Status {
  if (!isLL1())
    return UnimplementedError(
        "Not implemented for the grammar that is not LL1. ");

  return _do_parse(std::forward<decltype(elems)>(elems));
}
#pragma endregion Parse
} // namespace accat::cp
