#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <optional>
#include <ranges>
#include <stack>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

#include <accat/auxilia/auxilia.hpp>

#include "Lexing.hpp"
#include "Grammar.hpp"

namespace accat::cp {
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
namespace sr = std::ranges;
namespace rv = std::ranges::views;
} // namespace accat::cp

namespace accat::cp {
#pragma region Piece
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

  // create A'
  _immediate_left_recursion(
      A, std::move(recRhsElems), std::move(nonRecRhsElems));
  return true;
}
void Grammar::_indirect_left_recursion(Piece &A, const Piece &B) const {
  Piece::rhs_t new_rhs;
  for (auto &&rhsElem : std::move(A.rhs_) | rv::as_rvalue) {
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
#pragma region Process
Status Grammar::_preprocess(const std::vector<Token> &tokens) {
  if (tokens.size() == 1) {
    AC_RUNTIME_ASSERT(tokens.back().is_type(Token::Type::kEndOfFile))
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
auto Grammar::_do_parse(std::vector<Token> &&tokens) {
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
      | rv::as_rvalue   // mark token in tokens as rvalue
      | std::ranges::views::take(tokSize)           // drop that kEndOfFile
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
  newPiece.rhs_.append_range(std::move(APrimeRhs) | rv::as_rvalue);
}
#pragma endregion Factor
#pragma region FirstSet
Grammar::Piece::set_t
Grammar::_first_set_from_rhs_elem(std::ranges::common_range auto &&rhsElem) {

  AC_RUNTIME_ASSERT(!sr::empty(rhsElem))

  Piece::set_t partialFirstSet;

  auto f = sr::cbegin(rhsElem);

  // A -> ε
  if (*f == NilMarker) {
    partialFirstSet.emplace(*f);
    // else, continue search?
    AC_RUNTIME_ASSERT(sr::next(f) == sr::cend(rhsElem),
                      "epsilon should be alone in production")
    return partialFirstSet;
  }

  if (terminal(*f)) {
    partialFirstSet.emplace(*f);
    AC_RUNTIME_ASSERT(*f != NilMarker,
                      "epsilon should have been handled earlier")
    return partialFirstSet;
  }

  for (; f != sr::cend(rhsElem); sr::advance(f, 1L)) {
    // terminal, add and stop
    if (terminal(*f)) {
      partialFirstSet.emplace(*f);
      AC_RUNTIME_ASSERT(*f != NilMarker,
                        "epsilon should have been handled earlier")
      break;
    }

    // non-terminal, recursively compute its first set if needed
    auto piecePtr = non_terminal(*f);
    AC_RUNTIME_ASSERT(piecePtr != nullptr,
                      "symbol must be terminal or non-terminal")

    // if not empty it's calculated before, handled in the function already
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
      partialFirstSet.emplace(NilMarker);
    }
  }
  return partialFirstSet;
}
void Grammar::_first_set_from_piece(Piece &A) {
  // if already computed (select_set_ has entries), skip
  if (!A.first_set_.empty())
    return;

  std::ranges::for_each(A.rhs_ | rv::as_const, [&](auto &&rhsElem) {
    auto rhsElemFirstSet = _first_set_from_rhs_elem(rhsElem);
    A.select_set_.emplace_back(rhsElemFirstSet);
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
auxilia::StatusOr<Grammar> Grammar::Process(string_type &&str) {
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
auxilia::StatusOr<Grammar> Grammar::FromStr(string_type &&str) {
  auto maybeTokens = Lexer{std::forward<string_type>(str)}.lexAll_or_error();
  if (!maybeTokens)
    return InvalidArgumentError(std::move(maybeTokens).error());

  auto tokens = *std::move(maybeTokens);

  if (auto status = _preprocess(tokens); !status)
    return {status};

  Grammar grammar;

  if (auto status = grammar._do_parse(std::move(tokens)); !status)
    return {status};

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

  // only select_set_ shall be edited
  for (auto &piece : pieces_) {
    if (piece.nullable_ == std::nullopt) {
      // not calculated
      piece.nullable(this);
    }
    Piece::set_t acc;
    for (auto index = 0ull; index < piece.rhs_.size(); ++index) {
      auto &select = piece.select_set_[index];

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

auto Grammar::parse(string_type &&str) -> Status {
  if (!isLL1())
    return UnimplementedError(
        "Not implemented for the grammar that is not LL1, "
        "default to false.");

  const auto coro = Lexer::LexAsync(std::forward<string_type>(str));

  std::stack<string_type> myStack;
  myStack.push(pieces_.front().lhs_);

  AC_RUNTIME_ASSERT(pieces_.front().follow_set_.contains(EndMarker))

  // construct a table for LL1 parsing
  std::unordered_map<string_type,
                     std::unordered_map<string_type, Piece::rhs_elem_t>>
      table;

  std::ranges::for_each(pieces_ | rv::as_const, [&](auto &&piece) {
    for (auto index = 0ull; index < piece.rhs_.size(); ++index) {
      for (const auto &terminalSym : piece.select_set_[index] | rv::as_const) {
        table[piece.lhs_][terminalSym] = piece.rhs_[index];
      }
    }
  });

  for (const auto &token : coro) {
    using enum Token::Type;
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
      else
        return InvalidArgumentError("Unexpected end of input, expected '{}'",
                                    myStack.top());
    }

    auto &&ident = token.lexeme_str();
    if (non_terminal(ident))
      return InvalidArgumentError(
          "Non-terminal identifier {} "
          "should not appear in the final input string.",
          ident);

    if (!terminal(ident)) {
      return InvalidArgumentError(
          "Unrecognized identifier '{}' at line {}", ident, token.line());
    }

    while (!myStack.empty()) {
      auto top = myStack.top();
      myStack.pop();

      if (terminal(top)) {
        if (top == ident)
          // matched, continue with next token
          break;
        else
          return InvalidArgumentError(
              "Unexpected terminal symbol '{}', expected '{}' at location {}",
              ident,
              top,
              token.line());
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
      myStack.push_range(rhsElem                       //
                         | rv::reverse                 //
                         | rv::filter([](auto &&sym) { //
                             return sym != NilMarker;  //
                           })                          //
      );
    }
  }
  AC_UNREACHABLE()
}
#pragma endregion Interface
} // namespace accat::cp
