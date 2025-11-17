#pragma once
#include <algorithm>
#include <cctype>
#include <charconv>
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <expected>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "./config.hpp"
#include "./format.hpp"
#include "./Status.hpp"
#include "./StatusOr.hpp"
#include "./Generator.hpp"
#include "./chars.hpp"

#include "./EBNF_lexing.hpp"
namespace accat::auxilia {
class Grammar : public Printable {
  using enum Token::Type;
  static auto copyToken(const Token &token) noexcept { return token.copy(); };

public:
  Grammar() noexcept = default;
  Grammar(Grammar &&) noexcept = default;
  Grammar &operator=(Grammar &&) noexcept = default;
  Grammar(const Grammar &other) = delete;
  Grammar &operator=(const Grammar &other) = delete;

private:
  using elem_t = Token;
  struct Piece : Printable {
    using lhs_t = elem_t;
    using rhs_elem_t = std::vector<elem_t>;
    using rhs_t = std::vector<rhs_elem_t>;
    lhs_t lhs;
    rhs_t rhs;

    auto longest_common_prefix() const {
      std::unordered_map<string_type, size_t> m;
    }

    static auto longest_common_prefix_count(const rhs_elem_t &a,
                                            const rhs_elem_t &b) {
      AC_PRECONDITION(!a.empty() && !b.empty())
      auto &&[it_a, it_b] = std::ranges::mismatch(
          a, b, std::ranges::equal_to{}, &Token::lexeme, &Token::lexeme);
      return std::ranges::distance(a.begin(), std::move(it_a));
    }

    auto to_string(FormatPolicy policy = FormatPolicy::kDefault) const {
      return lhs //
          .to_string(FormatPolicy::kBrief)
          .append(" -> ")
          .append_range(
              rhs //
              | std::ranges::views::transform([](auto &&alt) {
                  return alt                                               //
                         | std::ranges::views::transform(Printable::Brief) //
                         | std::ranges::views::join_with(' ')              //
                      ;
                })                                                     //
              | std::ranges::views::join_with(std::string_view(" | ")) //
              // ^^^ workaround, pass const char* seems cause issue
          );
    }

  private:
    decltype(auto) tie(this auto &&self) noexcept {
      return std::tie(self.lhs, self.rhs);
    }
  };
  std::vector<Piece> pieces;
  std::unordered_map<std::string, size_t> index_map;

private:
  void _direct_lr(Piece &A,
                  Piece::rhs_t &&recRhsElems,
                  Piece::rhs_t &&nonRecRhsElems) {
    auto prime = std::string(A.lhs.lexeme()) + "'";
    // ensure uniqueness
    while (index_map.contains(prime))
      prime += "'";
    index_map[prime] = pieces.size();

    // create new piece for A'
    Piece newPiece;
    newPiece.lhs = Token::Identifier(prime);

    // A -> beta A'
    Piece::rhs_t new_A_rhs;
    for (auto &&beta : nonRecRhsElems | std::ranges::views::as_rvalue) {
      auto &betaAprime = new_A_rhs.emplace_back();
      betaAprime.reserve(beta.size() + 1);
      betaAprime.append_range(beta | std::ranges::views::as_rvalue);
      betaAprime.emplace_back(newPiece.lhs.copy());
    }
    A.rhs = std::move(new_A_rhs);

    // A' -> alpha A' | epsilion
    for (auto &&alpha : recRhsElems | std::ranges::views::as_rvalue) {
      auto &alphaAprime = newPiece.rhs.emplace_back();
      alphaAprime.reserve(alpha.size() + 1);
      alphaAprime.assign_range(alpha | std::ranges::views::as_rvalue);
      alphaAprime.emplace_back(newPiece.lhs.copy());
    }
    newPiece.rhs.emplace_back().emplace_back(Token::Identifier(epsilon));

    pieces.emplace_back(std::move(newPiece));
  }
  // eliminate direct left recursion for A
  Status _analyze_lr(Piece &A) {
    Piece::rhs_t nonRecRhsElems;
    Piece::rhs_t recRhsElems; // store alpha (without leading A)
    // for readability I choose not to remove braces and got alpha and beta as
    // aliases (, though I really want to write them one line).
    for (auto &&rhsElem : std::move(A.rhs) | std::ranges::views::as_rvalue) {
      if (rhsElem.front().lexeme() == A.lhs.lexeme()) {
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
      A.rhs.assign_range(std::move(nonRecRhsElems) |
                         std::ranges::views::as_rvalue);
      return OkStatus();
    }

    if (nonRecRhsElems.empty())
      // A -> A
      return ResourceExhaustedError("infinite loop");

    // create A'
    _direct_lr(A, std::move(recRhsElems), std::move(nonRecRhsElems));
    return OkStatus();
  }
  void _indirect_lr(Piece &A, const Piece &B) const {
    Piece::rhs_t new_rhs;
    for (auto &&rhsElem : std::move(A.rhs) | std::ranges::views::as_rvalue) {
      AC_DEBUG_ONLY(AC_RUNTIME_ASSERT(!rhsElem.empty(), "should not happen"))
      AC_STATIC_ASSERT(std::is_rvalue_reference_v<decltype(rhsElem)>);

      if ((rhsElem.front().lexeme() == B.lhs.lexeme())) {
        // A -> B gamma  =>  substitute B -> delta into A
        for (const auto &delta : B.rhs) {

          auto &combined = new_rhs.emplace_back();

          combined.reserve(delta.size() + (rhsElem.size() - 1));
          std::ranges::transform(
              delta, std::back_inserter(combined), &Token::copy);

          std::ranges::transform(rhsElem                            //
                                     | std::ranges::views::as_const //
                                     | std::ranges::views::drop(1),
                                 std::back_inserter(combined),
                                 &Token::copy);
          // ^^^ as_const is for readability:
          // we prevent/didn't perform move here from xvalue `rhsElem`
          // (we may use rhsElem again during next iteration of delta)
        }
      } else {
        // keep untouched
        new_rhs.emplace_back(std::move(rhsElem));
      }
    }
    A.rhs = std::move(new_rhs);
  }
  static Status preprocess(const std::vector<Token> &tokens) {
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
    return OkStatus("Lex process successfully finished.");
  }
  void postprocess(std::ranges::common_range auto &&lines) {

    for (auto &&l : lines) {
      AC_STATIC_ASSERT(std::is_rvalue_reference_v<decltype(l)>);
      auto &piece = pieces.emplace_back();
      piece.lhs = l.front().front();
      // build index_map here
      index_map.emplace(piece.lhs.lexeme(), piece.lhs.line());
      for (auto &&chunk_view : l // already rvalue
                                   | std::ranges::views::drop(1) //
                                   | std::ranges::views::as_rvalue) {
        piece.rhs.emplace_back().assign_range(chunk_view);
      }
    }
  }
  static StatusOr<Grammar> do_parse(std::vector<Token> &&tokens) {
    // I admit it's a bit messy here, but as the saying goes:
    // "If it works, don't touch it".

    if (auto status = preprocess(tokens); !status)
      return status;

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

    // transform_view<chunk_by_view<take_view<as_rvalue_view<owning_view<vector<Token>>>>,(l)>,(l)>
    auto lines =
        std::move(tokens) // xvalue to form a owning view rather than a ref view
        | std::ranges::views::as_rvalue // mark token in tokens as rvalue
        | std::ranges::views::take(tokens.size() - 1) // drop that kEndOfFile
        | std::ranges::views::chunk_by(stmtSeperator) // split by line
        |
        std::ranges::views::transform([&](auto &&line) {
          return line // now: lhs kLeftArrow rhs_elem1 kBitwiseOr rhs_elem2 ...
                 | std::ranges::views::chunk_by(segmentsSep) // result ^^^
                 | std::ranges::views::enumerate             // [index, chunk]
                 | std::ranges::views::filter(validSep) // extract lhs and rhs.
                 | std::ranges::views::values // drop views::enum's key.
                 | std::ranges::views::common // idk, prevent compat issues
              ;                      // returned: lhs rhs_elem1 rhs_elem2 ...
        })                           //
        | std::ranges::views::common // ditto
        ;

    Grammar grammar;
    grammar.postprocess(std::move(lines));

    // IMPORTANT: ranges::views are lazy-evaluated, so the validation is done
    // during the post_parse call(where we actually iterate through the views).
    // Thus we should check errorMsg after that;
    // otherwise errorMsg will always be empty here.
    if (!errorMsg_lazy.empty()) {
      return {InvalidArgumentError(std::move(errorMsg_lazy))};
    }

    return {std::move(grammar)};
  }

public:
  Status eliminate_left_recursion() {
    // eliminate indirect left recursion
    for (size_t i = 0; i < pieces.size(); ++i) {
      auto &A = pieces[i];
      for (size_t j = 0; j < i; ++j) {
        auto &B = pieces[j];
        _indirect_lr(A, B);
      }

      if (auto status = _analyze_lr(A); !status)
        return status;
    }
    return OkStatus();
  }
  Status extract_left_factor() {
    AC_TODO_()
    return UnimplementedError();
  }
  static auto parse(std::vector<Token> &&tokens) {
    return do_parse(std::move(tokens));
  }
  auto to_string(FormatPolicy = FormatPolicy::kDefault) const {
    return pieces                                              //
           | std::ranges::views::transform(Printable::Default) //
           | std::ranges::views::join_with('\n')               //
           | std::ranges::to<string_type>()                    //
        ;
  }
};
} // namespace accat::auxilia
