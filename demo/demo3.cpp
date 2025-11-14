#include <fmt/xchar.h>
#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <iterator>
#include <ostream>
#include <ranges>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>
#include <optional>

#include "accat/auxilia/details/BNFLexer.hpp"
#include "accat/auxilia/details/Generator.hpp"
#include "accat/auxilia/details/Status.hpp"
#include "accat/auxilia/details/StatusOr.hpp"
#include "accat/auxilia/details/format.hpp"
#include "accat/auxilia/details/views.hpp"
constexpr auto str = R"~~(
E -> E+T | T
T -> T*F | F
F -> (E) | id
)~~";

using namespace accat::auxilia;

struct Grammar : private Printable {
  Grammar() noexcept = default;
  Grammar(Grammar &&) noexcept = default;
  Grammar &operator=(Grammar &&) noexcept = default;
  static constexpr auto allowed = {
      kMonostate,
      kEndOfFile,
      kPlus,
      kStar,
      kIdentifier,
      kBitwiseOr,
      kLeftArrow,
      kLeftParen,
      kRightParen,
  };

private:
  using elem_t = Token;
  struct Piece : private Printable {
    using rhs_elem_t = std::vector<elem_t>;
    elem_t lhs;
    std::vector<rhs_elem_t> rhs;

    auto to_string(FormatPolicy = FormatPolicy::kDefault) const {
      return lhs.to_string(FormatPolicy::kBrief)
          .append(" -> ")
          .append_range(std::ranges::views::join_with(
                            rhs //
                                | std::ranges::views::transform([](auto &&alt) {
                                    return alt                              //
                                           | std::ranges::views::transform( //
                                                 [](auto &&sym) {
                                                   return sym.to_string(
                                                       FormatPolicy::kBrief);
                                                 })                   //
                                           | std::ranges::views::join //
                                        ;
                                  }),                   //
                            '|')                        //
                        | std::ranges::views::as_rvalue //
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
  Status _direct_lr(auto &&A) {
    // eliminate direct left recursion for A
    std::vector<Piece::rhs_elem_t> nonrec;
    std::vector<Piece::rhs_elem_t> rec; // store alpha (without leading A)
    for (auto &prod : A.rhs) {
      if (prod.front().lexeme() == (A.lhs.lexeme())) {

        auto &alpha = rec.emplace_back();
        std::ranges::transform(prod | std::ranges::views::drop(1),
                               std::back_inserter(alpha),
                               [](auto &&obj) { return obj.copy(); });
      } else {
        auto &beta = nonrec.emplace_back();
        std::ranges::transform(prod, std::back_inserter(beta), [](auto &&obj) {
          return obj.copy();
        });
      }
    }

    // create A'
    if (!rec.empty()) {

      if (nonrec.empty())
        return ResourceExhaustedError("infinite loop");

      std::string base = (A.lhs.lexeme()).data();
      std::string prime = base + "'";
      // ensure uniqueness
      while (index_map.contains(prime))
        prime += "'";
      index_map[prime] = pieces.size();

      // create new piece for A'
      Piece newPiece;
      newPiece.lhs = Token::Identifier(prime);

      // A -> beta A'
      std::vector<Piece::rhs_elem_t> new_A_rhs;
      for (auto &beta : nonrec) {
        auto &b = new_A_rhs.emplace_back();
        b.reserve(beta.size() + 1);
        for (auto &t : beta)
          b.emplace_back(t.copy());
        b.emplace_back(newPiece.lhs.copy());
      }
      A.rhs = std::move(new_A_rhs);

      // A' -> alpha A' | epsilion
      for (auto &alpha : rec) {
        auto &a = newPiece.rhs.emplace_back();
        a.reserve(alpha.size() + 1);
        for (const auto &t : alpha)
          a.emplace_back(t.copy());
        a.emplace_back(newPiece.lhs.copy());
      }
      newPiece.rhs.emplace_back().emplace_back(Token::Identifier(epsilon, 0));

      pieces.emplace_back(std::move(newPiece));
    }
    return OkStatus();
  }
  void _indirect_lr(Piece &A, Piece &B) {
    std::vector<Piece::rhs_elem_t> new_rhs;
    for (auto &prod : A.rhs) {
      if ((prod.front().lexeme() == B.lhs.lexeme())) {
        // A -> B gamma  =>  substitute B -> delta into A
        for (const auto &delta : B.rhs) {

          auto &combined = new_rhs.emplace_back();

          combined.reserve(delta.size() + (prod.size() - 1));
          // delta (explicit copy)
          std::ranges::transform(delta,
                                 std::back_inserter(combined),
                                 [](const auto &obj) { return obj.copy(); });
          // gamma (prod without first token) (explicit copy)
          std::ranges::transform(prod | std::ranges::views::drop(1),
                                 std::back_inserter(combined),
                                 [](auto &&obj) { return obj.copy(); });
        }
      } else {
        auto &kept = new_rhs.emplace_back();
        kept.reserve(prod.size());
        std::ranges::transform(prod, std::back_inserter(kept), [](auto &&obj) {
          return obj.copy();
        });
      }
    }
    A.rhs = std::move(new_rhs);
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

      if (auto status = _direct_lr(A); !status)
        return status;
    }
    return OkStatus();
  }

public:
  static StatusOr<Grammar> parse(auto &&tokens) {
    static constexpr auto lineSeperator = [](auto &&a, auto &&b) {
      return a.line() == b.line();
    };
    static constexpr auto validSep = [](auto &&pair) {
      auto &&[index, chunk] = pair;
      if (index == 0) {
        // first should be a single Identifier.
        if (chunk.size() != 1 || std::ranges::any_of(chunk, [](auto &&t) {
              return t.is_type(kLeftArrow);
            })) {
          AC_DEBUG_BREAK
        }
        return true;
      }
      // second should be a single LeftArrow.
      if (index == 1) {
        if (chunk.size() != 1 || !chunk.front().is_type(kLeftArrow)) {
          AC_DEBUG_BREAK
          return false;
        }
        return false;
      }
      // following chunks
      if (std::ranges::any_of(chunk,
                              [](auto &&t) { return t.is_type(kLeftArrow); })) {
        // should not be more LeftArrows.
        AC_DEBUG_BREAK
        return false;
      }
      if (std::ranges::any_of(chunk,
                              [](auto &&t) { return t.is_type(kBitwiseOr); })) {
        if (chunk.size() != 1) {
          // multiple BitwiseOr appears.
          AC_DEBUG_BREAK
          return false;
        }
        // valid BitwiseOr, but we don't need it.
        return false;
      }
      // valid elem of rhs.
      return true;
    };
    static constexpr auto segmentsSep = [](auto &&lhs, auto &&rhs) {
      return (lhs.is_type(kLeftArrow) == rhs.is_type(kLeftArrow)) &&
             (lhs.is_type(kBitwiseOr) == rhs.is_type(kBitwiseOr));
    };
    // split according to line number
    // transform_view<chunk_by_view<take_view<as_rvalue_view<owning_view<vector<Token>>>>,
    // (lambda)>, (lambda)>
    auto lines =
        std::move(tokens) // xvalue to form a owning view rather than a ref view
        | std::ranges::views::as_rvalue // mark token in tokens as rvalue
        | std::ranges::views::take(tokens.size() - 1) // drop that kEndOfFile
        | std::ranges::views::chunk_by(lineSeperator) // split by line
        |
        std::ranges::views::transform([](auto &&line) {
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
    grammar.post_parse(std::move(lines));

    return {std::move(grammar)};
  }
  void post_parse(auto &&lines) {

    for (auto &&l : lines) {
      auto &piece = pieces.emplace_back();
      piece.lhs = l.front().front();
      index_map.emplace(piece.lhs.lexeme(), piece.lhs.line());
      for (auto &&chunk_view : l // already rvalue
                                   | std::ranges::views::drop(1) //
                                   | std::ranges::views::as_rvalue) {
        piece.rhs.emplace_back().assign_range(chunk_view);
      }
    }
  }
  auto to_string(FormatPolicy = FormatPolicy::kDefault) const {
    return std::ranges::views::join_with(
               pieces //
                   | std::views::transform(
                         [](auto &&piece) { return piece.to_string(); }),
               '\n')                        //
           | std::ranges::to<string_type>() //
        ;
  }
};

static void postValidation(const std::vector<Lexer::token_t> &tokens) {
  if (tokens.size() == 1) {
    AC_RUNTIME_ASSERT(tokens.back().is_type(kEndOfFile))
    Println("nothing to do");
    exit(1);
  }
  if (std::ranges::any_of(tokens, [&](const Token &token) {
        // allowed type in this Left Recursion Grammar.
        using enum Token::Type;
        const auto invalid =
            std::ranges::all_of(Grammar::allowed, [&token](auto &&type) {
              return !token.is_type(type);
            });
        if (invalid)
          Println(R"('{1}' Contains non-allowed type '{0}')",
                  token.type_str(),
                  token.to_string(FormatPolicy::kBrief));
        return invalid;
      })) {
    Println(stderr, "Validation failure.");
    exit(1);
  }
  Println("Lex process successfully finished.");
}
static auto tokensOrExit(Lexer &lexer) {
  auto tokens = lexer.lexAll();
  if (!lexer.error()) {
    postValidation(tokens);
    return tokens;
  }
  Println(stderr, "Error:");
  std::ranges::for_each(tokens, [](const Token &token) {
    if (token.is_type(Token::Type::kLexError)) {
      Println(stderr,
              "   {}",
              std::ranges::views::drop_while(
                  token.to_string(), [](const char ch) { return ch != ','; }) |
                  std::ranges::views::drop(1) | //
                  std::ranges::to<std::string>());
    }
  });
  Println(stderr, "Lex process finished with {} error(s).", lexer.error());
  exit(1);
}

int main() {
  Lexer lexer(str);
  auto tokens = tokensOrExit(lexer);

  auto grammar = Grammar::parse(std::move(tokens)).rvalue().value();

  grammar.eliminate_left_recursion();

  // print result
  std::cout << grammar;
}
