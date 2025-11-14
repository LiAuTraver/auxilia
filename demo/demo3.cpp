#include <fmt/xchar.h>
#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdio>
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
#include "accat/auxilia/details/format.hpp"
#include "accat/auxilia/details/views.hpp"
constexpr auto str = R"~~(
E -> E+T | T
T -> T*F | F
F -> (E) | id
)~~";

using namespace accat::auxilia;
inline bool is_nonterminal(const Token &t) noexcept {
  // treat lexemes that start with an uppercase letter as non-terminals
  auto s = t.lexeme();
  if (s.empty())
    return false;
  unsigned char c = static_cast<unsigned char>(s.front());
  return std::isupper(c, std::locale());
}

inline bool is_terminal(const Token &t) noexcept { return !is_nonterminal(t); }
struct Parser {
  struct Grammar {
    using elem_t = Token;
    struct Piece : private Printable {
      using rhs_elem_t = std::vector<elem_t>;
      elem_t lhs;
      std::vector<rhs_elem_t> rhs;

      auto to_string(FormatPolicy) const {
        return lhs.to_string(FormatPolicy::kBrief)
            .append(" -> ")
            .append_range(
                std::ranges::views::join_with(
                    rhs //
                        | std::views::transform([](auto &&alt) {
                            return std::ranges::views::join_with(
                                alt //
                                    | std::views::transform([](auto &&sym) {
                                        return sym.to_string(
                                            FormatPolicy::kBrief);
                                      }),
                                "");
                          }),
                    "|")                        //
                | std::ranges::views::as_rvalue //
            );
      }

    private:
      decltype(auto) tie(this auto &&self) noexcept {
        return std::tie(self.lhs, self.rhs);
      }
    };
    std::vector<Piece> pieces;
  };
  Grammar grammar;

public:
};

void postValidation(const std::vector<Lexer::token_t> &tokens) {
  if (tokens.size() == 1) {
    AC_RUNTIME_ASSERT(tokens.back().is_type(kEndOfFile))
    Println("nothing to do");
    exit(1);
  }
  if (std::ranges::any_of(tokens, [&](const Token &token) {
        // allowed type in this Left Recursion Grammar.
        using enum Token::Type;
        static constexpr auto allowed = {kMonostate,
                                         kEndOfFile,
                                         kPlus,
                                         kStar,
                                         kIdentifier,
                                         kBitwiseOr,
                                         kLeftArrow,
                                         kLeftParen,
                                         kRightParen};
        const auto invalid = std::ranges::all_of(
            allowed, [&token](auto &&type) { return !token.is_type(type); });
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
auto tokensOrExit(Lexer &lexer) {
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

// New: eliminate indirect and direct left recursion
static Token make_identifier(const std::string &name) {
  using enum Token::Type;
  return Token::Lexeme(kIdentifier, name, /*line*/ 0);
}

static void eliminate_left_recursion(Parser::Grammar &g) {
  // build name -> index map
  std::unordered_map<std::string, size_t> idx;
  for (size_t i = 0; i < g.pieces.size(); ++i)
    idx[(g.pieces[i].lhs.lexeme()).data()] = i;

  // helper to check if a token is the non-terminal with name
  auto is_lhs_of = [&](const Token &tok, const std::string &name) {
    return (tok.lexeme()).data() == name && is_nonterminal(tok);
  };

  // eliminate indirect left recursion
  for (size_t i = 0; i < g.pieces.size(); ++i) {
    auto &Ai = g.pieces[i];
    for (size_t j = 0; j < i; ++j) {
      auto &Aj = g.pieces[j];
      std::vector<Parser::Grammar::Piece::rhs_elem_t> new_rhs;
      for (auto &prod : Ai.rhs) {
        if (!prod.empty() &&
            is_lhs_of(prod.front(), (Aj.lhs.lexeme()).data())) {
          // Ai -> Aj gamma  =>  substitute Aj -> delta into Ai
          for (auto &delta : Aj.rhs) {
            Parser::Grammar::Piece::rhs_elem_t combined;
            combined.reserve(delta.size() + (prod.size() - 1));
            // delta (explicit copy)
            for (auto &t : delta)
              combined.emplace_back(t.copy());
            // gamma (prod without first token) (explicit copy)
            for (size_t k = 1; k < prod.size(); ++k)
              combined.emplace_back(prod[k].copy());
            new_rhs.emplace_back(std::move(combined));
          }
        } else {
          // keep prod but copy its tokens explicitly
          Parser::Grammar::Piece::rhs_elem_t kept;
          kept.reserve(prod.size());
          for (auto &t : prod)
            kept.emplace_back(t.copy());
          new_rhs.emplace_back(std::move(kept));
        }
      }
      Ai.rhs = std::move(new_rhs);
    }

    // eliminate direct left recursion for Ai
    std::vector<Parser::Grammar::Piece::rhs_elem_t> nonrec;
    std::vector<Parser::Grammar::Piece::rhs_elem_t>
        rec; // store alpha (without leading Ai)
    for (auto &prod : Ai.rhs) {
      if (!prod.empty() && is_lhs_of(prod.front(), (Ai.lhs.lexeme()).data())) {
        Parser::Grammar::Piece::rhs_elem_t alpha;
        for (size_t k = 1; k < prod.size(); ++k)
          alpha.emplace_back(prod[k].copy());
        rec.emplace_back(std::move(alpha));
      } else {
        Parser::Grammar::Piece::rhs_elem_t beta;
        for (auto &t : prod)
          beta.emplace_back(t.copy());
        nonrec.emplace_back(std::move(beta));
      }
    }

    if (!rec.empty()) {
      // need to create Ai'
      std::string base = (Ai.lhs.lexeme()).data();
      std::string prime = base + "'";
      // ensure uniqueness
      while (idx.contains(prime))
        prime += "_";
      idx[prime] = g.pieces.size();

      // create new piece for Ai'
      Parser::Grammar::Piece newPiece;
      newPiece.lhs = make_identifier(prime);

      // Ai -> beta Ai'
      std::vector<Parser::Grammar::Piece::rhs_elem_t> new_Ai_rhs;
      for (auto &beta : nonrec) {
        Parser::Grammar::Piece::rhs_elem_t b =
            Parser::Grammar::Piece::rhs_elem_t{};
        b.reserve(beta.size() + 1);
        for (auto &t : beta)
          b.emplace_back(t.copy());
        b.emplace_back(newPiece.lhs.copy());
        new_Ai_rhs.emplace_back(std::move(b));
      }
      Ai.rhs = std::move(new_Ai_rhs);

      // Ai' -> alpha Ai' | ε
      for (auto &alpha : rec) {
        Parser::Grammar::Piece::rhs_elem_t a =
            Parser::Grammar::Piece::rhs_elem_t{};
        a.reserve(alpha.size() + 1);
        for (auto &t : alpha)
          a.emplace_back(t.copy());
        a.emplace_back(newPiece.lhs.copy());
        newPiece.rhs.emplace_back(std::move(a));
      }
      // epsilon = empty rhs (represent ε)
      newPiece.rhs.emplace_back().emplace_back(
          Token{Token::Type::kIdentifier,
                std::string_view(epsilon),
                Ai.lhs.line()}); // represents ε

      g.pieces.emplace_back(std::move(newPiece));
    }
  }
}

int main() {
  Lexer lexer(str);
  auto tokens = tokensOrExit(lexer);

  // split according to line number
  // std::ranges::chunk_by_view<owning_view<vector<Token,allocator<Token>>>,(lambda)>
  auto lines =
      std::move(tokens) // xvalue to form a owning view rather than a ref view
      | std::ranges::views::as_rvalue               //
      | std::ranges::views::take(tokens.size() - 1) // drop that kEndOfFile
      | std::ranges::views::chunk_by(
            [](auto &&a, auto &&b) { return a.line() == b.line(); }) //
      |
      std::ranges::views::transform([](auto &&line) {
        return std::move(line)
               // lhs kLeftArrow [rhs]
               |
               std::ranges::views::chunk_by([](auto &&lhs, auto &&rhs) {
                 return (lhs.is_type(kLeftArrow) == rhs.is_type(kLeftArrow)) &&
                        (lhs.is_type(kBitwiseOr) == rhs.is_type(kBitwiseOr));
               })                              //
               | std::ranges::views::enumerate //
               // get [lhs, rhs] for each line while performing validation.
               | std::ranges::views::filter( //
                     [](auto &&pair) {
                       auto &&[index, chunk] = pair;
                       if (index == 0) {
                         if (chunk.size() != 1 ||
                             std::ranges::any_of(chunk, [](auto &&t) {
                               return t.is_type(kLeftArrow);
                             })) {
                           AC_DEBUG_BREAK
                         }
                         return true;
                       }
                       if (index == 1) {
                         if (chunk.size() != 1 ||
                             !chunk.front().is_type(kLeftArrow)) {
                           AC_DEBUG_BREAK
                           return false;
                         }
                         return false;
                       }
                       if (std::ranges::any_of(chunk, [](auto &&t) {
                             return t.is_type(kLeftArrow);
                           })) {
                         AC_DEBUG_BREAK
                         return false;
                       }
                       if (std::ranges::any_of(chunk, [](auto &&t) {
                             return t.is_type(kBitwiseOr);
                           })) {
                         if (chunk.size() != 1) {
                           AC_DEBUG_BREAK
                           return false;
                         }
                         return false;
                       }
                       return true;
                     })                     //
               | std::ranges::views::values //
               | std::ranges::views::common //
            ;
      })                           //
      | std::ranges::views::common //
      ;
  std::cout << typeid(lines).name() << std::endl;
  Parser::Grammar grammar;
  for (auto &&l : lines) {
    Parser::Grammar::Piece piece;
    // explicitly copy lhs token
    piece.lhs = l.front().front().copy();
    for (auto &&chunk_view : l | std::ranges::views::drop(1)) {
      Parser::Grammar::Piece::rhs_elem_t rhs_vec;
      for (auto &&tok : chunk_view) {
        rhs_vec.emplace_back(tok.copy()); // explicit copy
      }
      piece.rhs.emplace_back(std::move(rhs_vec));
    }
    grammar.pieces.emplace_back(std::move(piece));
  }

  // New: eliminate left recursion in place
  eliminate_left_recursion(grammar);

  // print result
  Println();
  for (auto &p : grammar.pieces) {
    Println("{}", p);
  }
}
