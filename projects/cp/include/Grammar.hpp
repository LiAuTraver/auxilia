#pragma once

#include <algorithm>
#include <cstddef>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>
#include <unordered_set>

#include <accat/auxilia/auxilia.hpp>

namespace accat::cp {
class Token;
}

#pragma region Grammar
namespace accat::cp {
class Grammar : public auxilia::Printable {
  using elem_t = string_type;
  struct Piece;

public:
  Grammar() noexcept = default;
  Grammar(Grammar &&) noexcept = default;
  Grammar &operator=(Grammar &&) noexcept = default;
  Grammar(const Grammar &other) = delete;
  Grammar &operator=(const Grammar &other) = delete;

  using NonTerminal = Piece;
  using Terminal = elem_t;

public:
  decltype(auto) non_terminals_view(this auto &&self) {
    return self.pieces_ | std::ranges::views::transform(&Piece::lhs_);
  }
  decltype(auto) non_terminals(this auto &&self) { return self.pieces_; }
  decltype(auto) terminals(this auto &&self) {
    return self.terminals_ | std::ranges::views::common;
  }
  decltype(auto) terminal(this auto &&self, const std::string &str) {
    auto term = self.terminals_.find(str);
    if (term == self.terminals_.end())
      return static_cast<decltype(&*term)>(nullptr);
    return &*term;
  }
  decltype(auto) non_terminal(this auto &&self, const std::string &str) {
    auto nonT = std::ranges::find(self.pieces_, str, &Piece::lhs_);
    if (nonT == self.pieces_.end()) {
      return static_cast<decltype(&*nonT)>(nullptr);
    }
    return &*nonT;
  }

private:
  /// @note I named thie struct `Piece` at first but it turned out to be
  /// `NonTerminal` at the end.
  struct Piece : Printable {
    friend class Grammar;
    using lhs_t = elem_t;
    using rhs_elem_t = std::vector<elem_t>;
    template <class Ty> using rhs_container_t = std::vector<Ty>;
    using rhs_t = rhs_container_t<rhs_elem_t>;
    using set_t = std::unordered_set<string_type>;
    bool nullable(Grammar *myGrammar);
    string_type to_string(
        auxilia::FormatPolicy policy = auxilia::FormatPolicy::kDefault) const;
    auto &lhs() const noexcept { return lhs_; }
    auto &rhs() const noexcept { return rhs_; }
    auto &first_set() const noexcept { return first_set_; }
    auto &follow_set() const noexcept { return follow_set_; }

  private:
    lhs_t lhs_;
    rhs_t rhs_;
    set_t first_set_;
    set_t follow_set_;
    rhs_container_t<set_t> cache_rhsElemFirst2Select;
    rhs_container_t<bool> cache_rhsElemNullable;
    std::optional<bool> nullable_;
  };
  std::vector<Piece> pieces_;
  std::unordered_set<string_type> terminals_;
  std::optional<bool> is_ll1_;

private:
  /// ensure uniqueness of the name.
  /// @note this function only returns a unique name, and does NOT add it into
  /// the `index_map`.
  auto _new_unique_non_terminal_name(std::string_view origName,
                                     const char *prime) const -> string_type;

  void _immediate_left_recursion(Piece &A,
                                 Piece::rhs_t &&recRhsElems,
                                 Piece::rhs_t &&nonRecRhsElems);
  // eliminate direct left recursion for A
  bool _analyze_left_recursion(Piece &A);

  // eliminate indirect left recursion
  void _indirect_left_recursion(Piece &A, const Piece &B) const;
  void postprocess(std::ranges::common_range auto &&lines);
  auto do_parse(std::vector<Token> &&tokens);
  void do_factoring(size_t index);
  static auxilia::Status preprocess(const std::vector<Token> &tokens);

  auto _first_set_from_rhs_elem(std::ranges::common_range auto &&rhsElem)
      -> Piece::set_t;

  void _first_set_from_piece(Piece &A);
  // IMPORTANT: only Piece::first_set should be modified, other shall as_const
  void _compute_first_set();
  // ditto: follow_set
  void _compute_follow_set();

public:
  string_type
      to_string(auxilia::FormatPolicy = auxilia::FormatPolicy::kDefault) const;
  auxilia::Status eliminate_left_recursion();
  Grammar &apply_left_factorization();
  Grammar &calculate_set();
  bool isLL1();
  bool parse(std::string_view str);

public:
  static auxilia::StatusOr<Grammar> FromTokens(std::vector<Token> &&tokens);
  static auxilia::StatusOr<Grammar> FromStr(std::string_view str);
  static auxilia::StatusOr<Grammar> Process(auto &&tokens);
};

auxilia::StatusOr<Grammar> Grammar::Process(auto &&tokens) {
  auto grammar = FromTokens(std::forward<decltype(tokens)>(tokens));

  if (!grammar)
    return {std::move(grammar).as_status()};

  if (auto status = grammar->eliminate_left_recursion(); !status)
    return {std::move(status)};

  grammar->apply_left_factorization();
  return {std::move(grammar->calculate_set())};
}
} // namespace accat::cp
#pragma endregion Grammar
