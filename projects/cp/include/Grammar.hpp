#pragma once

#include <algorithm>
#include <cstddef>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>
#include <unordered_set>

#include "accat/auxilia/base/config.hpp"
#include "accat/auxilia/base/format.hpp"

namespace accat::auxilia {
class Status;
template <typename> class StatusOr;
} // namespace accat::auxilia
namespace accat::cp {
class Token;
} // namespace accat::cp

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

  static constexpr auto EndMarker = "$";
  static constexpr auto NilMarker = auxilia::epsilon;

public:
  decltype(auto) non_terminals_view(this auto &&self) {
    return self.pieces_ | std::ranges::views::transform(&Piece::lhs_);
  }
  decltype(auto) non_terminals(this auto &&self) { return (self.pieces_); }
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
    template <class Ty> using rhs_container_t = std::vector<Ty>;
    template <class Ty> using rhs_view_t = std::span<Ty>;
    using rhs_elem_t = rhs_container_t<elem_t>;
    using rhs_elem_view_t = rhs_view_t<elem_t>;
    using rhs_t = rhs_container_t<rhs_elem_t>;
    using set_t = std::unordered_set<string_type>;
    bool nullable(Grammar *);
    bool endable() const;
    string_type to_string(
        auxilia::FormatPolicy = auxilia::FormatPolicy::kDefault) const;
    auto &lhs() const noexcept { return lhs_; }
    auto &rhs() const noexcept { return rhs_; }
    auto &first_set() const noexcept { return first_set_; }
    auto &follow_set() const noexcept { return follow_set_; }
    auto &select_set() const noexcept { return cache_selectSet_; }

  private:
    lhs_t lhs_;
    rhs_t rhs_;
    set_t first_set_;
    set_t follow_set_;
    /// during the process, it would first store each elem's first set as a
    /// temporary cache, and turns into select set for each elem when the
    /// `is_LL1` function called.
    rhs_container_t<set_t> cache_selectSet_;
    /// ditto, store when `is_nullable` is called, used in `isLL1`.
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

  static auto _preprocess(const std::vector<Token> &tokens) -> auxilia::Status;
  void _postprocess(std::ranges::common_range auto &&lines);
  auto _expand_ebnf_constructs(std::vector<Token> &&tokens) -> std::vector<Token>;
  auto _do_process(std::vector<Token> &&tokens);

  void _do_factoring(size_t index);

  auto _first_set_from_rhs_elem(std::ranges::common_range auto &&rhsElem)
      -> Piece::set_t;

  void _first_set_from_piece(Piece &A);

  bool _follow_set_from_rhs_elem(const Piece &A,
                                 std::ranges::common_range auto &&rhsElem);

  auto _do_parse(std::ranges::common_range auto &&elems) const;

public:
  auto eliminate_left_recursion() -> auxilia::Status;
  void apply_left_factorization();
  // IMPORTANT: only Piece::first_set should be modified, other shall as_const
  void compute_first_set();
  // ditto: follow_set
  void compute_follow_set();

  bool isLL1();

  auto parse(string_type &&) -> auxilia::Status;
  auto parse(Piece::rhs_elem_view_t &&) -> auxilia::Status;

  auto to_string(auxilia::FormatPolicy = auxilia::FormatPolicy::kDefault) const
      -> string_type;

public:
  /// parse the string, eliminate left recursion, extract the common left
  /// factor, and compute it's FIRST/FOLLOW set.
  static auxilia::StatusOr<Grammar> Process(string_type &&str);
  /// Only parse the string and get the unprocessed grammar.
  static auxilia::StatusOr<Grammar> FromStr(string_type &&str);
};
} // namespace accat::cp
#pragma endregion Grammar
