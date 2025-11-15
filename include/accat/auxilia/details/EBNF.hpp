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
#include <limits>
#include <memory>
#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "./config.hpp"
#include "./format.hpp"
#include "./Status.hpp"
#include "./StatusOr.hpp"
#include "./Generator.hpp"
#include "./chars.hpp"

namespace accat::auxilia {
struct Token : Printable {
  enum class Type : uint8_t {
    // clang-format off
    kMonostate = 0,
    // Single-character tokens.
    kLeftParen, kRightParen, kLeftBrace, kRightBrace, kComma, 
    kDot, kMinus, kPlus, kSemicolon, kSlash, kAmpersand, kStar,
    // One or two character tokens.
    kBang, kBangEqual, kEqual, kEqualEqual, 
    kGreater, kGreaterEqual, kLess, kLessEqual,
    // Literals.
    kIdentifier, kString, kNumber,

    // added to suit this grammar parser
    kLeftArrow, // ->
    kBitwiseOr, // |
    kSquareBracketOpen, // [
    kSquareBracketClose, // ]

    kLexError,
    // end of file.
    kEndOfFile = std::numeric_limits<uint8_t>::max()
    // clang-format on
  };
  static constexpr auto token_type_str(Type type) -> std::string_view;
  constexpr auto token_type_operator() const -> std::string_view;
  Token() = default;
  Token(Type type, std::string_view lexeme, uint_least32_t line)
      : type_(type), lexeme_(std::string{lexeme}), line_(line) {}
  Token(const Token &) = delete;
  Token &operator=(const Token &) = delete;
  Token(Token &&that) noexcept { _do_move(std::move(that)); }
  Token &operator=(Token &&that) noexcept {
    if (this == &that)
      return *this;
    return _do_move(std::move(that));
  }
  AC_CONSTEXPR20_ ~Token() noexcept {
    if (type_ != Type::kNumber && type_ != Type::kMonostate)
      lexeme_.~string_type();
  }

public:
  auto lexeme() const AC_NOEXCEPT [[clang::lifetimebound]] -> std::string_view {
    AC_PRECONDITION(type_ != Type::kNumber && type_ != Type::kLexError,
                    "lexeme() called on a non-lexeme token")
    return lexeme_;
  }
  auto number() const AC_NOEXCEPT {
    AC_PRECONDITION(type_ == Type::kNumber,
                    "number() called on a non-number token");
    using NumberType = std::variant<long long, long double>;
    return number_is_integer_ ? NumberType{num_ll_} : NumberType{num_ld_};
  }
  auto error_message() const AC_NOEXCEPT [[clang::lifetimebound]]
  -> std::string_view {
    AC_PRECONDITION(type_ == Type::kLexError,
                    "error_message() called on a non-error token")
    return lexeme_;
  }
  constexpr auto type() const noexcept { return type_; }
  constexpr auto type_str() const noexcept { return token_type_str(type_); }
  constexpr auto is_type(const Type type) const noexcept {
    return type_ == type;
  }
  constexpr auto line() const noexcept { return line_; }
  auto to_string(const auxilia::FormatPolicy &format_policy =
                     auxilia::FormatPolicy::kDefault) const -> string_type {
    auto str = string_type{};
    if (format_policy == auxilia::FormatPolicy::kBrief)
      str = _do_format(format_policy);
    else
      str = Format("type: {}, {}, line: {}",
                   type_str(),
                   _do_format(format_policy),
                   line_);

    return str;
  }

protected:
  Token(Type type, std::string &&error_message, uint_least32_t line)
      : type_(type), lexeme_(std::move(error_message)), line_(line) {}
  Token(Type type, long double number, uint_least32_t line)
      : type_(type), number_is_integer_(false), num_ld_(number), line_(line) {}
  Token(Type type, long long number, uint_least32_t line)
      : type_(type), number_is_integer_(true), num_ll_(number), line_(line) {}

public:
  static auto Number(const std::variant<long long, long double> value,
                     const uint_least32_t line = npos32) noexcept {
    return std::visit(
        [line](auto &&v) { return Token{Type::kNumber, v, line}; }, value);
  }
  static auto Lexeme(const Type type,
                     const std::string_view lexeme,
                     const uint_least32_t line = npos32) noexcept {
    return Token{type, lexeme, line};
  }
  static auto Error(std::string &&message,
                    const uint_least32_t line = npos32) noexcept {
    return Token{Type::kLexError, std::move(message), line};
  }
  static auto Identifier(const std::string_view ident,
                         const uint_least32_t line = npos32) noexcept {
    return Token{Type::kIdentifier, ident, line};
  }
  static auto eof(const uint_least32_t line /* required */) noexcept {
    using namespace std::string_view_literals;
    return Token{Type::kEndOfFile, ""sv, line};
  }

public:
  auto operator<=>(const Token &that) const
      noexcept(noexcept(lexeme_ <=> that.lexeme_)) -> std::partial_ordering {
    if (this == std::addressof(that))
      return std::partial_ordering::equivalent;
    if (type_ != that.type_)
      return type_ <=> that.type_;
    switch (type_) {
    case Type::kLexError:
      return std::partial_ordering::unordered;
    case Type::kNumber:
      if (number_is_integer_ && that.number_is_integer_)
        return num_ll_ <=> that.num_ll_; // strong ordering convert to partial
      return num_ld_ <=> that.num_ld_;
    case Type::kMonostate:
      return std::partial_ordering::equivalent;
    default:
      return lexeme_ <=> that.lexeme_;
    }
  }
  auto operator!=(const Token &that) const
      noexcept(noexcept(this->operator<=>(that))) -> bool {
    return *this <=> that != std::partial_ordering::equivalent;
  }
  Token copy() const noexcept {
    Token newT;
    newT.type_ = type_;
    if (type_ == Type::kNumber) {
      if ((newT.number_is_integer_ = number_is_integer_)) {
        newT.num_ll_ = num_ll_;
      } else {
        newT.num_ld_ = num_ld_;
      }
    } else if (type_ != Type::kMonostate) {
      ::new (std::addressof(newT.lexeme_)) std::string((lexeme_));
    }
    AC_DEBUG_ONLY(else { newT.monostate_ = monostate_; })
    AC_DEBUG_ONLY(newT.line_ = line_;)

    return newT;
  }

private:
  Type type_ = Type::kMonostate;
  union {
    static_assert(std::is_trivial_v<std::monostate>);
    std::monostate monostate_{};
    string_type lexeme_;
    struct {
      bool number_is_integer_;
      union {
        long double num_ld_;
        long long num_ll_;
      };
    };
  };
  uint_least32_t line_ = std::numeric_limits<uint_least32_t>::signaling_NaN();

private:
  Token &_do_move(Token &&that) noexcept {
    // Destroy old active member
    if (type_ != Type::kNumber && type_ != Type::kMonostate)
      lexeme_.~string_type();

    type_ = that.type_;
    line_ = that.line_;

    if (that.type_ == Type::kNumber) {
      if (that.number_is_integer_) {
        num_ll_ = that.num_ll_;
        number_is_integer_ = true;
      } else {
        num_ld_ = that.num_ld_;
        number_is_integer_ = false;
      }
    } else {
      ::new (std::addressof(lexeme_)) std::string(std::move(that.lexeme_));
    }

    that.type_ = Type::kMonostate;
    return *this;
  }
  auto _do_format(const auxilia::FormatPolicy format_policy) const
      -> string_type {
    auto str = string_type{};
    const auto format_number = [this]() -> long double {
      return number_is_integer_ ? static_cast<long double>(num_ll_) : num_ld_;
    };
    using namespace std::string_literals;
    if (format_policy == auxilia::FormatPolicy::kBrief) {
      if (type_ == Type::kNumber)
        str = Format("{}", format_number());
      else if (type_ == Type::kLexError)
        str = lexeme_;
      else if (type_ == Type::kMonostate)
        str = "monostate"s;
      else
        str = token_type_operator();
    } else {
      if (type_ == Type::kNumber)
        str = Format("number: '{}'", format_number());
      else if (type_ == Type::kLexError)
        str = Format("error: '{}'", lexeme_);
      else if (type_ == Type::kMonostate)
        str = "monostate"s;
      else
        str = Format("lexeme: '{}'", token_type_operator());
    }
    return str;
  }
} inline AC_CONSTEXPR20_ nulltok{};
class Lexer {
public:
  using size_type = typename std::string::size_type;
  using string_type = std::string;
  using string_view_type = std::string_view;
  using path_type = std::filesystem::path;
  using status_t = auxilia::Status;
  using token_t = Token;
  using token_type_t = Token::Type;
  using char_t = typename string_type::value_type;
  using generator_t = auxilia::Generator<token_t, uint_least32_t>;
  using number_value_t = std::variant<long long, long double>;
  using enum token_type_t;
  static constexpr auto tolerable_chars = as_chars("_`$");
  static constexpr auto whitespace_chars = as_chars(" \t\r");
  static constexpr auto newline_chars = as_chars("\n\v\f");

public:
  Lexer() = delete;
  Lexer(string_type &&s) : contents(std::move(s)) {}
  Lexer(const Lexer &other) = delete;
  Lexer(Lexer &&other) noexcept = default;
  Lexer &operator=(const Lexer &other) = delete;
  Lexer &operator=(Lexer &&other) noexcept = default;
  ~Lexer() = default;

public:
  auto lexAsync() -> generator_t {
    while (not is_at_end()) {
      head = cursor;
      if (auto token = next_token(); token.type() != kMonostate) {
        co_yield token;
      }
    }
    co_yield add_token(kEndOfFile);
    co_return error_count;
  }
  auto lexAll_or_error() -> std::expected<std::vector<token_t>, string_type> {
    std::vector<token_t> result;
    while (not is_at_end()) {
      head = cursor;
      if (auto token = next_token(); token.type() != kMonostate) {
        result.emplace_back(std::move(token));
      }
    }
    result.emplace_back(add_token(kEndOfFile));
    if (!error())
      return result;

    return std::unexpected(
        std::ranges::views::join_with(
            result | std::views::filter([](const token_t &t) {
              return t.is_type(Token::Type::kLexError);
            }) | std::views::transform([](const token_t &t) {
              return std::string(t.error_message());
            }),
            '\n') |
        std::ranges::to<string_type>());
  }
  /// @brief get the number of errors
  /// @return the number of errors
  auto error() const noexcept -> uint_least32_t { return error_count; }

private:
  bool is_at_end(const size_t offset = 0) const {
    return cursor + offset >= contents.size();
  }
  inline token_t add_identifier_or_keyword() {
    auto value = lex_identifier();
    AC_DEBUG_LOGGING(trace, "identifier: {}", value)
    return add_token(kIdentifier);
  }
  inline token_t add_number() {
    if (auto value = lex_number()) {
      return add_token(*value);
    }
    return add_error_token("Invalid number: " +
                           contents.substr(head, cursor - head));
  }
  inline token_t add_string() {
    // hard to do...

    if (auto status = lex_string(); !status.ok()) {
      // not null-terminated, passing `.data()` only will include the rest of
      // the whole contents.
      return add_error_token(
          {status.message().data(), status.message().size()});
    }
    return add_token(kString);
  }
  inline token_t add_comment() {
    while (peek() != '\n' && !is_at_end())
      get();
    return {};
  }
  inline token_t next_token() {
    // token1 token2
    // 			 ^ cursor position
    AC_PRECONDITION(cursor < contents.size(), "cursor out of bounds")

    // we use `monostate` to indicate that the token is a
    switch (auto c = get()) {
    case '(':
      return add_token(kLeftParen);
    case ')':
      return add_token(kRightParen);
    case '{':
      return add_token(kLeftBrace);
    case '}':
      return add_token(kRightBrace);
    case ',':
      return add_token(kComma);
    case '.':
      return add_token(kDot);
    case '-':
      return add_token(advance_if_is('>') ? kLeftArrow : kMinus);
    case '|':
      return add_token(kBitwiseOr);
    case '+':
      return add_token(kPlus);
    case ';':
      return add_token(kSemicolon);
    case '*':
      return add_token(kStar);
    case '&':
      return add_token(kAmpersand);
    case '[':
      return add_token(kSquareBracketOpen);
    case ']':
      return add_token(kSquareBracketClose);
    case '!':
      return add_token(advance_if_is('=') ? kBangEqual : kBang);
    case '=':
      return add_token(advance_if_is('=') ? kEqualEqual : kEqual);
    case '<':
      return add_token(advance_if_is('=') ? kLessEqual : kLess);
    case '>':
      return add_token(advance_if_is('=') ? kGreaterEqual : kGreater);
    case '/':
      return advance_if_is('/') ? add_comment() : add_token(kSlash);
    default:
      if (whitespace_chars.contains(c))
        return {}; // continue
      if (newline_chars.contains(c)) {
        current_line++;
        return {}; // continue
      }
      if (c == '"') {
        return add_string();
      }
      // first, numbers(order matters!)
      if (std::isdigit(c)) {
        return add_number();
      }
      // finally, letters
      if (std::isalpha(c) or tolerable_chars.contains(c)) {
        return add_identifier_or_keyword();
      }
      return add_error_token(
          Format("Unexpected character: '{}' at line {}", c, current_line));
    }
  }

  inline auto add_token(Token::Type type) -> token_t {
    if (type == kEndOfFile) { // FIXME: lexeme bug at EOF(not critical)
      return token_t::eof(current_line);
    }
    auto lexeme = string_view_type(contents.data() + head, cursor - head);
    AC_DEBUG_LOGGING(trace, "lexeme: {}", lexeme)
    return token_t::Lexeme(type, lexeme, current_line);
  }
  inline auto add_token(number_value_t number) const -> token_t {
    AC_DEBUG_LOGGING(trace, "lexeme: number")
    return token_t::Number(number, current_line);
  }
  inline auto add_error_token(string_type &&msg) -> token_t {
    error_count++;
    // return {kLexError, msg, current_line};
    return token_t::Error(std::move(msg), current_line);
  }
  inline auto lex_string() -> status_t {
    while (peek() != '"' && !is_at_end()) {
      if (peek() == '\n')
        current_line++; // multiline string, of course we dont want act like
                        // C/C++ which will result in a compile error if the
                        // string is not closed at the same current_line.
      get();
    }
    if (is_at_end() && peek() != '"') {
      return auxilia::InvalidArgumentError(
          "Unterminated string: " + contents.substr(head, cursor - head));
    }
    // "i am a string..."
    // 						      ^ cursor position
    else
      get(); // consume the closing quote.
    return {};
  }
  inline auto lex_number() -> std::optional<number_value_t> {
    // 0x123456
    //  ^ cursor position(one after the first digit)
    auto Base = 10;
    if (auto c = peek(); is_valid_base(c)) {
      if (c == 'x' || c == 'X')
        Base = 16;
      else if (c == 'b' || c == 'B')
        Base = 2;
      else if (c == 'o' || c == 'O')
        Base = 8;
      else if (c == 'd' || c == 'D')
        Base = 10;
      get(); // consume the 'x', 'b', or 'o'
    }
    while (is_valid_digit_of_base(peek(), Base)) {
      get();
    }
    bool is_floating_point = false;
    // maybe a '.'?
    if (peek() == '.' && std::isdigit(peek(1))) {
      get(); // consume the '.'
      while (is_valid_digit_of_base(peek(), Base)) {
        get();
      }
      // 123.456
      // 		    ^ cursor position(one after the last digit)
      is_floating_point = true;
    }
    // 789
    //    ^ cursor position
    auto value = contents.substr(head, cursor - head);
    return to_number(value, is_floating_point, Base);
  }
  inline string_view_type lex_identifier() {
    while (std::isalnum(peek()) ||
           tolerable_chars.find(peek()) != string_view_type::npos) {
      get();
    }
    // 123_abc
    //       ^ cursor position
    return {contents.data() + head, cursor - head};
  }

private:
  auto to_number(const string_view_type value,
                 const bool isFloating,
                 const int Base) -> std::optional<number_value_t> {
    auto realValStr = value;
    if (value.size() >= 2 && value[0] == '0' && is_valid_base(value[1]))
      realValStr = value.substr(2);

    std::from_chars_result res;
    if (isFloating) {
      if (Base != 10) {
        Println("Only base 10 is supported for floating point");
        return {};
      }
      long double number;
      res = std::from_chars(
          realValStr.data(), realValStr.data() + realValStr.size(), number);
      if (res.ec == std::errc())
        return {{number}};
    } else {
      long long number;
      res = std::from_chars(realValStr.data(),
                            realValStr.data() + realValStr.size(),
                            number,
                            Base);
      if (res.ec == std::errc())
        return {{number}};
    }
    Println("Unable to convert string '{0}' to number: at {1}, "
            "error: {2}",
            realValStr,
            res.ptr,
            std::make_error_code(res.ec).message());
    return {};
  }

  /// @brief lookaheads; we have only consumed the character before the cursor
  char_t peek(const size_t offset = 0) const {
    if (is_at_end(offset))
      return 0; // equivalent to '\0'
    return contents[cursor + offset];
  }
  /// @brief get current character and advance the cursor
  const char_t &get(const size_t offset = 1) {
    AC_PRECONDITION(cursor < contents.size(), "cursor out of bounds")
    AC_POSTCONDITION(cursor <= contents.size(), "cursor out of bounds")
    auto &c = contents[cursor];
    cursor += offset;
    return c;
  }

  /// @brief advance the cursor if the character is the expected character
  /// @return true if the character is the expected character and the cursor is
  /// advanced, false otherwise
  bool advance_if_is(const char_t expected) {
    if (is_at_end() || contents[cursor] != expected)
      return false;
    cursor++;
    return true;
  }

  /// @brief advance the cursor if the predicate is true
  /// @tparam Predicate the predicate to check
  /// @param predicate the predicate to check
  /// @return true if the predicate is true and the cursor is advanced, false
  template <typename Predicate> bool advance_if(Predicate &&predicate) {
    static_assert(std::invocable<Predicate, char_t> &&
                  std::convertible_to<Predicate, bool>);
    if (is_at_end() ||
        !std::invoke(std::forward<Predicate>(predicate), contents[cursor]))
      return false;
    cursor++;
    return true;
  }

  static constexpr auto is_valid_base(const char c) noexcept -> bool {
    return c == 'x' || c == 'X' || c == 'b' || c == 'B' || c == 'o' ||
           c == 'O' || c == 'd' || c == 'D';
  }
  static constexpr auto is_valid_digit_of_base(const char c,
                                               const int base) noexcept
      -> bool {
    switch (base) {
    case 2:
      return c == '0' || c == '1';
    case 8:
      return c >= '0' && c <= '7';
    case 10:
      return std::isdigit(c);
    case 16:
      return std::isxdigit(c);
    default:
      return false;
    }
  }

private:
  /// @brief head of a token
  size_type head = 0;
  /// @brief current cursor position
  size_type cursor = 0;
  /// @brief the contents of the file
  string_type contents = string_type();
  /// @brief current source line number
  uint_least32_t current_line = 1;
  /// @brief error count
  uint_least32_t error_count = 0;
};
class Grammar : public Printable {
  using enum Token::Type;
  static constexpr auto copyToken = [](const Token &token) constexpr noexcept {
    return token.copy();
  };

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

    auto to_string(FormatPolicy = FormatPolicy::kDefault) const {
      return lhs.to_string(FormatPolicy::kBrief)
          .append(" -> ")
          .append_range( //
              std::ranges::views::join_with(
                  rhs //
                      | std::ranges::views::transform([](auto &&alt) {
                          return std::ranges::views::join_with(
                              alt                                  //
                                  | std::ranges::views::transform( //
                                        [](auto &&sym) {
                                          return sym.to_string(
                                              FormatPolicy::kBrief);
                                        }),
                              ' ') //
                              ;
                        }),
                  std::string_view(" | ")) //
              // ^^^ workaround, pass const char* seems cause issue
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
  void _direct_lr(Piece &A,
                  const Piece::rhs_t &rhsElems,
                  const Piece::rhs_t &nonRecRhsElems) {
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
    for (auto &beta : nonRecRhsElems) {
      auto &b = new_A_rhs.emplace_back();
      b.reserve(beta.size() + 1);
      for (auto &t : beta)
        b.emplace_back(t.copy());
      b.emplace_back(newPiece.lhs.copy());
    }
    A.rhs = std::move(new_A_rhs);

    // A' -> alpha A' | epsilion
    for (auto &alpha : rhsElems) {
      auto &a = newPiece.rhs.emplace_back();
      a.reserve(alpha.size() + 1);
      for (const auto &t : alpha)
        a.emplace_back(t.copy());
      a.emplace_back(newPiece.lhs.copy());
    }
    newPiece.rhs.emplace_back().emplace_back(Token::Identifier(epsilon, 0));

    pieces.emplace_back(std::move(newPiece));
  }
  // eliminate direct left recursion for A
  Status _analyze_lr(Piece &A) {
    Piece::rhs_t nonRecRhsElems;
    Piece::rhs_t recRhsElems; // store alpha (without leading A)
    // for readability I choose not to remove braces and got alpha and beta as
    // aliases (, though I really want to write tham one line).
    for (auto &rhsElem : A.rhs) {
      if (rhsElem.front().lexeme() == A.lhs.lexeme()) {

        auto &alpha = recRhsElems.emplace_back();
        std::ranges::transform(rhsElem | std::ranges::views::drop(1),
                               std::back_inserter(alpha),
                               copyToken);
      } else {
        auto &beta = nonRecRhsElems.emplace_back();
        std::ranges::transform(rhsElem, std::back_inserter(beta), copyToken);
      }
    }
    if (recRhsElems.empty())
      // no direct left recursion
      return OkStatus();

    if (nonRecRhsElems.empty())
      // A -> A
      return ResourceExhaustedError("infinite loop");

    // create A'
    _direct_lr(A, recRhsElems, nonRecRhsElems);
    return OkStatus();
  }
  void _indirect_lr(Piece &A, const Piece &B) const {
    Piece::rhs_t new_rhs;
    for (auto &rhsElem : A.rhs) {
      if ((rhsElem.front().lexeme() == B.lhs.lexeme())) {
        // A -> B gamma  =>  substitute B -> delta into A
        for (const auto &delta : B.rhs) {

          auto &combined = new_rhs.emplace_back();

          combined.reserve(delta.size() + (rhsElem.size() - 1));
          std::ranges::transform(
              delta, std::back_inserter(combined), copyToken);
          std::ranges::transform(rhsElem | std::ranges::views::drop(1),
                                 std::back_inserter(combined),
                                 copyToken);
        }
      } else {
        auto &kept = new_rhs.emplace_back();
        kept.reserve(rhsElem.size());
        std::ranges::transform(rhsElem, std::back_inserter(kept), copyToken);
      }
    }
    A.rhs = std::move(new_rhs);
  }
  static Status postValidation(const std::vector<Token> &tokens) {
    if (tokens.size() == 1) {
      AC_RUNTIME_ASSERT(tokens.back().is_type(Token::Type::kEndOfFile))
      Println("nothing to do");
      return OkStatus();
    }
    if (std::ranges::any_of(tokens, [&](const Token &token) {
          // allowed type in this Left Recursion Grammar.
          using enum Token::Type;
          const auto invalid = false; // workaround
                                      // std::ranges::all_of(Grammar::allowed,
                                      // [&token](auto &&type) {
                                      //   return !token.is_type(type);
                                      // });
          if (invalid)
            Println(R"('{1}' Contains non-allowed type '{0}')",
                    token.type_str(),
                    token.to_string(FormatPolicy::kBrief));
          return invalid;
        })) {
      Println(stderr, "Validation failure.");
      return UnimplementedError("Grammar contains non-allowed token types.");
    }
    Println("Lex process successfully finished.");
    return OkStatus();
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

  static StatusOr<Grammar> parse(std::vector<Token> &&tokens) {
    if (auto status = postValidation(tokens); !status)
      return status;

    constexpr auto lineSeperator = [](auto &&a, auto &&b) {
      return a.line() == b.line();
    };
    constexpr auto validSep = [](auto &&pair) {
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
    constexpr auto segmentsSep = [](auto &&lhs, auto &&rhs) {
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
    grammar.post_parse(std::move(lines));

    return {std::move(grammar)};
  }
  void post_parse(auto &&lines) {

    for (auto &&l : lines) {
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
constexpr auto Token::token_type_str(const Type type) -> std::string_view {
  using namespace std::string_view_literals;
  switch (type) {
  case Type::kMonostate:
    return "Monostate"sv;
  case Type::kLeftParen:
    return "LeftParen"sv;
  case Type::kRightParen:
    return "RightParen"sv;
  case Type::kLeftBrace:
    return "LeftBrace"sv;
  case Type::kRightBrace:
    return "RightBrace"sv;
  case Type::kComma:
    return "Comma"sv;
  case Type::kDot:
    return "Dot"sv;
  case Type::kMinus:
    return "Minus"sv;
  case Type::kPlus:
    return "Plus"sv;
  case Type::kSemicolon:
    return "Semicolon"sv;
  case Type::kSlash:
    return "Slash"sv;
  case Type::kAmpersand:
    return "Ampersand"sv;
  case Type::kStar:
    return "Star"sv;
  case Type::kBang:
    return "Bang"sv;
  case Type::kBangEqual:
    return "BangEqual"sv;
  case Type::kEqual:
    return "Equal"sv;
  case Type::kEqualEqual:
    return "EqualEqual"sv;
  case Type::kGreater:
    return "Greater"sv;
  case Type::kGreaterEqual:
    return "GreaterEqual"sv;
  case Type::kLess:
    return "Less"sv;
  case Type::kLessEqual:
    return "LessEqual"sv;
  case Type::kIdentifier:
    return "Identifier"sv;
  case Type::kString:
    return "String"sv;
  case Type::kNumber:
    return "Number"sv;
  case Type::kLexError:
    return "LexError"sv;
  case Type::kEndOfFile:
    return "EndOfFile"sv;
  case Type::kLeftArrow:
    return "LeftArrow"sv;
  case Type::kBitwiseOr:
    return "BitwiseOr"sv;
  case Type::kSquareBracketOpen:
    return "SquareBracketOpen"sv;
  case Type::kSquareBracketClose:
    return "SquareBracketClose"sv;
  default:
    break;
  }
  return "Unknown"sv;
}
constexpr auto Token::token_type_operator() const -> std::string_view {
  using namespace std::string_view_literals;
  switch (type_) {
  case Type::kLeftParen:
    return "("sv;
  case Type::kRightParen:
    return ")"sv;
  case Type::kLeftBrace:
    return "{"sv;
  case Type::kRightBrace:
    return "}"sv;
  case Type::kComma:
    return ","sv;
  case Type::kDot:
    return "."sv;
  case Type::kMinus:
    return "-"sv;
  case Type::kPlus:
    return "+"sv;
  case Type::kSemicolon:
    return "sv;"sv;
  case Type::kSlash:
    return "/"sv;
  case Type::kAmpersand:
    return "&"sv;
  case Type::kStar:
    return "*"sv;
  case Type::kBang:
    return "!"sv;
  case Type::kBangEqual:
    return "!="sv;
  case Type::kEqual:
    return "="sv;
  case Type::kEqualEqual:
    return "=="sv;
  case Type::kGreater:
    return ">"sv;
  case Type::kGreaterEqual:
    return ">="sv;
  case Type::kLess:
    return "<"sv;
  case Type::kLessEqual:
    return "<="sv;
  case Type::kLeftArrow:
    return "->"sv;
  case Type::kBitwiseOr:
    return "|"sv;
  case Type::kSquareBracketOpen:
    return "["sv;
  case Type::kSquareBracketClose:
    return "]"sv;
  case Type::kMonostate:
    [[fallthrough]];
  case Type::kIdentifier:
    [[fallthrough]];
  case Type::kString:
    [[fallthrough]];
  case Type::kNumber:
    [[fallthrough]];
  case Type::kLexError:
    [[fallthrough]];
  case Type::kEndOfFile:
    break;
    break;
  }
  return lexeme_;
}
} // namespace accat::auxilia
