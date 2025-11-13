#pragma once

#include <cstddef>
#include <cstdlib>
#include <format>
#include <string>
#include <string_view>
#include <span>
#include <unordered_map>
#include <utility>
#include <type_traits>
#include <ranges>
#include <iostream>
#include <vector>

#include "./format.hpp"
#include "./StatusOr.hpp"
#include "Status.hpp"

namespace accat::auxilia::program_options {

EXPORT_AUXILIA class Option;
/* not necessary to export */ class Parser;

EXPORT_AUXILIA inline Parser *Global(std::string_view,
                                     std::string_view = "unknown");
EXPORT_AUXILIA inline Parser Local(std::string_view,
                                   std::string_view = "unknown");
EXPORT_AUXILIA inline Parser *find(std::string_view);
EXPORT_AUXILIA inline Parser &get(std::string_view);

namespace details {
inline auto _get_global_parsers() -> std::vector<Parser> &;
}

class Option {
public:
  // name, fullname, shortname and description must be compiled-time constant
  constexpr Option(const std::string_view name,
                   const std::string_view shortname = "",
                   const std::string_view desc = "")
      : name_(name), shortname_(shortname), desc_(desc) {}

  Option(const Option &other) = delete;
  auto operator=(const Option &other) -> Option & = delete;

  Option(Option &&other) noexcept
      : name_(other.name_), shortname_(other.shortname_), desc_(other.desc_),
        values_(std::move(other.values_)), required_(other.required_),
        nargs_(other.nargs_) {}
  auto operator=(Option &&other) noexcept -> Option & {
    if (this == &other)
      return *this;
    name_ = other.name_;
    shortname_ = other.shortname_;
    desc_ = other.desc_;
    values_ = std::move(other.values_);
    required_ = other.required_;
    has_default_value_ = other.has_default_value_;
    nargs_ = other.nargs_;
    return *this;
  }

public:
  auto name() const -> const std::string_view { return name_; }
  auto shortname() const -> const std::string_view { return shortname_; }
  auto description(this auto &&self) -> std::string_view { return self.desc_; }

  auto &required(const bool is_required = true) {
    required_ = is_required;
    return *this;
  }

  /// @param num_args 0 for a flag, 1 for a single value, '+' for one or more.
  auto &nargs(const char num_args) {
    AC_PRECONDITION(num_args == 0 || num_args == 1 || // 1 and '1' are same
                        num_args == '1' || num_args == '+',
                    "num_args must be 0, 1, or '+'")
    nargs_ = num_args;
    return *this;
  }

  template <typename... Valty> auto &default_value(Valty &&...values) {
    static_assert(
        (std::is_convertible_v<std::remove_cvref_t<Valty>, std::string> && ...),
        "All default values must be convertible to std::string");
    (values_.emplace_back(std::forward<Valty>(values)), ...);
    has_default_value_ = true;
    return *this;
  }

  auto values() const -> std::span<const std::string> { return values_; }

  /// @pre single value
  auto value() const -> StatusOr<std::string_view> {
    if (values_.empty()) {
      return NotFoundError(format("Option {} has no value.", name_));
    }
    if (values_.size() > 1) {
      return InvalidArgumentError(
          format("Option {} does not have a single value.", name_));
    }
    return StatusOr<std::string_view>{values_[0]};
    // ^^^^^^^^^^^^^^^^^^^^^^^^^ workaround for MSCV C4927: illegal conversion;
    // more than one user-defined conversion has been implicitly applied
  }

private:
  std::string help() const {
    std::string help_text = "  ";
    if (!shortname_.empty()) {
      help_text += format("{}, ", shortname_);
    }
    help_text += format("{}", name_);
    if (nargs_ != 0) {
      help_text += " <value>";
    }
    help_text += format("\t\t{}\n", desc_);
    return help_text;
  }

private:
  friend class Parser;
  // --help
  std::string_view name_;
  // -h
  std::string_view shortname_;
  // show message
  std::string_view desc_;
  std::vector<std::string> values_;
  bool required_ = false;
  bool has_default_value_ = false;
  // 0: flag, 1 or '1': single value, '+': one or more
  char nargs_ = 0;
};

class Parser {
  std::string_view program_name_;
  std::string_view program_version_;
  std::unordered_map<std::string_view, Option> options_;
  std::vector<std::string> error_msgs_;
  std::vector<std::string_view> positional_args_;

  Parser() = default;
  Parser(const Parser &other) = delete;
  auto operator=(const Parser &other) -> Parser & = delete;

  /// @brief Create a local parser,
  ///        whose lifetime is limited to the current scope.
  /// @returns the parser object.
  friend inline auto Local(const std::string_view program_name,
                           const std::string_view version) -> Parser {
    return Parser(program_name, version);
  }
  /// @brief Create a global parser, whose lifetime is the entire program.
  /// @returns nullptr if the parser with the same name already exists.
  friend inline auto Global(std::string_view program_name,
                            std::string_view version) -> Parser * {
    if (find(program_name))
      return nullptr;
    auto &parsers = details::_get_global_parsers();
    parsers.emplace_back(program_name, version);
    return &parsers.back();
  }

public:
  Parser(Parser &&other) noexcept
      : program_name_(other.program_name_), options_(std::move(other.options_)),
        error_msgs_(std::move(other.error_msgs_)),
        positional_args_(std::move(other.positional_args_)) {}
  auto operator=(Parser &&other) noexcept -> Parser & {
    if (this == &other)
      return *this;
    program_name_ = other.program_name_;
    program_version_ = other.program_version_;
    options_ = std::move(other.options_);
    error_msgs_ = std::move(other.error_msgs_);
    positional_args_ = std::move(other.positional_args_);
    return *this;
  }

  Parser(const std::string_view program_name,
         const std::string_view program_version)
      : program_name_(program_name), program_version_(program_version) {}

public:
  auto add_option(Option &&opt) -> Option & {
    // dummy option, return when the input option is invalid
    // just make chaining more convenient, since there are no `?.` in C++.
    static Option garbage_opt{"", ""};

    // handle `help` and `version` specially
    if (check_reserved(opt)) {
      return garbage_opt;
    }
    // check conflicts
    if (auto *conflict = is_conflicting(opt)) {
      error_msgs_.emplace_back(format(
          "Option '{}' conflicts with existing option '{}'. This option will "
          "be ignored.",
          opt.name(),
          conflict->name()));
      return garbage_opt;
    }

    auto [it, inserted] =
        options_.try_emplace(opt.name(), std::forward<Option>(opt));
    if (!inserted)
      error_msgs_.emplace_back(format("Option already exists: {}", opt.name()));

    return it->second;
  }

  template <typename... Properties>
  auto &add_option(std::string_view name, Properties &&...props) {
    return add_option(Option{name, std::forward<Properties>(props)...});
  }

  auto program_name() const -> const std::string_view { return program_name_; }
  auto error() const -> size_t { return error_msgs_.size(); }
  auto error_messages() const -> std::span<const std::string> {
    return error_msgs_;
  }
  auto positional_arguments() const -> std::span<const std::string_view> {
    return positional_args_;
  }

  auto get_option(const std::string_view name) const [[clang::lifetimebound]]
  -> const Option * {
    if (auto it = options_.find(name); it != options_.end()) {
      return &it->second;
    }
    return nullptr;
  }

private:
  auto is_conflicting(const Option &opt) const -> const Option * {
    for (const auto &existing_opt : options_ | std::views::values) {
      if (opt.name() == existing_opt.name() && !opt.name().empty()) {
        return &existing_opt;
      }
      if (opt.shortname() == existing_opt.shortname() &&
          !opt.shortname().empty()) {
        return &existing_opt;
      }
    }
    return nullptr;
  }
  auto is_option(const std::string_view str) const {
    return str.starts_with("--") || str.starts_with('-');
  }
  auto check_reserved(const Option &opt) -> bool {
    if (opt.name() == "--help" || opt.name() == "--version") {
      error_msgs_.emplace_back(
          format("Option name '{}' is reserved. Please use another name. "
                 "This option will be ignored.",
                 opt.name()));
      return true;
    }
    if (opt.shortname() == "-h" || opt.shortname() == "-v") {
      error_msgs_.emplace_back(
          format("Option shortname '{}' is reserved. Please use another "
                 "shortname. This option will be ignored.",
                 opt.shortname()));
      return true;
    }
    return false;
  }

public:
  auto parse(const int argc, char **argv) {
    AC_PRECONDITION(argv, "argv is null")
    AC_PRECONDITION(argc > 0, "argc is not positive")
    std::vector<std::string_view> args;
    for (int i = 0; i < argc; ++i) {
      args.emplace_back(argv[i]);
    }
    return parse(args);
  }
  auto parse(const std::span<const std::string_view> args) -> bool {
    if (!error_msgs_.empty()) {
      return false;
    }

    std::unordered_map<std::string_view, Option *> name_map;
    std::unordered_map<std::string_view, Option *> short_name_map;
    for (auto &opt : options_ | std::views::values) {
      name_map[opt.name()] = &opt;
      if (!opt.shortname().empty()) {
        short_name_map[opt.shortname()] = &opt;
      }
    }

    for (size_t i = 0; i < args.size(); ++i) {
      std::string_view arg = args[i];
      Option *opt = nullptr;

      if (arg == "--help" or arg == "-h") {
        help();
        exit(0);
      } else if (arg == "--version" or arg == "-v") {
        version();
        exit(0);
      }

      if (arg.starts_with("--")) {
        opt = name_map.contains(arg) ? name_map[arg] : nullptr;
      } else if (arg.starts_with('-')) {
        opt = short_name_map.contains(arg) ? short_name_map[arg] : nullptr;
      }
      // else: positional argument
      if (!opt) {
        if (!is_option(arg))
          positional_args_.emplace_back(arg);
        else
          error_msgs_.emplace_back(format("Unknown option: {}", arg));
        continue;
      }
      if (!opt->has_default_value_ && !opt->values_.empty()) {
        error_msgs_.emplace_back(format(
            "Option {} specified multiple times. Default to override...", arg));
      }
      // TODO: has default value and specified multiple times.

      opt->values_.clear();

      if (opt->nargs_ == 0) {
        opt->values_.emplace_back("true");
        continue;
      }

      if (opt->nargs_ == 1 or opt->nargs_ == '1') {
        if (i + 1 < args.size() && !is_option(args[i + 1])) {
          i++;
          opt->values_.emplace_back(args[i]);
        } else {
          error_msgs_.emplace_back(
              format("Option {} requires an argument.", arg));
        }
        continue;
      }

      AC_RUNTIME_ASSERT(opt->nargs_ == '+', "nargs_ must be '+' at this point")

      while (i + 1 < args.size() && !is_option(args[i + 1])) {
        i++;
        opt->values_.emplace_back(args[i]);
      }
    }

    for (const auto &opt : options_ | std::views::values) {
      if (opt.required_ && opt.values_.empty()) {
        error_msgs_.emplace_back(
            format("Required option {} is missing.", opt.name()));
      }
    }

    return error_msgs_.empty();
  }
  auto help(std::ostream &os = std::cerr) const -> void {
    using namespace auxilia::literals;
    os << format("Usage: {} [options]\n\nOptions:\n", program_name_);
    os << R"(
  -h, --help        Show this help message and exit
  -v, --version     Show program's version number and exit
)"_raw;

    for (const auto &opt : options_ | std::views::values) {
      os << opt.help();
    }
  }
  auto version(std::ostream &os = std::cerr) const -> void {
    os << format("{} version {}\n", program_name_, program_version_);
  }
};
namespace details {
inline auto _get_global_parsers() -> std::vector<Parser> & {
  static std::vector<Parser> parsers;
  return parsers;
}
} // namespace details

/// @returns nullptr if the parser does not exist.
inline auto find(const std::string_view program_name) -> Parser * {
  auto &parsers = details::_get_global_parsers();
  for (auto &parser : parsers) {
    if (parser.program_name() == program_name)
      return &parser;
  }
  return nullptr;
}
/// @pre The parser must exist, otherwise throws.
inline auto get(std::string_view program_name) -> Parser & {
  if (auto p = find(program_name))
    return *p;

  AC_THROW_OR_DIE_(format("Parser does not exist: {}", program_name));
}
} // namespace accat::auxilia::program_options
