#pragma once

#include "./config.hpp"
#include "./format.hpp"
#include "./StatusOr.hpp"

namespace accat::auxilia::program_options {

EXPORT_AUXILIA class Option;
/* not necessary to export */ class Parser;

EXPORT_AUXILIA inline Parser *Global(string_view, string_view = "unknown"sv);
EXPORT_AUXILIA inline Parser Local(string_view, string_view = "unknown"sv);
EXPORT_AUXILIA inline Parser *find(string_view);
EXPORT_AUXILIA inline Parser &get(string_view);

namespace details {
inline auto _get_global_parsers() -> std::vector<Parser> &;
}

class Option {
public:
  // name, fullname, shortname and description must be compiled-time constant
  constexpr Option(const string_view name,
                   const string_view fullname,
                   const string_view shortname = "",
                   const string_view desc = "")
      : name_(name), fullname_(fullname), shortname_(shortname), desc_(desc) {}

  Option(const Option &other) = delete;
  auto operator=(const Option &other) -> Option & = delete;

  Option(Option &&other) noexcept
      : name_(other.name_), fullname_(other.fullname_),
        shortname_(other.shortname_), desc_(other.desc_),
        values_(std::move(other.values_)), required_(other.required_),
        nargs_(other.nargs_) {}
  auto operator=(Option &&other) noexcept -> Option & {
    if (this == &other)
      return *this;
    name_ = other.name_;
    fullname_ = other.fullname_;
    shortname_ = other.shortname_;
    desc_ = other.desc_;
    values_ = std::move(other.values_);
    required_ = other.required_;
    nargs_ = other.nargs_;
    return *this;
  }

public:
  auto name() const -> const string_view { return name_; }
  auto fullname() const -> const string_view { return fullname_; }
  auto shortname() const -> const string_view { return shortname_; }
  auto description(this auto &&self) -> string_view { return self.desc_; }

  auto &required(const bool is_required = true) {
    required_ = is_required;
    return *this;
  }

  /// @param num_args 0 for a flag, 1 for a single value, '+' for one or more.
  auto &nargs(const char num_args) {
    nargs_ = num_args;
    return *this;
  }

  template <typename... Valty>
  auto &default_value(Valty &&...values)
    requires(std::is_convertible_v<std::remove_cvref_t<Valty>, string> && ...)
  {
    (values_.emplace_back(std::forward<Valty>(values)), ...);
    return *this;
  }

  auto values() const -> std::span<const string> { return values_; }

  /// @pre single value
  auto value() const -> StatusOr<string_view> {
    if (values_.empty()) {
      return NotFoundError(format("Option {} has no value.", name_));
    }
    if (values_.size() > 1) {
      return InvalidArgumentError(
          format("Option {} does not have a single value.", name_));
    }
    return StatusOr<string_view>{values_[0]};
    // ^^^^^^^^^^^^^^^^^^^^^^^^^ workaround for MSCV C4927: illegal conversion;
    // more than one user-defined conversion has been implicitly applied
  }

private:
  string help() const {
    string help_text = "  ";
    if (!shortname_.empty()) {
      help_text += format("{}, ", shortname_);
    }
    help_text += format("{}", fullname_);
    if (nargs_ != 0) {
      help_text += " <value>";
    }
    help_text += format("\t\t{}\n", desc_);
    return help_text;
  }

private:
  friend class Parser;
  // help
  string_view name_;
  // --help
  string_view fullname_;
  // -h
  string_view shortname_;
  // show message
  string_view desc_;
  std::vector<string> values_;
  bool required_ = false;
  // 0: flag, 1 or '1': single value, '+': one or more
  char nargs_ = 0;
};

class Parser {
  string_view program_name_;
  string_view program_version_;
  std::unordered_map<string_view, Option> options_;
  std::vector<string> error_msgs_;
  std::vector<string_view> positional_args_;

  Parser() = default;
  Parser(const Parser &other) = delete;
  auto operator=(const Parser &other) -> Parser & = delete;

  friend inline auto Local(const string_view program_name,
                           const string_view version)
      -> Parser {
    return Parser(program_name, version);
  }
  /// @brief Create a global parser, whose lifetime is the entire program.
  /// @returns nullptr if the parser already exists.
  friend inline auto Global(string_view program_name, string_view version)
      -> Parser * {
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

  AC_CONSTEXPR23 Parser(const string_view program_name,
                   const string_view program_version)
      : program_name_(program_name), program_version_(program_version) {}

public:
  auto add_option(Option &&opt) -> Option & {
    // dummy option, return when the input option is invalid
    // just make chaining more convenient, since there are no `?.` in C++.
    static Option garbage_opt{"", ""};

    // handle `help` and `version` specially
    if (opt.name() == "help" || opt.name() == "version") {
      error_msgs_.emplace_back(
          format("Option name '{}' is reserved. Please use another name. "
                 "This option will be ignored.",
                 opt.name()));
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
  auto &add_option(string_view name, Properties &&...props) {
    return add_option(Option{name, std::forward<Properties>(props)...});
  }

  auto program_name() const -> const string_view { return program_name_; }
  auto error() const -> size_t { return error_msgs_.size(); }
  auto error_messages() const -> std::span<const string> { return error_msgs_; }
  auto positional_arguments() const -> std::span<const string_view> {
    return positional_args_;
  }

  auto get_option(const string_view name) const -> const Option * {
    if (auto it = options_.find(name); it != options_.end()) {
      return &it->second;
    }
    return nullptr;
  }

private:
  auto is_conflicting(const Option &opt) const -> const Option * {
    for (const auto &existing_opt : options_ | std::views::values) {
      if (opt.fullname() == existing_opt.fullname() &&
          !opt.fullname().empty()) {
        return &existing_opt;
      }
      if (opt.shortname() == existing_opt.shortname() &&
          !opt.shortname().empty()) {
        return &existing_opt;
      }
    }
    return nullptr;
  }

public:
  auto parse(const std::span<const string_view> args) -> bool {
    if (!error_msgs_.empty()) {
      return false;
    }

    std::unordered_map<string_view, Option *> name_map;
    std::unordered_map<string_view, Option *> short_name_map;
    for (auto &opt : options_ | std::views::values) {
      name_map[opt.fullname()] = &opt;
      if (!opt.shortname().empty()) {
        short_name_map[opt.shortname()] = &opt;
      }
    }

    for (size_t i = 0; i < args.size(); ++i) {
      string_view arg = args[i];
      Option *opt = nullptr;

      if (arg == "--help" or arg == "-h") {
        help();
        exit(0); // workaround
      } else if (arg == "--version" or arg == "-v") {
        version();
        exit(0); // workaround
      } else if (arg.starts_with("--")) {
        opt = name_map.contains(arg) ? name_map[arg] : nullptr;
      } else if (arg.starts_with('-')) {
        opt = short_name_map.contains(arg) ? short_name_map[arg] : nullptr;
      }

      if (opt) {
        opt->values_.clear();
        if (opt->nargs_ == 1 || opt->nargs_ == '1' || opt->nargs_ == '+') {
          if (i + 1 < args.size()) {
            i++;
            opt->values_.emplace_back(args[i]);
          } else {
            error_msgs_.emplace_back(
                format("Option {} requires an argument.", arg));
          }
        } else { // flag
          opt->values_.emplace_back("true");
        }
      } else {
        positional_args_.emplace_back(arg);
      }
    }

    for (const auto &opt : options_ | std::views::values) {
      if (opt.required_ && opt.values_.empty()) {
        error_msgs_.emplace_back(
            format("Required option {} is missing.", opt.fullname()));
      }
    }

    return error_msgs_.empty();
  }
  auto help(std::ostream &os = std::cout) const -> void {
    os << format("Usage: {} [options]\n\nOptions:\n", program_name_);
    // Add built-in help and version
    os << R"(
  -h, --help        Show this help message and exit
  -v, --version     Show program's version number and exit
)"_raw;

    for (const auto &opt : options_ | std::views::values) {
      os << opt.help();
    }
  }
  auto version(std::ostream &os = std::cout) const -> void {
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
inline auto find(const string_view program_name) -> Parser * {
  auto &parsers = details::_get_global_parsers();
  for (auto &parser : parsers) {
    if (parser.program_name() == program_name)
      return &parser;
  }
  return nullptr;
}
/// @pre The parser must exist, otherwise throws.
inline auto get(string_view program_name) -> Parser & {
  if (auto p = find(program_name))
    return *p;

  dbg_break;
  throw std::runtime_error(format("Parser does not exist: {}", program_name));
}
} // namespace accat::auxilia::program_options
