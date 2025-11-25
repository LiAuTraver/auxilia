#pragma once

#include <concepts>
#include <cstddef>
#include <functional>
#include <optional>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>
#include <ranges>

#include "accat/auxilia/base/config.hpp"
#include "accat/auxilia/base/format.hpp"

namespace accat::auxilia {
template <typename KeyT = std::string,
          typename ValueT = void,
          typename CharT = typename KeyT::value_type,
          typename CharTHasher = std::hash<CharT>,
          typename CharTEq = std::equal_to<CharT>>
class Trie : Printable {
public:
  Trie() = default;
  Trie(Trie &&) noexcept = default;
  Trie &operator=(Trie &&) noexcept = default;
  Trie(const Trie &) = delete;
  Trie &operator=(const Trie &) = delete;

public:
  using fake_value_type = ValueT;
  using node_key_type = CharT;
  using hasher = CharTHasher;
  using key_type = KeyT;
  using key_equal = CharTEq;
  using mapped_type = std::conditional_t<std::is_void_v<fake_value_type>,
                                         Monostate,
                                         fake_value_type>;
  using value_type = std::pair<const key_type, mapped_type>;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using reference = value_type &;
  using const_reference = const value_type &;
  using pointer = value_type *;
  using const_pointer = const value_type *;

private:
  struct Node {
    friend class Trie;

  public:
    Node() = default;
    Node(Node &&) noexcept = default;
    Node &operator=(Node &&) noexcept = default;
    Node(const Node &) = delete;
    Node &operator=(const Node &) = delete;

  private:
    std::optional<mapped_type> value_;
    std::unordered_map<node_key_type, Node, hasher, key_equal> children_;

  public:
    bool has_value() const noexcept { return value_.has_value(); }
    bool is_empty() const noexcept { return !has_value() && children_.empty(); }
    auto &value(this auto &&self) {
      AC_PRECONDITION(self.has_value(), "Bad optional access")
      return self.value_.value();
    }
    auto &children(this auto &&self) { return self.children_; }
  };
  struct from_lcp_result {
    key_type path;
    const Node *node;
  };
  AC_STATIC_ASSERT(std::is_aggregate_v<from_lcp_result>);
  Node root_;

private:
  template <typename V>
  auto do_insert(const key_type &key, V &&value, const bool doOverride)
      -> std::pair<std::optional<mapped_type> &, bool> {
    static_assert(!std::is_void_v<ValueT> ||
                      std::is_same_v<std::remove_cvref_t<V>, mapped_type>,
                  "For void value_type, pass std::monostate");

    Node *current = &root_;
    for (const CharT &ch : key | std::views::take(key.size() - 1)) {
      current = &current->children_[ch];
    }
    bool isNew = !current->children_.contains(key.back());
    current = &current->children_[key.back()];

    bool isValueless = !current->has_value();
    if (isValueless || doOverride) {
      current->value_ = std::forward<V>(value);
    }

    return {current->value_, isValueless};
  }

  bool do_erase(Node *current, const key_type &key, size_t depth) {
    if (depth == key.size()) {
      if (!current->has_value())
        return false;

      current->value_.reset();
      return current->is_empty();
    }

    auto it = current->children_.find(key[depth]);
    if (it == current->children_.end()) {
      return false;
    }

    if (do_erase(&it->second, key, depth + 1)) {
      // should delete its child
      current->children_.erase(it);
    }

    return current->is_empty();
  }

public:
  auto begin(this auto &&self) { return self.root_.children.begin(); }
  auto end(this auto &&self) { return self.root_.children.end(); }
  auto cbegin(this auto &&self) { return self.root_.children.cbegin(); }
  auto cend(this auto &&self) { return self.root_.children.cend(); }

public:
  auto insert(const key_type &key, auto &&...value) {
    if constexpr (std::is_void_v<ValueT>) {
      static_assert(sizeof...(value) == 0,
                    "void value_type doesn't accept values");
      return do_insert((key), std::monostate{}, false);
    } else {
      static_assert(sizeof...(value) <= 1,
                    "insert accepts at most one value argument");
      if constexpr (sizeof...(value) == 1) {
        return do_insert((key), std::forward<decltype(value)>(value)..., false);
      } else {
        return do_insert((key), mapped_type{}, false);
      }
    }
  }

  auto find(const key_type &key) const noexcept [[clang::lifetimebound]]
  -> const Node * {
    const auto *current = &root_;
    for (const auto &ch : key) {
      auto it = current->children_.find(ch);
      if (it == current->children_.end()) {
        return nullptr;
      }
      current = &it->second;
    }
    return current->has_value() ? current : nullptr;
  }

  auto contains(const key_type &key) const noexcept {
    return find(key) != nullptr;
  }

  auto insert_or_assign(const key_type &key, auto &&...value) {
    if constexpr (std::is_void_v<ValueT>) {
      static_assert(sizeof...(value) == 0,
                    "void value_type doesn't accept values");
      return do_insert((key), std::monostate{}, true);
    } else {
      static_assert(sizeof...(value) <= 1,
                    "insert_or_assign accepts at most one value argument");
      if constexpr (sizeof...(value) == 1) {
        return do_insert((key), std::forward<decltype(value)>(value)..., true);
      } else
        return do_insert((key), mapped_type{}, true);
    }
  }

  auto assign_range(auto &&range) {
    for (auto &&elem : range) {
      if constexpr (std::is_void_v<ValueT>) {
        do_insert(elem, mapped_type{}, true);
      } else {
        auto &&[key, value] = elem;
        insert_or_assign(key, value);
      }
    }
  }

  bool erase(const key_type &key) { return do_erase(&root_, key, 0); }

  auto &root(this auto &&self) noexcept { return self.root_; }

  auto longest_common_prefix() const -> from_lcp_result {
    // track one factoring candidate (longest prefix with branching).
    key_type path;
    key_type lcp;
    const Node *lcpNode = nullptr;

    // dfs to find deepest branching node.
    const auto dfs = [&](this auto &&self, const Node *node) -> void {
      const bool branching =
          (node->children().size() > 1)
          // branching, e.g, when iterating `app` in [`application`,`apple`]
          || (node->has_value() && !node->children().empty());
      // it itself is a value and also has branch, ^^^
      // e.g,  when iterating `app` in [`app` and `apple`]

      if (branching && path.size() > lcp.size()) {
        lcp = path;
        lcpNode = node;
      }
      for (const auto &[elemKey, child] : node->children()) {
        // string only has `push_back` not `emplace_back`
        // yet no performance boost to use `emaplce_back` with lvalue.
        path.push_back(elemKey);
        self(&child);
        path.pop_back();
      }
    };

    dfs(&root_);
    return {std::move(lcp), lcpNode};
  }

  /// this is a heavy operation, use it sparely.
  /// currently only used for left factoring in Grammar.
  /// @pre the @param node should be in this Trie.
  auto collect(const Node *node,
               const std::optional<node_key_type> defaultKey) const {
    std::vector<key_type> allKeys;

    key_type path;

    const auto traverse = [&](this auto &&self, const Node *node) -> void {
      if (node->has_value()) {
        // pathAccum is a full original production,
        // e.g., `app` in [`app`, `apple`].
        auto &newPath = allKeys.emplace_back();
        // A -> app | apple
        // => A -> app A'
        // with A' -> epsilon | le
        if (path.size() > 0) {
          newPath.assign_range(path);
        } else if (defaultKey.has_value()) {
          newPath.emplace_back(*defaultKey);
        }
      }
      for (const auto &[elemKey, child] : node->children()) {
        path.push_back(elemKey);
        self(&child);
        path.pop_back();
      }
    };

    traverse(node);

    return allKeys;
  }
  auto iterate(const Node *node, auto &&fun = std::identity{}) const {
    static_assert(std::invocable<decltype(fun), const Node *, key_type &>,
                  "fun should be callable with (const Node*, key_type&)");
    key_type path;

    const auto traverse = [&](this auto &&self, const Node *current) -> void {
      std::invoke(std::forward<decltype(fun)>(fun), current, std::ref(path));

      for (const auto &[elemKey, child] : current->children()) {
        path.push_back(elemKey);
        self(&child);
        path.pop_back();
      }
    };

    traverse(node);
  }

  auto to_string(FormatPolicy) const { AC_TODO_() }
};
} // namespace accat::auxilia
