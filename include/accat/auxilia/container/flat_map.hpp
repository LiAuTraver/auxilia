#pragma once

#include "accat/auxilia/base/config.hpp"

namespace accat::auxilia {
/// basic implementation of C++23's flat_map, since it's not available currently
/// in std. this is not amied at full compliance with the standard, just for my
/// own use cases.
template <typename KeyT,
          typename ValueT,
          typename Compare = std::less<KeyT>,
          typename KeyContainer = std::vector<KeyT>,
          typename MappedContainer = std::vector<ValueT>>
class flat_map {
  template <typename, typename, typename, typename, typename>
  friend class flat_map;
  static_assert(std::is_same_v<typename KeyContainer::value_type, KeyT>,
                "KeyContainer's value_type must be the same as KeyT");
  static_assert(std::is_same_v<typename MappedContainer::value_type, ValueT>,
                "MappedContainer's value_type must be the same as ValueT");
  static_assert(!std::is_same_v<KeyContainer, std::vector<bool>>,
                "vector<bool> is not a sequence container");
  static_assert(!std::is_same_v<MappedContainer, std::vector<bool>>,
                "vector<bool> is not a sequence container");
  template <bool IsConst> class PairIterator;
  class Comparator;
  struct Containers;

public:
  using key_type = KeyT;
  using mapped_type = ValueT;
  using value_type = std::pair<key_type, mapped_type>;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using key_container_type = KeyContainer;
  using mapped_container_type = MappedContainer;
  using reference = std::pair<const key_type &, mapped_type &>;
  using const_reference = std::pair<const key_type &, const mapped_type &>;
  using iterator = PairIterator<false>;
  using const_iterator = PairIterator<true>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;
  using key_compare = std::type_identity_t<Compare>;
  using value_compare = Comparator;

private:
  Containers myContainers;
  AC_NO_UNIQUE_ADDRESS key_compare myKeyComparator;

public:
  flat_map(const flat_map &) = delete;
  flat_map &operator=(const flat_map &) = delete;

public:
  void clear() noexcept {
    myContainers.myKeys.clear();
    myContainers.myValues.clear();
  }
  constexpr key_compare key_comp() const noexcept { return myKeyComparator; }
  constexpr value_compare value_comp() const noexcept {
    return value_compare(myKeyComparator);
  }
  const key_container_type &keys() const noexcept {
    return myContainers.myKeys;
  }
  const mapped_container_type &values() const noexcept {
    return myContainers.myValues;
  }
  // TODO: rest of function, insert, find, etc.
private:
  template <bool IsConst> class PairIterator {
    friend class flat_map;
    template <typename, typename, typename, typename, typename>
    friend class flat_map::PairIterator;
    using key_iterator = typename KeyContainer::const_iterator;
    using mapped_iterator =
        std::conditional_t<IsConst,
                           typename MappedContainer::const_iterator,
                           typename MappedContainer::iterator>;
    using reference = std::conditional_t<IsConst,
                                         typename flat_map::const_reference,
                                         typename flat_map::reference>;
    key_iterator myKeyIt;
    mapped_iterator myMappedIt;

  public:
    // this doesn't sound true; the `reference` is not a true reference type
    using iterator_category = std::random_access_iterator_tag;
    using value_type = typename flat_map::value_type;
    using difference_type = typename flat_map::difference_type;

  public:
    reference operator*() const { return reference(*myKeyIt, *myMappedIt); }
    auto operator->() const {
      /// proxy object to return a pointer to a pair-like object
      struct proxy {
        reference myRef;
        reference *operator->() { return std::addressof(myRef); }
      };
      return proxy{**this};
    }
    PairIterator &operator++() {
      ++myKeyIt;
      ++myMappedIt;
      return *this;
    }
    PairIterator &operator--() {
      --myKeyIt;
      --myMappedIt;
      return *this;
    }
    PairIterator operator++(int) {
      PairIterator temp = *this;
      ++*this;
      return temp;
    }
    PairIterator operator--(int) {
      PairIterator temp = *this;
      --*this;
      return temp;
    }
    PairIterator &operator+=(difference_type n) {
      myKeyIt += n;
      myMappedIt += n;
      return *this;
    }
    PairIterator &operator-=(difference_type n) {
      myKeyIt -= n;
      myMappedIt -= n;
      return *this;
    }
    friend PairIterator operator+(const PairIterator &it,
                                  const difference_type n) {
      PairIterator temp = it;
      temp += n;
      return temp;
    }
    friend PairIterator operator+(const difference_type n,
                                  const PairIterator &it) {
      return it + n;
    }
    friend PairIterator operator-(const PairIterator &it,
                                  const difference_type n) {
      PairIterator temp = it;
      temp -= n;
      return temp;
    }
    friend difference_type operator-(const PairIterator &lhs,
                                     const PairIterator &rhs) {
      return lhs.myKeyIt - rhs.myKeyIt;
    }
    // only compares keys
    friend bool operator==(const PairIterator &lhs, const PairIterator &rhs) {
      return lhs.myKeyIt == rhs.myKeyIt;
    }
    // ditto
    friend auto operator<=>(const PairIterator &lhs, const PairIterator &rhs) {
      return lhs.myKeyIt <=> rhs.myKeyIt;
    }
  };
  class Comparator {
    friend class flat_map;
    AC_NO_UNIQUE_ADDRESS key_compare myComparator;
    constexpr Comparator(key_compare c) noexcept : myComparator(c) {}

  public:
    constexpr bool operator()(const_reference lhs, const_reference rhs) const
        noexcept(noexcept(myComparator(
            std::declval<const key_type &>(),
            std::declval<const key_type &>()))) // this `noexcept` is not in
                                                // standard
    {
      return myComparator(lhs.first, rhs.first);
    }
  };
  struct Containers {
    KeyContainer myKeys;
    MappedContainer myValues;
  };
};
} // namespace accat::auxilia
