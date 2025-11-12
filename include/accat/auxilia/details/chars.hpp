#pragma once

#include <compare>
#include <cstddef>
#include <initializer_list>
#include <type_traits>
#include <iterator>
#include <string>
#include <string_view>

#include "./type_traits.hpp"

namespace accat::auxilia {
template <typename CharT, size_t N, typename Traits = std::char_traits<CharT>>
class basic_chars;
template <size_t N> using chars = basic_chars<char, N, std::char_traits<char>>;
template <size_t N>
using wchars = basic_chars<wchar_t, N, std::char_traits<wchar_t>>;
#ifdef __cpp_char8_t
template <size_t N>
using u8chars = basic_chars<char8_t, N, std::char_traits<char8_t>>;
#endif
template <size_t N>
using u16chars = basic_chars<char16_t, N, std::char_traits<char16_t>>;
template <size_t N>
using u32chars = basic_chars<char32_t, N, std::char_traits<char32_t>>;
/// @brief a tiny compile-time string literal wrapper;
///          essentially an owned string_view
template <typename CharT, size_t N, typename Traits> class basic_chars {
  template <typename CharU, size_t M, typename TraitsU>
  friend class basic_chars;

public:
  class iterator;
  class const_iterator;
  using value_type = CharT;
  using pointer = CharT *;
  using const_pointer = const CharT *;
  using reference = CharT &;
  using const_reference = const CharT &;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;
  using size_type = size_t;
  using difference_type = ptrdiff_t;

private:
  value_type myArr[N] = {};
  static constexpr auto real_size = N - 1;
  static constexpr auto npos = static_cast<size_type>(-1);

private:
  consteval auto assign_from(const value_type (&arr)[N]) noexcept {
    static_assert(std::is_trivially_destructible_v<basic_chars>);
    for_each_char([&](value_type &current, const size_type index) {
      Traits::assign(current, arr[index]);
    });
  }
  constexpr void for_each_char(this auto &&self, auto &&func) noexcept {
    for (size_type i = 0; i < self.real_size; ++i) {
      func(self.myArr[i], i);
    }
  }

public:
  consteval basic_chars() noexcept = default;
  constexpr basic_chars(const value_type ch) noexcept {
    static_assert(N == 2);
    Traits::assign(myArr[0], ch);
    Traits::assign(myArr[1], '\0');
  }
  consteval basic_chars(const value_type (&arr)[N]) noexcept {
    assign_from(arr);
  }
  consteval basic_chars(const basic_chars &other) noexcept {
    assign_from(other.myArr);
  }
  consteval basic_chars(basic_chars &&other) noexcept {
    assign_from(static_cast<decltype(other.myArr) &&>(other.myArr));
  }
  consteval basic_chars(std::initializer_list<value_type> ilist) noexcept {
    size_type i = 0;
    for (const auto &c : ilist) {
      if (i >= real_size)
        break;
      Traits::assign(myArr[i++], c);
    }
    for (; i < real_size; ++i) {
      Traits::assign(myArr[i], '\0');
    }
  }
  constexpr ~basic_chars() noexcept = default;

public:
  consteval basic_chars &operator=(const basic_chars &other) noexcept {
    assign_from(other.myArr);
    return *this;
  }
  consteval basic_chars &operator=(basic_chars &&other) noexcept {
    assign_from(static_cast<decltype(other.myArr) &&>(other.myArr));
    return *this;
  }
  consteval basic_chars &operator=(const value_type (&arr)[N]) noexcept {
    assign_from(arr);
    return *this;
  }
  constexpr bool operator==(const basic_chars &other) const noexcept {
    for (size_type i = 0; i < real_size; ++i) {
      if (!Traits::eq(myArr[i], other.myArr[i]))
        return false;
    }
    return true;
  }
  constexpr bool operator==(const value_type (&arr)[N]) const noexcept {
    for (size_type i = 0; i < real_size; ++i) {
      if (!Traits::eq(myArr[i], arr[i]))
        return false;
    }
    return true;
  }

public:
  constexpr auto size() const noexcept { return real_size; }
  constexpr auto length() const noexcept { return real_size; }
  constexpr auto &operator[](this auto &&self, size_type index) noexcept {
    return self.myArr[index];
  }
  constexpr auto &at(this auto &&self, const size_type index) noexcept {
    return self.myArr[index];
  }
  constexpr auto count(const value_type c) const noexcept {
    size_type total = 0;
    for_each_char([&](const value_type current, auto &&_) {
      if (Traits::eq(current, c))
        ++total;
    });
    return total;
  }
  constexpr auto begin(this auto &&self) noexcept { return iterator(&self, 0); }
  constexpr auto end(this auto &&self) noexcept {
    return iterator(&self, self.real_size);
  }
  constexpr auto cbegin() const noexcept { return const_iterator(this, 0); }
  constexpr auto cend() const noexcept {
    return const_iterator(this, this->real_size);
  }
  constexpr auto rbegin() noexcept { return reverse_iterator(end()); }
  constexpr auto rend() noexcept { return reverse_iterator(begin()); }
  constexpr auto rbegin() const noexcept {
    return const_reverse_iterator(cend());
  }
  constexpr auto rend() const noexcept {
    return const_reverse_iterator(cbegin());
  }
  constexpr auto crbegin() const noexcept {
    return const_reverse_iterator(cend());
  }
  constexpr auto crend() const noexcept {
    return const_reverse_iterator(cbegin());
  }
  constexpr void clear() noexcept {
    for_each_char([&](value_type &current, auto &&_) { current = '\0'; });
  }
  template <size_type pos, value_type c> constexpr auto &replace() noexcept {
    Traits::assign(myArr[pos], c);
    return *this;
  }
  constexpr auto &front(this auto &&self) noexcept { return self.myArr[0]; }
  constexpr auto &back(this auto &&self) noexcept {
    return self.myArr[self.real_size - 1];
  }
  constexpr auto &data() const noexcept { return myArr; }
  constexpr operator std::basic_string_view<value_type>() const noexcept {
    return std::basic_string_view<value_type>(myArr, real_size);
  }
  constexpr operator std::basic_string<value_type>() const noexcept {
    return std::basic_string<value_type>(myArr, real_size);
  }
  constexpr operator const value_type *() const noexcept
      [[clang::lifetimebound]] {
    return myArr;
  }
  constexpr auto contains(const value_type c) const noexcept {
    return count(c) > 0;
  }
  template <const value_type c> constexpr auto &fill() noexcept {
    for_each_char(
        [&](value_type &current, auto &&_) { Traits::assign(current, c); });
    return *this;
  }
  constexpr auto &reverse() noexcept {
    for (size_type i = 0; i < (real_size) / 2; ++i) {
      auto temp = myArr[i];
      Traits::assign(myArr[i], myArr[(real_size - 1) - i]);
      Traits::assign(myArr[(real_size - 1) - i], temp);
    }
    return *this;
  }
  template <size_t M>
  constexpr auto
  starts_with(const basic_chars<value_type, M> &prefix) const noexcept {
    for (size_type i = 0; i < prefix.size(); ++i) {
      if (!Traits::eq(myArr[i], prefix.myArr[i]))
        return false;
    }
    return true;
  }
  constexpr auto starts_with(const value_type c) const noexcept {
    return (real_size) > 0 && Traits::eq(myArr[0], c);
  }
  template <size_t M>
  constexpr auto starts_with(const value_type (&arr)[M]) const noexcept {
    for (size_type i = 0; i < M - 1; ++i) {
      if (!Traits::eq(myArr[i], arr[i]))
        return false;
    }
    return true;
  }

  constexpr auto find(const value_type c) const noexcept {
    for (size_type i = 0; i < real_size; ++i) {
      if (Traits::eq(myArr[i], c))
        return i;
    }
    return npos;
  }
  class iterator {
  public:
    using iterator_category = std::random_access_iterator_tag;
    using value_type = CharT;
    using difference_type = ptrdiff_t;
    using pointer = CharT *;
    using reference = CharT &;

  private:
    basic_chars *container;
    size_type index;

  public:
    constexpr iterator() noexcept : container(nullptr), index(0) {}
    constexpr iterator(const basic_chars *cont, size_type idx) noexcept
        : container(const_cast<basic_chars *>(cont)), index(idx) {}

    constexpr reference operator*() const noexcept {
      return (*container)[index];
    }
    constexpr pointer operator->() const noexcept {
      return &(*container)[index];
    }
    constexpr reference operator[](difference_type n) const noexcept {
      return (*container)[index + n];
    }

    constexpr iterator &operator++() noexcept {
      ++index;
      return *this;
    }
    constexpr iterator operator++(int) noexcept {
      auto tmp = *this;
      ++index;
      return tmp;
    }
    constexpr iterator &operator--() noexcept {
      --index;
      return *this;
    }
    constexpr iterator operator--(int) noexcept {
      auto tmp = *this;
      --index;
      return tmp;
    }

    constexpr iterator &operator+=(difference_type n) noexcept {
      index += n;
      return *this;
    }
    constexpr iterator &operator-=(difference_type n) noexcept {
      index -= n;
      return *this;
    }

    constexpr iterator operator+(difference_type n) const noexcept {
      return iterator(container, index + n);
    }
    constexpr iterator operator-(difference_type n) const noexcept {
      return iterator(container, index - n);
    }

    constexpr difference_type operator-(const iterator &other) const noexcept {
      return (index) - (other.index);
    }

    constexpr bool operator==(const iterator &other) const noexcept {
      return container == other.container && index == other.index;
    }
    constexpr std::strong_ordering
    operator<=>(const iterator &other) const noexcept {
      return index <=> other.index;
    }

    friend constexpr iterator operator+(difference_type n,
                                        const iterator &it) noexcept {
      return it + n;
    }
  };

  class const_iterator {
  public:
    using iterator_category = std::random_access_iterator_tag;
    using value_type = CharT;
    using difference_type = ptrdiff_t;
    using pointer = const CharT *;
    using reference = const CharT &;

  private:
    const basic_chars *container;
    size_type index;

  public:
    constexpr const_iterator() noexcept : container(nullptr), index(0) {}
    constexpr const_iterator(const basic_chars *cont, size_type idx) noexcept
        : container(cont), index(idx) {}
    constexpr const_iterator(const iterator &it) noexcept
        : container(it.container), index(it.index) {}

    constexpr reference operator*() const noexcept {
      return (*container)[index];
    }
    constexpr pointer operator->() const noexcept {
      return &(*container)[index];
    }
    constexpr reference operator[](difference_type n) const noexcept {
      return (*container)[index + n];
    }

    constexpr const_iterator &operator++() noexcept {
      ++index;
      return *this;
    }
    constexpr const_iterator operator++(int) noexcept {
      auto tmp = *this;
      ++index;
      return tmp;
    }
    constexpr const_iterator &operator--() noexcept {
      --index;
      return *this;
    }
    constexpr const_iterator operator--(int) noexcept {
      auto tmp = *this;
      --index;
      return tmp;
    }

    constexpr const_iterator &operator+=(difference_type n) noexcept {
      index += n;
      return *this;
    }
    constexpr const_iterator &operator-=(difference_type n) noexcept {
      index -= n;
      return *this;
    }

    constexpr const_iterator operator+(difference_type n) const noexcept {
      return const_iterator(container, index + n);
    }
    constexpr const_iterator operator-(difference_type n) const noexcept {
      return const_iterator(container, index - n);
    }

    constexpr difference_type
    operator-(const const_iterator &other) const noexcept {
      return (index) - (other.index);
    }
    constexpr bool operator==(const const_iterator &other) const noexcept {
      return container == other.container && index == other.index;
    }
    constexpr std::strong_ordering
    operator<=>(const const_iterator &other) const noexcept {
      return index <=> other.index;
    }
    friend constexpr const_iterator
    operator+(difference_type n, const const_iterator &it) noexcept {
      return it + n;
    }
  };
  constexpr std::basic_string<value_type> to_string() const noexcept {
    return myArr;
  }
  constexpr std::basic_string_view<value_type> to_string_view() const noexcept
      [[clang::lifetimebound]] {
    return myArr;
  }
};

// deduction guide for C-style arrays
template <typename CharT, size_t N>
basic_chars(const CharT (&)[N]) -> basic_chars<CharT, N>;

// deduction guide for initializer_list
template <typename CharT, typename... Args>
basic_chars(CharT, Args...) -> basic_chars<CharT, sizeof...(Args) + 2>;

template <typename CharT> constexpr auto as_chars(const CharT ch) noexcept {
  return basic_chars(ch);
}
template <typename CharT, size_t N>
consteval auto as_chars(const CharT (&arr)[N]) noexcept {
  return basic_chars<CharT, N>(arr);
}

template <typename CharT> constexpr auto widen(const CharT ch) noexcept {
  return as_chars(ch);
}

namespace literals {
// NTTP magic
template <const details::basic_chars_storage MyChars>
consteval auto operator""_c() noexcept {
  return basic_chars<typename decltype(MyChars)::value_type,
                     sizeof(MyChars.arr)>(
      static_cast<decltype((MyChars.arr)) &&>(MyChars.arr));
}
} // namespace literals
} // namespace accat::auxilia
