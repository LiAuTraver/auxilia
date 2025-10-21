#pragma once
#include <string>
#include <string_view>
#include "./type_traits.hpp"

namespace accat::auxilia {
/// @brief a tiny compile-time character array wrapper
template <typename CharT, size_t N> class basic_chars {

public:
  class iterator;
  using value_type = CharT;
  using pointer = CharT *;
  using const_pointer = const CharT *;
  using reference = CharT &;
  using const_reference = const CharT &;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_iterator = const typename basic_chars<CharT, N>::iterator;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;
  using size_type = size_t;
  using difference_type = ptrdiff_t;

private:
  value_type myArr[N] = {};

private:
  consteval auto assign_from(const value_type (&arr)[N]) noexcept {
    for_each_char(
        [&](value_type &current, size_type index) { current = arr[index]; });
  }
  constexpr void for_each_char(this auto &&self, auto &&func) noexcept {
    for (size_type i = 0; i < self.my_real_size(); ++i) {
      func(self.myArr[i], i);
    }
  }
  consteval auto my_real_size() const noexcept { return N - 1; }

public:
  consteval basic_chars() noexcept = default;
  consteval basic_chars(const value_type (&arr)[N]) noexcept {
    assign_from(arr);
  }
  consteval basic_chars(const basic_chars &other) noexcept {
    assign_from(other.myArr);
  }
  consteval basic_chars(basic_chars &&other) noexcept {
    assign_from(static_cast<decltype(other.myArr) &&>(other.myArr));
  }
  constexpr ~basic_chars() noexcept = default;

public:
  constexpr basic_chars &operator=(const basic_chars &other) noexcept {
    assign_from(other.myArr);
    return *this;
  }
  constexpr basic_chars &operator=(basic_chars &&other) noexcept {
    assign_from(static_cast<decltype(other.myArr) &&>(other.myArr));
    return *this;
  }
  constexpr basic_chars &operator=(const value_type (&arr)[N]) noexcept {
    assign_from(arr);
    return *this;
  }
  constexpr basic_chars &operator==(const basic_chars &other) const noexcept {
    for (size_type i = 0; i < my_real_size(); ++i) {
      if (myArr[i] != other.myArr[i])
        return false;
    }
    return true;
  }
  constexpr basic_chars &operator==(const value_type (&arr)[N]) const noexcept {
    for (size_type i = 0; i < my_real_size(); ++i) {
      if (myArr[i] != arr[i])
        return false;
    }
    return true;
  }

public:
  consteval auto size() const noexcept { return my_real_size(); }
  consteval auto length() const noexcept { return my_real_size(); }
  constexpr auto &operator[](this auto &&self, size_type index) noexcept {
    return self.myArr[index];
  }
  constexpr auto &at(this auto &&self, const size_type index) noexcept {
    return self.myArr[index];
  }
  constexpr auto count(const value_type c) const noexcept {
    size_type total = 0;
    for_each_char([&](const value_type current, auto &&_) {
      if (current == c)
        ++total;
    });
    return total;
  }
  constexpr auto begin(this auto &&self) noexcept { return iterator(self, 0); }
  constexpr auto end(this auto &&self) noexcept {
    return iterator(self, self.my_real_size());
  }
  constexpr auto cbegin() const noexcept { return const_iterator(*this, 0); }
  constexpr auto cend() const noexcept {
    return const_iterator(*this, my_real_size());
  }
  constexpr void clear() noexcept {
    for_each_char([&](value_type &current) { current = '\0'; });
  }
  template <size_type pos, value_type c> constexpr auto &replace() noexcept {
    myArr[pos] = c;
    return *this;
  }
  constexpr auto &front(this auto &&self) noexcept { return self.myArr[0]; }
  constexpr auto &back(this auto &&self) noexcept {
    return self.myArr[self.my_real_size() - 1];
  }
  constexpr auto &data() const noexcept { return myArr; }
  constexpr operator std::basic_string_view<value_type>() const noexcept {
    return std::basic_string_view<value_type>(myArr, my_real_size());
  }
  constexpr operator std::basic_string<value_type>() const noexcept {
    return std::basic_string<value_type>(myArr, my_real_size());
  }
  constexpr auto contains(const value_type c) const noexcept {
    return count(c) > 0;
  }
  template <const value_type c> constexpr auto &fill() noexcept {
    for_each_char([&](value_type &current, auto &&_) { current = c; });
    return *this;
  }
  constexpr auto &reverse() noexcept {
    for (size_type i = 0; i < (my_real_size()) / 2; ++i) {
      auto temp = myArr[i];
      myArr[i] = myArr[(my_real_size())-1 - i];
      myArr[(my_real_size())-1 - i] = temp;
    }
    return *this;
  }
  template <size_t M>
  constexpr auto
  starts_with(const basic_chars<value_type, M> &prefix) const noexcept {
    for (size_type i = 0; i < prefix.size(); ++i) {
      if (myArr[i] != prefix.myArr[i])
        return false;
    }
    return true;
  }
  constexpr auto starts_with(const value_type c) const noexcept {
    return (my_real_size()) > 0 && myArr[0] == c;
  }
  template <size_t M>
  constexpr auto starts_with(const value_type (&arr)[M]) const noexcept {
    for (size_type i = 0; i < M - 1; ++i) {
      if (myArr[i] != arr[i])
        return false;
    }
    return true;
  }
  class iterator {
  private:
    using container_type = basic_chars<value_type, N>;
    container_type &myContainer;
    size_type myIndex;

  public:
    constexpr iterator(container_type &container, size_type index) noexcept
        : myContainer(container), myIndex(index) {}
    constexpr auto &operator++() noexcept {
      ++myIndex;
      return *this;
    }
    constexpr auto &operator*() noexcept { return myContainer.myArr[myIndex]; }
    constexpr bool operator==(const iterator &other) const noexcept {
      return myContainer.data() == other.myContainer.data() &&
             myIndex == other.myIndex;
    }
  };
};
template <typename CharT, size_t N>
basic_chars(const CharT (&)[N]) -> basic_chars<CharT, N>;

template <size_t N> consteval auto as_chars(const char (&arr)[N]) noexcept {
  return basic_chars<char, N>(arr);
}
template <size_t N> using chars = basic_chars<char, N>;
template <size_t N> using wchars = basic_chars<wchar_t, N>;
#ifdef __cpp_char8_t
template <size_t N> using u8chars = basic_chars<char8_t, N>;
#endif
template <size_t N> using u16chars = basic_chars<char16_t, N>;
template <size_t N> using u32chars = basic_chars<char32_t, N>;
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
