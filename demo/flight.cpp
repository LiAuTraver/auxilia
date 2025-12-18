#include <algorithm>
#include <cassert>
#include <climits>
#include <cstddef>
#include <iostream>
#include <new>
#include <span>

#include <accat/auxilia/auxilia.hpp>
#include <accat/auxilia/defines.hpp>
#include "accat/auxilia/status/StatusOr.hpp"
#include "accat/auxilia/utility/Noise.hpp"

using namespace accat::auxilia;
// unused vvv
// class dynamic_bitset : details::_bitset_base<std::dynamic_extent>, Printable
// {
//   using myBase = details::_bitset_base<std::dynamic_extent>;
//   friend myBase;

//   using myBase::bit_offset;
//   using myBase::bits_per_word;
//   using myBase::word_offset;
//   using storage_type = myBase::storage_type;

//   using container_type = std::vector<storage_type>;

// public:
//   using size_type = size_t;

// protected:
//   container_type myArr;
//   size_type mySize;

// public:
//   constexpr auto size() const noexcept { return mySize; }
// };
// https://gcc.godbolt.org/z/zbY8cYTP7
template <typename T> struct pmf_signature;

template <typename Ret, typename Cls, typename... Args>
struct pmf_signature<Ret (Cls::*)(Args...)> {
  using return_type = Ret;
  using class_type = Cls;
  static constexpr auto is_const_pmf = false;
};

// 2. Specialization for const member functions
template <typename Ret, typename Cls, typename... Args>
struct pmf_signature<Ret (Cls::*)(Args...) const> {
  using return_type = Ret;
  using class_type = Cls;
  static constexpr auto is_const_pmf = true;
};

template <auto PMF> struct pmf_traits : pmf_signature<decltype(PMF)> {};

template <typename T, auto PMFSet, auto PMFGet, auto FOffset, typename = void>
class property;

template <typename T, auto PMFSet, auto PMFGet, auto FOffset>
  requires std::is_fundamental_v<T>
class property<T, PMFSet, PMFGet, FOffset> {
private:
  using parent_type = pmf_traits<PMFSet>::class_type;
  friend parent_type;

  using getter_return_type = pmf_traits<PMFGet>::return_type;

  AC_NO_UNIQUE_ADDRESS T value;

  AC_FORCEINLINE AC_FLATTEN inline parent_type *get_parent() {
    return reinterpret_cast<parent_type *>(
        reinterpret_cast<char *>(this) -
        FOffset.template operator()<parent_type>());
  }

  AC_FORCEINLINE AC_FLATTEN inline const parent_type *get_parent() const {
    return reinterpret_cast<const parent_type *>(
        reinterpret_cast<const char *>(this) -
        FOffset.template operator()<parent_type>());
  }

public:
  AC_FORCEINLINE AC_FLATTEN inline property &operator=(const T &x) {
    (get_parent()->*PMFSet)(x);
    return *this;
  }

  AC_FORCEINLINE AC_FLATTEN inline operator getter_return_type() const {
    return (get_parent()->*PMFGet)();
  }

  AC_FORCEINLINE AC_FLATTEN inline T &as_value() { return value; }
  AC_FORCEINLINE AC_FLATTEN inline const T &as_value() const { return value; }
};

template <typename T, auto PMFSet, auto PMFGet, auto FOffset>
  requires(!std::is_fundamental_v<T>)
class property<T, PMFSet, PMFGet, FOffset> : public T {
private:
  using parent_type = pmf_traits<PMFSet>::class_type;
  friend parent_type;

  using getter_return_type = pmf_traits<PMFGet>::return_type;

  AC_FORCEINLINE AC_FLATTEN inline parent_type *get_parent() {
    return reinterpret_cast<parent_type *>(
        reinterpret_cast<char *>(this) -
        FOffset.template operator()<parent_type>());
  }

  AC_FORCEINLINE AC_FLATTEN inline const parent_type *get_parent() const {
    return reinterpret_cast<const parent_type *>(
        reinterpret_cast<const char *>(this) -
        FOffset.template operator()<parent_type>());
  }

public:
  AC_FORCEINLINE AC_FLATTEN inline property &operator=(const T &x) {
    (get_parent()->*PMFSet)(x);
    return *this;
  }

  AC_FORCEINLINE AC_FLATTEN inline operator getter_return_type() const {
    return (get_parent()->*PMFGet)();
  }

  AC_FORCEINLINE AC_FLATTEN inline T &as_value() {
    return static_cast<T &>(*this);
  }
  AC_FORCEINLINE AC_FLATTEN inline const T &as_value() const {
    return static_cast<const T &>(*this);
  }
};

#define DEFINE_PROPERTY(parent_type, type, setter_name, getter_name, name)     \
  ::property<type,                                                             \
             &parent_type::setter_name,                                        \
             &parent_type::getter_name,                                        \
             []<typename P>() consteval {                                      \
               return __builtin_offsetof(P, name);                             \
             }>                                                                \
      name

class parent {
private:
  void set_foo(int x) {
    std::printf("setting foo to %d\n", x);
    foo.as_value() = x;
  }

  int get_foo() const {
    std::printf("getting foo (%d)", foo.as_value());
    return foo.as_value();
  }

  void set_bar(const std::string &x) {
    std::printf("setting bar to %s\n", x.c_str());
    bar.as_value() = x;
  }

  const std::string &get_bar() const {
    std::printf("getting bar (%s)", bar.as_value().c_str());
    return bar.as_value();
  }

public:
  //   DEFINE_PROPERTY(parent, int, set_foo, get_foo, foo);
  //   DEFINE_PROPERTY(parent, std::string, set_bar, get_bar, bar);
  property<int,
           &parent::set_foo,
           &parent::get_foo,
           []<typename P>() consteval { return __builtin_offsetof(P, foo); }>
      foo;
  property<std::string,
           &parent::set_bar,
           &parent::get_bar,
           []<typename P>() consteval { return __builtin_offsetof(P, bar); }>
      bar;
};

struct exemplar {
  int i;
  std::string s;
};

static_assert(sizeof(parent) == sizeof(exemplar));
static_assert(alignof(parent) == alignof(exemplar));

int main() {
  StatusOr<Noise<>> s1 = Noise<>();
  std::move(s1)
      .and_then([](auto &&val) {
        Println("Got Noise from StatusOr");
        return StatusOr<Noise<>>(std::move(val));
      })
      .and_then([](auto &&val) {
        Println("Chained Noise from StatusOr");
        return StatusOr<Noise<>>(std::move(val));
      })
      .transform([](auto &&val) { std::cout << val; });
  // parent p;
  // p.foo = 100;

  // p.bar = "hello world";

  // auto x = p.bar.size();
  // assert(x == 11);

  // return p.foo;
}
