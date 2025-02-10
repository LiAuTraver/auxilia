#pragma once

#include "./config.hpp"

namespace accat::auxilia {
#define AC_UTILS_PROPERTY_DBG_CHECK()                                          \
  dbg_block                                                                    \
  {                                                                            \
    precondition(instance, "Property instance is null")                        \
  }

/// @brief (almost) zero-cost fancy wrapper around
/// the getter and setter functions.
/// @tparam Instance the parent class
/// @tparam Field the field type
/// @tparam ReturnType the return type of the getter function
/// @tparam getter the getter function, must be a const member function
/// @tparam setter the setter function, must accept
/// a const reference of the field type
/// @remarks C#-like properties in C++; sugar is all you need. :)
EXPORT_AUXILIA
template <typename Instance,
          typename Field,
          typename ReturnType,
          Field (Instance::*getter)() const,            // MUST be const
          ReturnType (Instance::*setter)(const Field &) // MUST be const ref
          >
struct Property {
  Instance *instance;
  /// DANGER: do not use this constructor
  AC_FORCEINLINE AC_FLATTEN AC_CONSTEXPR20 Property() : instance(nullptr) {}
  AC_FORCEINLINE AC_FLATTEN AC_CONSTEXPR20 Property(Instance *instance)
      : instance(instance) {}
  AC_FORCEINLINE AC_FLATTEN AC_CONSTEXPR20 operator ReturnType() const
      noexcept(noexcept((instance->*getter)())) {
    [[clang::musttail]] return (instance->*getter)();
  }
  AC_FORCEINLINE AC_FLATTEN AC_CONSTEXPR20 Property &
  operator=(const Field &value) noexcept(noexcept((instance->*setter)(value))) {
    AC_UTILS_PROPERTY_DBG_CHECK();
    (instance->*setter)(value);
    return *this;
  }

  template <typename ThatParent,
            typename ThatField,
            typename ThatReturnType,
            ThatField (ThatParent::*ThatGetter)() const,
            ThatReturnType (ThatParent::*ThatSetter)(const ThatField &)>
  AC_FORCEINLINE AC_FLATTEN AC_CONSTEXPR20 Property &
  operator=(const Property<ThatParent,
                           ThatField,
                           ThatReturnType,
                           ThatGetter,
                           ThatSetter>
                &that) noexcept(noexcept((that.instance->*ThatGetter)())) {
    AC_UTILS_PROPERTY_DBG_CHECK();
    return *this = (that.instance->*ThatGetter)();
  }

  AC_FORCEINLINE AC_FLATTEN AC_CONSTEXPR20 Property &operator=(
      const Property &that) noexcept(noexcept((that.instance->*getter)())) {
    AC_UTILS_PROPERTY_DBG_CHECK();
    return *this = (that.instance->*getter)();
  }

  AC_FORCEINLINE AC_FLATTEN AC_CONSTEXPR20 Property &operator=(
      const ReturnType &value) noexcept(noexcept((instance->*setter)(value))) {
    AC_UTILS_PROPERTY_DBG_CHECK();
    return *this = (instance->*setter)(value);
  }
  AC_FORCEINLINE AC_FLATTEN AC_CONSTEXPR20 bool
  operator==(const Field &value) const
      noexcept(noexcept((instance->*getter)())) {
    AC_UTILS_PROPERTY_DBG_CHECK();
    return (instance->*getter)() == value;
  }
  AC_FORCEINLINE AC_FLATTEN AC_CONSTEXPR20 auto
  operator<=>(const Field &value) const
      noexcept(noexcept((instance->*getter)())) {
    AC_UTILS_PROPERTY_DBG_CHECK();
    return (instance->*getter)() <=> value;
  }
};
#undef AC_UTILS_PROPERTY_DBG_CHECK
} // namespace accat::auxilia
