#pragma once

#include "./config.hpp"
#include "./Status.hpp"

namespace accat::auxilia {
/// @brief fancy wrapper around the getter and setter functions.
/// @tparam Instance the parent class
/// @tparam Field the field type
/// @tparam ReturnType the return type of the getter function
/// @tparam getter the getter function
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
  constexpr Property() : instance(nullptr) {}
  constexpr Property(Instance *instance) : instance(instance) {}
  constexpr operator ReturnType() const
      noexcept(noexcept((instance->*getter)())) {
    return (instance->*getter)();
  }
  constexpr Property &
  operator=(const Field &value) noexcept(noexcept((instance->*setter)(value))) {
    precondition(instance, "Property instance is null")
    (instance->*setter)(value);
    return *this;
  }

  template <typename ThatParent,
            typename ThatField,
            typename ThatReturnType,
            ThatField (ThatParent::*ThatGetter)() const,
            ThatReturnType (ThatParent::*ThatSetter)(const ThatField &)>
  constexpr Property &
  operator=(const Property<ThatParent,
                           ThatField,
                           ThatReturnType,
                           ThatGetter,
                           ThatSetter>
                &that) noexcept(noexcept((that.instance->*ThatGetter)())) {
    precondition(instance, "Property instance is null")
    return *this = (that.instance->*ThatGetter)();
  }

  constexpr Property &operator=(const Property &that) noexcept(
      noexcept((that.instance->*getter)())) {
    precondition(instance, "Property instance is null")
    return *this = (that.instance->*getter)();
  }

  constexpr Property &operator=(const ReturnType &value) noexcept(
      noexcept((instance->*setter)(value))) {
    precondition(instance, "Property instance is null")
    return *this = (instance->*setter)(value);
  }
  constexpr bool operator==(const Field &value) const
      noexcept(noexcept((instance->*getter)())) {
    precondition(instance, "Property instance is null")
    return (instance->*getter)() == value;
  }
  constexpr auto operator<=>(const Field &value) const
      noexcept(noexcept((instance->*getter)())) {
    precondition(instance, "Property instance is null")
    return (instance->*getter)() <=> value;
  }
};

} // namespace accat::auxilia
