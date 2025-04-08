# Auxilia - My tiny C++ header-only toolkit for specialized use

This is a collection of small C++ utilities that I find useful in various projects. **Header-only**.
C++20 is required to use this library, but the library will progressively be updated to the latest C++ standard. 

## Installation

If MSVC is used, make sure to turn on standard preprocessor via `/Zc:preprocessor` to enable the `__VA_OPT__` macro and conformant `__VA_ARGS__`. 

`fmt` is strongly recommended, but not required. `spdlog` is also recommended for logging.

### Quick start

Add the following lines to your `CMakeLists.txt` file; since its a header-only library, no need to build it(or use `FetchContent`).:

```cmake
include(ExternalProject)

set(AUXILIA_INCLUDE_DIR ${CMAKE_BINARY_DIR}/auxilia-src/include)

# Download but don't build
ExternalProject_Add(auxilia_download
  GIT_REPOSITORY https://github.com/LiAuTraver/auxilia.git
  GIT_TAG main
  SOURCE_DIR ${CMAKE_BINARY_DIR}/auxilia-src
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)

add_library(auxilia INTERFACE)
add_library(auxilia::auxilia ALIAS auxilia)
target_include_directories(auxilia INTERFACE ${AUXILIA_INCLUDE_DIR})
target_compile_features(auxilia INTERFACE cxx_std_23)

add_dependencies(auxilia auxilia_download)
```

## Features

name a few. All utilities are in the `accat::auxilia` namespace.

### Useful utilities

- `Status`: A simple status class for error handling and reporting, mimic from `absl::Status`.

- `StatusOr`: A class that represents a value or an error status, similar to `absl::StatusOr`. Requirement: concept `Storable<Ty>`, resembles `_STD __SMF_control<...>` in Microsoft's STL.
```cpp
if (auto maybeResult = func()) {
    // do something with the result
    std::println("Result: {}", *maybeResult);
} else {
    // handle the error
    std::println("Error: {}", maybeResult.message());
}

// or conveniently if only printing the result:
std::println("Result: {}", func());

// or more idiomatically,
func()
  .and_then([](auto&& result) {
    // do something with the result
    return new_status_or_with_anything;
  })
  .or_else([](auto&& error) {
    // handle the error
    return new_status_or;
  });

// additional monadic operations:
func()
  .transform([](auto&& result) {
    return anything;
  })
  .transform_error([](auto&& error) {
    return new_status;
  })
  .transform([](auto&& result) {
    // no need to return a value, implicitly returns a `Monostate`(inside a `StatusOr`)
  })
  .transform_error([](auto&& error) {
    // no need to return a value, which means the error is not modified and will propagate
  });
```
- `Generator`: Similar to `std::generator` in C++23, used in some platforms where C++23's `std::generator` is not fully supported.

- `MemoryPool`: A simple memory pool allocator which allocates fix-sized memory on the stack.

- `Monostate`,`Variant`: `std::variant` wrapper with more functionality. Say goodbye to awfully long `std::holds_alternative`. The types used in `Variant` must satisfy `Variantable<...>` concept: the first type must be `Monostate` or class derived from `Monostate`, and the rest must be `default constructible` and `Storable`.
> tested that the `template` keyword is not needed(e.g., `v.template get<Ty>`) with current compilers.
```cpp
Variant<Monostate, int, std::string> v = 1;
if (auto ptr = v.get_if<int>()) {
    std::println("int: {}", *ptr);
} 
v = "Hello world!";
if (v.is_type<std::string>()) {
    std::println("string: {}", v.get<std::string>());
}
v = Monostate{}; // reset to monostate, or v.reset() 
v.visit(match(
    [](const int& i) { std::println("int: {}", i); },
    [](const std::string& s) { std::println("string: {}", s); },
    [](const auto&) { std::println("don't care"); } // catch-all
));
// or when multiple variants are needed:
accat::auxilia::visit(pattern, v1, v2, v3, ...);
// printing:
std::println("{}", v); // prints "class accat::auxilia::Variant<struct accat::auxilia::Monostate,int,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > > "
std::println("{}", v.to_string()); // prints "Monostate" or "1" or "Hello world!" depending on the type of `v`
```

- `Property`: C#-like property system for C++ full of syntax sugar.

- `Printable`,`Viewable`: template-less static interface for `fmt::print` and `fmt::format_to`, or possibly `std::print` and `std::format_to` in C++23.
```cpp
struct MyStruct : Printable {
  // no need to override
  auto to_string(const accat::auxilia::FormatPolicy& policy = accat::auxilia::FormatPolicy::kDefault) const -> string_type {
    return "a string";
  }
};
MyStruct s;
std::println("MyStruct: {}", s); // prints "MyStruct: a string"
```
- `id`: Thread-safe simple function to assign unique IDs to objects.

- `rand_u8` (or `u16, u32, u64`, maybe `u128`): Numpy-like random number generator without writing boilerplate code like `std::random_device` and `std::mt19937` every time.

- `views::trim`: A view that trims whitespace-like characters from the beginning and end of a string. Pipe operator-chainable.

- `read_as_bytes`: read a file as binary, with more efficiency and better error handling. Endianness is supported.

### Useful Macros

> Most of the functionalities must be enabled by defining the variable `AC_UTILS_DEBUG_ENABLED` to `1` or set the environment variable `AC_CPP_DEBUG` to `ON` or in `CMakeLists.txt`.

- `defer`: Similar to `defer` in Go, allows you to defer some execution until the end of the scope. This may make the code less-readable, but it is useful in some cases.
```cpp
defer { /* do something */ }; // don't forget semicolon
```

- `contract_assert`, `precondition` and `postcondition`: initialy an idea proposed for C++26(now accepted, thus sometime maybe I'll have their name changed), these macros are used to assert preconditions and postconditions in a function. Precondition and Postcondition won't be checked in release mode, but contract_assert will be checked in both debug and release mode. This is useful for debugging and testing purposes. Semicolon is not needed but better add it to `.clang-format` as `StatementAttributeLikeMacros` in order to avoid *iℕℂ*öṙṙĕℂţ **Föṙ**MäṮ*ţĭng*.
Furthermore, those assertion triggers a debug break when debugger is attached, so you can easily debug the code when it fails(Rather than an awkward `Microsoft C++ Runtime Library` window popping up and terminates the program), otherwise prints stacktrace and aborts the program. I found it more useful both than `assert` and `boost::contract::check` boilerplate code. 

> note: the functionalities of `pre`, `post` and `contract_assert` is slightly different from the original proposal. I just borrowed the name.
```cpp
void func(int x) {
    contract_assert(x > 0, "x must be greater than 0"); // will be checked in both debug and release mode
    precondition(x > 0, "x must be greater than 0") // will be checked at once, so ensure put it at the beginning of the function
    postcondition(x < 10) // will be checked in when the function returns
    // do something
}
```
- `dbg`: With `spdlog`, this macro is used to print debug information with zero cost at release mode. `spdlog::debug` persists in release mode so it comes with a cost. ditto, it's a debug utility. 
```cpp
AC_SPDLOG_INITIALIZATION(myapp, info); // log with `info` level and above will be printed 
dbg(debug, "Hi!"); // equivalent to `spdlog::debug("Hi!")`
dbg(error, "Error: {}", msg); // ditto
```
- `TODO()`: Marks a line of code as a TODO. It will be highlighted in the IDE and can be used to track TODOs in the codebase. Throws exception in release mode; if exceptions are disabled, raises `SIGABRT` signal. 

- `DebugUnreachable`: Marks a line of code as unreachable. It will be checked in debug mode and will trigger debugbreak just like above.

- `dbg_block`: A block of code that will be executed only in debug mode. 
```cpp
dbg_block {
    // this code will be executed only in debug mode
};
```

- `AC_BITMASK_OPS(_bitmask_)`: from Microsoft's STL implementation, bring some bit operations for scoped enums.

## Notes
A considerable part of the idea comes from stackoverflow, existing libraries, or my own projects. I don't claim any ownership of the ideas, and their inspiration are documented in the code. If you find any bugs or have any suggestions, please open an issue or a pull request.

## License
Apache License 2.0, see [LICENSE](LICENSE) for details.
