project(cp LANGUAGES CXX)

find_package(GTest REQUIRED)

add_library(cp.shared OBJECT
  include/automaton.hpp
  src/Automaton.cpp
  include/Grammar.hpp
  src/Grammar.cpp
  src/Piece.cpp
  include/Lexing.hpp
  src/Lexing.cpp
  src/NFA.cpp
  include/NFA.hpp
  src/DFA.cpp
  include/DFA.hpp
)
target_link_libraries(cp.shared PRIVATE
  auxilia::auxilia
  fmt::fmt
  spdlog::spdlog
)
target_include_directories(cp.shared PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)
add_executable(cp
  cp.cpp
)
target_include_directories(cp PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)
target_link_libraries(cp PRIVATE
  auxilia::auxilia
  fmt::fmt
  spdlog::spdlog

  cp.shared
)

add_folder_if_auxilia_top_level("Tools")

add_executable(cp.Grammar.demo
  demo/Grammar.demo.cpp
)
target_include_directories(cp.Grammar.demo PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)
target_link_libraries(cp.Grammar.demo PRIVATE
  auxilia::auxilia
  fmt::fmt
  spdlog::spdlog

  cp.shared
)

add_executable(cp.Automaton.demo
  demo/Automaton.demo.cpp
)
target_include_directories(cp.Automaton.demo PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)
target_link_libraries(cp.Automaton.demo PRIVATE
  auxilia::auxilia
  fmt::fmt
  spdlog::spdlog

  cp.shared
)
add_folder_if_auxilia_top_level("Demo")
add_executable(cp.test
  tests/Automata.test.cpp
  tests/Grammar.test.cpp
  tests/Grammar.partial.test.cpp
)
target_include_directories(cp.test PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)
target_link_libraries(cp.test PRIVATE
  auxilia::auxilia
  fmt::fmt
  spdlog::spdlog

  cp.shared

  GTest::gtest_main
)
gtest_discover_tests(cp.test)

add_folder_if_auxilia_top_level("Tests")