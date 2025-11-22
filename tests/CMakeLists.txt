find_package(GTest CONFIG QUIET)

# if not found GTest, download it
if(NOT GTest_FOUND)
  message(STATUS "GTest not found, downloading from github")
  include(FetchContent)
  FetchContent_Declare(
    gtest
    URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
  )
  FetchContent_MakeAvailable(gtest)
endif()

add_executable(auxilia.test
  Variant.test.cpp
  Status.test.cpp
  StatusOr.test.cpp
  Options.test.cpp
  Bitset.test.cpp
  Chars.test.cpp
)

target_compile_options(auxilia.test PRIVATE -DAC_USE_STD_FMT=1)

target_link_libraries(auxilia.test
  PRIVATE
  GTest::gtest_main
  auxilia::auxilia
)

set_property(GLOBAL PROPERTY CTEST_TARGETS_ADDED 1)

include(CTest)
include(GoogleTest)

gtest_discover_tests(auxilia.test)

add_folder_if_auxilia_top_level("Tests")