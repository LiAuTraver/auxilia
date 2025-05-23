cmake_minimum_required(VERSION 3.21 FATAL_ERROR)

project(auxilia
  VERSION 1.1.0
  DESCRIPTION "A collection of utilities for C++"
  HOMEPAGE_URL "https://github.com/liautraver/auxilia"
  LANGUAGES C CXX
)

option(AUXILIA_INSTALL "Include an install target" ${PROJECT_IS_TOP_LEVEL})
option(AUXILIA_BUILD_TESTS "Build tests" ${PROJECT_IS_TOP_LEVEL})

add_library(auxilia INTERFACE)
add_library(auxilia::auxilia ALIAS auxilia)

target_compile_features(auxilia INTERFACE cxx_std_20)
target_include_directories(auxilia INTERFACE
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/include>
)

if(PROJECT_IS_TOP_LEVEL)
  include(cmake/prelude.cmake)
endif()

if(AUXILIA_BUILD_TESTS)
  add_subdirectory(tests)
endif()

if(AUXILIA_INSTALL)
  include(cmake/install.cmake)
endif()

include(CPack)