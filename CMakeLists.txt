cmake_minimum_required(VERSION 3.21 FATAL_ERROR)

project(auxilia
  VERSION 1.1.0
  DESCRIPTION "A collection of utilities for C++"
  HOMEPAGE_URL "https://github.com/liautraver/auxilia"
  LANGUAGES C CXX
)

option(AUXILIA_INSTALL "Include an install target" ${PROJECT_IS_TOP_LEVEL})
option(AUXILIA_BUILD_TESTS "Build tests" ${PROJECT_IS_TOP_LEVEL})
option(AUXILIA_BUILD_DEMO "Build demo" ${PROJECT_IS_TOP_LEVEL})
option(AUXILIA_BUILD_PROJECTS "Build demo" ${PROJECT_IS_TOP_LEVEL})

add_library(auxilia INTERFACE)
add_library(auxilia::auxilia ALIAS auxilia)

target_include_directories(auxilia INTERFACE
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/include>
)
add_compile_definitions(_ENABLE_STL_INTERNAL_CHECK=1)

if(DEFINED ENV{AC_CPP_DEBUG} AND "$ENV{AC_CPP_DEBUG}" STREQUAL "ON" AND CMAKE_BUILD_TYPE STREQUAL "Debug")
  message(STATUS "Enabling debug mode macros")
  target_compile_definitions(auxilia INTERFACE AC_CPP_DEBUG)
  find_package(spdlog REQUIRED)
  target_link_libraries(auxilia INTERFACE spdlog::spdlog)
endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
include(cmake/folders.cmake)

if(PROJECT_IS_TOP_LEVEL)
  if((NOT DEFINED ${CMAKE_CXX_STANDARD}) OR("${${CMAKE_CXX_STANDARD}}" STREQUAL ""))
    set(CMAKE_CXX_STANDARD 23)
  endif()

  if((NOT DEFINED ${CMAKE_C_STANDARD}) OR("${${CMAKE_C_STANDARD}}" STREQUAL ""))
    set(CMAKE_C_STANDARD 17)
  endif()

  include(cmake/prelude.cmake)
  target_compile_features(auxilia INTERFACE cxx_std_23)
else()
  target_compile_features(auxilia INTERFACE cxx_std_20)
endif()

if(AUXILIA_BUILD_TESTS)
  add_subdirectory(tests)
endif()

if(AUXILIA_BUILD_DEMO)
  add_subdirectory(demo)
endif()

if(AUXILIA_BUILD_PROJECTS)
  add_subdirectory(projects)
endif()

if(AUXILIA_INSTALL)
  include(cmake/install.cmake)
endif()

include(CPack)

add_folder_if_auxilia_top_level("Library")