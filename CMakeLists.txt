cmake_minimum_required(VERSION 3.15 FATAL_ERROR)

project(auxilia
VERSION 1.0.0
DESCRIPTION "A collection of utilities for C++"
HOMEPAGE_URL "https://github.com/liautraver/auxilia"
LANGUAGES CXX
)

if(NOT DEFINED PROJECT_NAME)
  set(AUXILIA_IS_TOP_LEVEL ON)
else()
  set(AUXILIA_IS_TOP_LEVEL OFF)
endif()

option(AUXILIA_INSTALL "Include an install target" ${AUXILIA_IS_TOP_LEVEL})
option(AUXILIA_BUILD_TESTS "Build tests" ${AUXILIA_IS_TOP_LEVEL})
option(AUXILIA_BUILD_SAMPLES "Build samples" OFF)

add_library(auxilia INTERFACE)
add_library(auxilia::auxilia ALIAS auxilia)

target_compile_features(auxilia INTERFACE cxx_std_20)
target_include_directories(auxilia INTERFACE
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/include>
)

if(AUXILIA_BUILD_SAMPLES)
  add_subdirectory(samples)
endif()

if(AUXILIA_BUILD_TESTS)
  add_subdirectory(test)
endif()

if(AUXILIA_INSTALL)
  include(GNUInstallDirs)
  include(CMakePackageConfigHelpers)

  install(
    TARGETS auxilia EXPORT auxiliaConfig
  )
  install(
    EXPORT auxiliaConfig
    NAMESPACE auxilia::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

  install(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    FILES_MATCHING PATTERN "*.hpp" PATTERN "*.h"
  )

  set(CONFIG_FILE_NAME_WITHOUT_EXT "${PROJECT_NAME}Config")
  set(CMAKE_CONFIG_FILE_BASENAME "${CMAKE_CURRENT_BINARY_DIR}/${CONFIG_FILE_NAME_WITHOUT_EXT}")
  set(CMAKE_CONFIG_VERSION_FILE_NAME "${CMAKE_CONFIG_FILE_BASENAME}-version.cmake")
  set(CMAKE_CONFIG_FILE_NAME "${CMAKE_CONFIG_FILE_BASENAME}.cmake")

  set(OPTIONAL_ARCH_INDEPENDENT "ARCH_INDEPENDENT")

  set(PackagingTemplatesDir "${CMAKE_CURRENT_SOURCE_DIR}/packaging")

  # cmake find_package config file
  configure_package_config_file(
    "${PackagingTemplatesDir}/auxiliaConfig.cmake.in"
    "${CMAKE_CONFIG_FILE_NAME}"
    INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  )

  install(
    FILES "${CMAKE_CONFIG_FILE_NAME}"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  )

  # linux pkg-config file
  write_basic_package_version_file("${CMAKE_CONFIG_VERSION_FILE_NAME}"
    COMPATIBILITY SameMajorVersion
    ${OPTIONAL_ARCH_INDEPENDENT}
  )

  export(EXPORT auxiliaConfig
    NAMESPACE auxilia::
  )

  install(
    FILES "${CMAKE_CONFIG_VERSION_FILE_NAME}"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  )

  # CPack
  set(CPACK_PACKAGE_NAME "${PROJECT_NAME}")
  set(CPACK_PACKAGE_VENDOR "auxilia developers")
  set(CPACK_PACKAGE_DESCRIPTION "${PROJECT_DESCRIPTION}")
  set(CPACK_DEBIAN_PACKAGE_NAME "${CPACK_PACKAGE_NAME}")
  set(CPACK_RPM_PACKAGE_NAME "${CPACK_PACKAGE_NAME}")
  set(CPACK_PACKAGE_HOMEPAGE_URL "${PROJECT_HOMEPAGE_URL}")
  set(CPACK_PACKAGE_MAINTAINER "LiAuTraver")
  set(CPACK_DEBIAN_PACKAGE_MAINTAINER "${CPACK_PACKAGE_MAINTAINER}")
  set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
  set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")

  set(CPACK_DEBIAN_PACKAGE_NAME "lib${PROJECT_NAME}-dev")
  set(CPACK_DEBIAN_PACKAGE_SUGGESTS "cmake, pkg-config, pkg-conf")

  set(CPACK_RPM_PACKAGE_NAME "lib${PROJECT_NAME}-devel")
  set(CPACK_RPM_PACKAGE_SUGGESTS "${CPACK_DEBIAN_PACKAGE_SUGGESTS}")

  set(CPACK_DEB_COMPONENT_INSTALL ON)
  set(CPACK_RPM_COMPONENT_INSTALL ON)
  set(CPACK_NSIS_COMPONENT_INSTALL ON)
  set(CPACK_DEBIAN_COMPRESSION_TYPE "xz")

  set(PKG_CONFIG_FILE_NAME "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc")

  configure_file("${PackagingTemplatesDir}/pkgconfig.pc.in" "${PKG_CONFIG_FILE_NAME}" @ONLY)
  install(FILES "${PKG_CONFIG_FILE_NAME}"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig"
  )
endif()

include(CPack)