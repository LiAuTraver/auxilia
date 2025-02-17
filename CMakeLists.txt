cmake_minimum_required(VERSION 3.15)

project(auxilia VERSION 1.0.0 LANGUAGES CXX)

add_library(auxilia INTERFACE)

target_include_directories(auxilia INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

target_compile_features(auxilia INTERFACE cxx_std_23)

include(GNUInstallDirs)
install(
    DIRECTORY include/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
install(
    TARGETS auxilia
    EXPORT auxilia-targets
)

install(EXPORT auxilia-targets
    FILE auxilia-config
    NAMESPACE auxilia::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/auxilia
)
