add_subdirectory(cp)

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/sysY-antlr4/CMakeLists.txt")
  add_subdirectory(sysY-antlr4)
endif()