set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if((NOT DEFINED ${CMAKE_CXX_STANDARD}) OR("${${CMAKE_CXX_STANDARD}}" STREQUAL ""))
  set(CMAKE_CXX_STANDARD 23)
endif()

if((NOT DEFINED ${CMAKE_C_STANDARD}) OR("${${CMAKE_C_STANDARD}}" STREQUAL ""))
  set(CMAKE_C_STANDARD 17)
endif()

add_executable(demo
  demo.cpp
)
target_link_libraries(demo
  PRIVATE
  auxilia::auxilia
)

add_executable(program_options_demo
  program.options.demo.cpp
)
target_link_libraries(program_options_demo
  PRIVATE
  auxilia::auxilia
)

add_executable(demo2
  demo2.cpp
)

target_link_libraries(demo2
  PRIVATE
  auxilia::auxilia
)

add_executable(flight
  flight.cpp
)
target_link_libraries(flight
  PRIVATE
  auxilia::auxilia
)