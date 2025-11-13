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

add_executable(demo1
  demo1.cpp
)
target_link_libraries(demo1
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

add_executable(demo3
  demo3.cpp
)

target_link_libraries(demo3
  PRIVATE
  auxilia::auxilia
)