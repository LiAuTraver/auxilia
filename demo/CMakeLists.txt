add_executable(program_options_demo
  program.options.demo.cpp
)
target_link_libraries(program_options_demo
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

add_folder_if_auxilia_top_level("Demo")