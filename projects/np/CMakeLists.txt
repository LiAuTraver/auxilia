project(np LANGUAGES CXX)

add_executable(np.chatroom
  chatroom.cpp
)

target_link_libraries(np.chatroom PRIVATE
  auxilia::auxilia
  fmt::fmt
  spdlog::spdlog
)
add_executable(np.oneshot
  oneshot.cpp
)

target_link_libraries(np.oneshot PRIVATE
  auxilia::auxilia
  fmt::fmt
  spdlog::spdlog
)
add_executable(np.http_server
  http_server.cpp
)
target_link_libraries(np.http_server
  PRIVATE
  auxilia::auxilia
  fmt::fmt
  spdlog::spdlog
)

add_folder_if_auxilia_top_level("Demo")
