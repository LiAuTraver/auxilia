include_guard()

if(NOT PROJECT_IS_TOP_LEVEL)
  function(add_folder_if_auxilia_top_level FOLDER_NAME)
    # do nothing if not top level
  endfunction()
else()
  set_property(GLOBAL PROPERTY USE_FOLDERS YES)

  # sort targets into corresponding folders
  function(add_folder_if_auxilia_top_level FOLDER_NAME)
    get_property(targets DIRECTORY PROPERTY BUILDSYSTEM_TARGETS)

    foreach(target IN LISTS targets)
      get_property(folder TARGET "${target}" PROPERTY FOLDER)

      if(NOT DEFINED folder OR folder STREQUAL "")
        set(folder Utility)
        get_property(target_type TARGET "${target}" PROPERTY TYPE)

        if(NOT target_type STREQUAL "UTILITY")
          # not utils, create a new folder for it
          set(folder "${FOLDER_NAME}")
        endif()

        # a utility, put it in the Utility folder
        # note: CMake does not allow two targets with the same name globally,
        # it's safe to do so
        set_property(TARGET "${target}" PROPERTY FOLDER "${folder}")
      endif()
    endforeach()
  endfunction()
endif()