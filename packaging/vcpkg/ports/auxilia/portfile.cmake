# portfile.cmake
set(SOURCE_PATH "Z:/auxilia")

# vcpkg_from_github(
# OUT_SOURCE_PATH SOURCE_PATH
# REPO liautraver/auxilia
# REF "v${VERSION}"
# # TODO  SHA512 db14d71da3c1ecb849f00ac1e334f39c532592230e950aa1009ff00ba56670cb71e33ca457fd4ac66595ff43f0dca0e42d45f672848b9cde3cba80f19ef8693f
# HEAD_REF master
# )
vcpkg_cmake_configure(
  SOURCE_PATH ${SOURCE_PATH}
)

vcpkg_cmake_install()

# Fix CMake config files location
vcpkg_cmake_config_fixup(
  PACKAGE_NAME auxilia
  CONFIG_PATH lib/cmake/auxilia
)

# Remove debug includes and other unnecessary directories
file(REMOVE_RECURSE
  "${CURRENT_PACKAGES_DIR}/debug/"
  "${CURRENT_PACKAGES_DIR}/lib"
)

# Install copyright
file(INSTALL "${SOURCE_PATH}/LICENSE"
  DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}"
  RENAME copyright)

vcpkg_copy_pdbs()