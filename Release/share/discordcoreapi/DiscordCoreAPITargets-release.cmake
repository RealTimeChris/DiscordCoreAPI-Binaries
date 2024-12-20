#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "DiscordCoreAPI::DiscordCoreAPI" for configuration "Release"
set_property(TARGET DiscordCoreAPI::DiscordCoreAPI APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(DiscordCoreAPI::DiscordCoreAPI PROPERTIES
  IMPORTED_IMPLIB_RELEASE "${_IMPORT_PREFIX}/lib/discordcoreapi.lib"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/bin/discordcoreapi.dll"
  )

list(APPEND _cmake_import_check_targets DiscordCoreAPI::DiscordCoreAPI )
list(APPEND _cmake_import_check_files_for_DiscordCoreAPI::DiscordCoreAPI "${_IMPORT_PREFIX}/lib/discordcoreapi.lib" "${_IMPORT_PREFIX}/bin/discordcoreapi.dll" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
