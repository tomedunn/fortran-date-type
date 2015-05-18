cmake_minimum_required ( VERSION 2.8.8 FATAL_ERROR )

# Set CMake language to Fortran
enable_language ( Fortran )

# Define object binary directory
set ( BIN_DIR ${CMAKE_BINARY_DIR}/bin )
set ( LIB_DIR ${CMAKE_BINARY_DIR}/lib )

# place executables in the binary root directory
set ( EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR} )

# Define lib as the module directory
set ( CMAKE_Fortran_MODULE_DIRECTORY ${LIB_DIR} )

# Define string utility module source directory
set ( DT_SOURCE_DIR ${CMAKE_SOURCE_DIR}/src )
set ( DT_SOURCE ${DT_SOURCE_DIR}/date_type.f90 )

#set ( BUILD_DT_TESTS TRUE )
#if ( BUILD_DT_TESTS )
  # Set the name for string utility tests executable
  set ( DT_TESTS_EXE date_tests )

  # Define tests directory
  set ( DT_TESTS_SOURCE_DIR ${CMAKE_SOURCE_DIR}/tests )
  add_subdirectory ( ${DT_TESTS_SOURCE_DIR} ${BIN} )
#endif
