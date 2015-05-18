# Define tests source files
set ( DT_TESTS_SOURCE
  ${DT_TESTS_SOURCE_DIR}/tests.f90
  ${DT_SOURCE}
)

# Define tests executable
add_executable ( ${DT_TESTS_EXE} ${DT_TESTS_SOURCE} )
