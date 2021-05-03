list ( APPEND _files
src/framework/UCLAND_coms.F90
src/framework/UCLAND_cpu_clock.F90
src/framework/UCLAND_domains.F90
src/framework/UCLAND_error_handler.F90
src/framework/UCLAND_get_input.F90
src/framework/UCLAND_io.F90
src/framework/UCLAND_string_functions.F90
src/framework/UCLAND_time_manager.F90
src/framework/UCLAND_write_cputime.F90
)
set ( src_files
      ${_files}
      PARENT_SCOPE
)
