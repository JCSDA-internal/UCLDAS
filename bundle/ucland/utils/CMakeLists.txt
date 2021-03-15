################################################################################
# UTILS
################################################################################

cmake_minimum_required( VERSION 3.3.2 FATAL_ERROR )

project( utils VERSION 1.0.0 LANGUAGES Fortran )

set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH})

set( CMAKE_DIRECTORY_LABELS "utils" )

set( ECBUILD_DEFAULT_BUILD_TYPE Release )
set( ENABLE_MPI ON CACHE BOOL "Compile with MPI" )

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 2.5 )

################################################################################
# Project
################################################################################

ecbuild_declare_project()

ecbuild_enable_fortran( REQUIRED )

set( UTILS_LINKER_LANGUAGE Fortran )

################################################################################
# Standard compiler flags
################################################################################

include( utils_extra_macros )

################################################################################
# Dependencies
################################################################################

# # MPI
# ecbuild_find_mpi( COMPONENTS Fortran REQUIRED )
# ecbuild_include_mpi()
# link_libraries(${MPI_Fortran_LIBRARIES})

# NetCDF
find_package( NetCDF REQUIRED COMPONENTS Fortran )
include_directories( ${NETCDF_INCLUDE_DIR} )

################################################################################
# Export package info
################################################################################

list( APPEND UTILS_TPLS NetCDF)

set( UTILS_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}
                        ${CMAKE_CURRENT_SOURCE_DIR}/src
                        ${CMAKE_Fortran_MODULE_DIRECTORY})

set( UTILS_LIBRARIES utils )

get_directory_property( UTILS_DEFINITIONS COMPILE_DEFINITIONS )

foreach( _tpl ${UTILS_TPLS} )
  string( TOUPPER ${_tpl} TPL )
  list( APPEND UTILS_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS}  ${${TPL}_TPL_DEFINITIONS}  )
  list( APPEND UTILS_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} ${${TPL}_TPL_INCLUDE_DIRS} )
  list( APPEND UTILS_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES}    ${${TPL}_TPL_LIBRARIES}    )
endforeach()

################################################################################
# Sources
################################################################################

include_directories( ${UTILS_INCLUDE_DIRS}
                     ${UTILS_EXTRA_INCLUDE_DIRS} )

add_subdirectory( src )

################################################################################
# Finalise configuration
################################################################################

# prepares a tar.gz of the sources and/or binaries
ecbuild_install_project( NAME utils )

# print the summary of the configuration
ecbuild_print_summary()
