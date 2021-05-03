# (C) Copyright 2017-2020 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# noahmp
################################################################################

cmake_minimum_required( VERSION 3.3.2 FATAL_ERROR )

project( noahmp Fortran )

set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH} )

set( CMAKE_DIRECTORY_LABELS "noahmp" )

set( ECBUILD_DEFAULT_BUILD_TYPE Release )
set( ENABLE_OS_TESTS           OFF CACHE BOOL "Disable OS tests" FORCE )
set( ENABLE_LARGE_FILE_SUPPORT OFF CACHE BOOL "Disable testing of large file support" FORCE )
set( ENABLE_MPI ON CACHE BOOL "Compile with MPI" )

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 2.7 )

################################################################################
# Project
################################################################################

ecbuild_declare_project()

ecbuild_enable_fortran( REQUIRED )

set( NOAHMP_LINKER_LANGUAGE Fortran )

################################################################################
# Dependencies
################################################################################

# MPI
ecbuild_add_option( FEATURE MPI DEFAULT ON
                    DESCRIPTION "Support for MPI distributed parallelism"
                    REQUIRED_PACKAGES "MPI COMPONENTS Fortran" )
ecbuild_find_mpi( COMPONENTS Fortran REQUIRED )
ecbuild_include_mpi()
link_libraries(${MPI_Fortran_LIBRARIES})

# OpenMP
if( HAVE_OMP )
  ecbuild_enable_omp()
else()
  ecbuild_enable_ompstubs()
endif()

# NetCDF
find_package( NetCDF REQUIRED COMPONENTS Fortran )
include_directories( ${NETCDF_INCLUDE_DIR} )

################################################################################
# Definitions
################################################################################

################################################################################
# Export package info
################################################################################

set( NOAHMP_INCLUDE_DIRS ${CMAKE_Fortran_MODULE_DIRECTORY}
                         ${CMAKE_CURRENT_SOURCE_DIR}/physics)
set( NOAHMP_LIBRARIES noahmp )

get_directory_property( NOAHMP_DEFINITIONS COMPILE_DEFINITIONS )

list( APPEND NOAHMP_TPLS NetCDF )

foreach( _tpl ${NOAHMP_TPLS} )
  string( TOUPPER ${_tpl} TPL )
  list( APPEND NOAHMP_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS}  ${${TPL}_TPL_DEFINITIONS}  )
  list( APPEND NOAHMP_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} ${${TPL}_TPL_INCLUDE_DIRS} )
  list( APPEND NOAHMP_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES}    ${${TPL}_TPL_LIBRARIES}    )
endforeach()

################################################################################
# Sources
################################################################################

# NOAHMP compiler flags are set here (e.g. -cpp -DCCPP)
include( noahmp_compiler_flags )

include_directories( ${NOAHMP_INCLUDE_DIRS} ${NOAHMP_EXTRA_INCLUDE_DIRS} )

################################################################################

list( APPEND noahmp_src_files
  physics/machine.F
  physics/set_soilveg.f
  physics/sflx.f
  physics/funcphys.f90
  physics/namelist_soilveg.f
  physics/sfc_noahmp_drv.f
  physics/sfc_drv.f
  physics/sfc_diff.f
  physics/date_def.f
  physics/machine.F
  physics/noahmp_tables.f90
  physics/module_sf_noahmp_glacier.f90
  physics/surface_perturbation.F90
  physics/physcons.F90
  physics/module_sf_noahmplsm.f90
)
################################################################################

################################################################################
ecbuild_add_library( TARGET   noahmp
                     SOURCES  ${noahmp_src_files}
                     LIBS     ${NETCDF_LIBRARIES}
                     LINKER_LANGUAGE ${NOAHMP_LINKER_LANGUAGE}
                    )
################################################################################

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU AND CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10)
    target_compile_options(${PROJECT_NAME} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-fallow-invalid-boz>
                                                   $<$<COMPILE_LANGUAGE:Fortran>:-fallow-argument-mismatch>)
endif()

if(HAVE_GETTID)
    target_compile_definitions(noahmp PRIVATE HAVE_GETTID)
endif()


if(ECBUILD_INSTALL_FORTRAN_MODULES)
  install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/${CMAKE_CFG_INTDIR} DESTINATION ${INSTALL_INCLUDE_DIR} )
endif()

################################################################################
# Finalise configuration
################################################################################

# prepares a tar.gz of the sources and/or binaries
ecbuild_install_project( NAME noahmp )

# print the summary of the configuration
ecbuild_print_summary()
