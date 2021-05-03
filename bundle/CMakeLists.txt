# (C) Copyright 2017-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.


# ------------------------------------------------------------------------- #
# Buldle for UCLDAS (land only)
# ------------------------------------------------------------------------- #

# Create project
project( ucldas-bundle C CXX Fortran )

# Check for minimim cmake requirement
cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

# Include ecbuild_bundle macro
include( ecbuild_bundle )

# Default build mode, change with --build=[release]|debug|bit
set( ECBUILD_DEFAULT_BUILD_TYPE Release )

# Enable MPI
set( ENABLE_MPI ON CACHE BOOL "Compile with MPI" )

# Initialize bundle
# -----------------
ecbuild_bundle_initialize()
ecbuild_requires_macro_version( 2.7 )

# required repositories
ecbuild_bundle( PROJECT crtm        GIT "https://github.com/jcsda/crtm.git"                     TAG v2.3-jedi )
ecbuild_bundle( PROJECT fms         GIT "https://github.com/jcsda-internal/FMS.git"             UPDATE BRANCH release-stable-ecbuild )
ecbuild_bundle( PROJECT eckit       GIT "https://github.com/jcsda-internal/eckit.git"           UPDATE BRANCH release-stable )
ecbuild_bundle( PROJECT fckit       GIT "https://github.com/jcsda-internal/fckit.git"           UPDATE BRANCH release-stable )
ecbuild_bundle( PROJECT atlas       GIT "https://github.com/jcsda-internal/atlas.git"           UPDATE BRANCH release-stable )
ecbuild_bundle( PROJECT oops        GIT "https://github.com/jcsda/oops.git"                     TAG 1.0.0 )
ecbuild_bundle( PROJECT saber       GIT "https://github.com/jcsda/saber.git"                    TAG 1.0.0 )
ecbuild_bundle( PROJECT ioda        GIT "https://github.com/jcsda/ioda.git"                     TAG 1.0.0 )
ecbuild_bundle( PROJECT ufo         GIT "https://github.com/jcsda/ufo.git"                      TAG 1.0.0 )
#ecbuild_bundle( PROJECT ucland       GIT "https://github.com/zhichang-guo/ucland.git"             TAG 0.0.1 )
ecbuild_bundle( PROJECT ucldas        SOURCE "../" )

add_subdirectory( ucland )

# Finalize bundle
# ---------------
ecbuild_bundle_finalize()
