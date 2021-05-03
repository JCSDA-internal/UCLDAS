list( APPEND utils_src_files
module_interpolation_utilities.f90
module_cosine_zenith.f90
module_time_utilities.f90
module_error_handling.f90
module_nf90_utilities.F90
)

#Append includes and libraries with particular model
list (APPEND UTILS_LIB_DEP ${NETCDF_LIBRARIES})
list (APPEND UTILS_LIB_DEP ${NOAHMP_LIBRARIES})

#Append src with particular model
list (APPEND UTILS_SRC_DEP ${utils_src_files} )

#Add the library
ecbuild_add_library( TARGET          utils
                     SOURCES         ${utils_src_files}
                     LIBS            ${UTILS_LIB_DEP}
                     LINKER_LANGUAGE ${UTILS_LINKER_LANGUAGE}
                   )
