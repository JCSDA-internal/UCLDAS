list( APPEND driver_src_files
ufsLandNoahMPRestartModule.f90
ufsLandNoahRestartModule.f90 
ufsLandStaticModule.f90
ufsLandNoahType.f90
ufsLandNoahMPType.f90
ufsLandForcingModule.f90
ufsLandNoahDriverModule.f90
ufsLandNoahMPDriverModule.f90
ufsLandIOModule.f90
ufsLandInitialModule.f90
ufsLandNamelistRead.f90
UCLAND_domains.F90
UCLAND_io.F90
)

#Append includes and libraries with particular model
list (APPEND DRIVER_LIB_DEP ${NETCDF_LIBRARIES})
list (APPEND DRIVER_LIB_DEP ${NOAHMP_LIBRARIES})
list (APPEND DRIVER_LIB_DEP ${UTILS_LIBRARIES})

#Append src with particular model
list (APPEND DRIVER_SRC_DEP ${driver_src_files} )

#Add the library
ecbuild_add_library( TARGET          driver
                     SOURCES         ${driver_src_files}
                     LIBS            ${DRIVER_LIB_DEP} ${FMS_LIBRARIES}
                     LINKER_LANGUAGE ${DRIVER_LINKER_LANGUAGE}
                   )
