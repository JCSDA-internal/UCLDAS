program convert_restart_vec2tiles
use module_nf90_utilities
integer            :: location, nx, ny, tiles
integer            :: incid, oncid, nDims, nVars, nAtts, uDimId
character(len=256) :: ifname, ofname
  ifname = '/scratch1/NCEPDEV/da/Azadeh.Gholoubi/GlobalLand/ufs-land-driver/run/restart/ufs_land_output.2000-01-01_00-00-00.nc'
  call unf90_op_ncfile('R', ifname, incid)
  call unf90_get_ncinfo(incid, nDimensions=nDims, nVariables=nVars, nAttributes=nAtts, unlimitedDimID=uDimId)
  ofname = 'out.nc4'
  call unf90_op_ncfile('C', ofname, oncid, 'netcdf4')
  call unf90_close_ncfile(incid)
  call unf90_close_ncfile(oncid)
end program convert_restart_vec2tiles
