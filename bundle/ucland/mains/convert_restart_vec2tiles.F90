program convert_restart_vec2tiles
use module_nf90_utilities
integer            :: location, nx, ny, tiles
integer            :: incid, oncid, nDims, nVars, nAtts, uDimId
integer            :: iret, dlen
character(len=256) :: ifname, ofname, dname_in, dname_out, attname
  ifname = '/scratch1/NCEPDEV/da/Azadeh.Gholoubi/GlobalLand/ufs-land-driver/run/restart/ufs_land_output.2000-01-01_00-00-00.nc'
  ofname = 'out.nc4'
  nx = 96
  ny = 96
  call unf90_op_ncfile('R', ifname, incid)
  call unf90_op_ncfile('C', ofname, oncid, 'netcdf4')
  call unf90_get_ncinfo(incid, nDimensions=nDims, nVariables=nVars, nAttributes=nAtts, unlimitedDimID=uDimId)
  do iv = 1, ndims
    iret = nf90_inquire_dimension(incid, iv, name=dname_in, len=dlen)
    call unf90_check_err(iret)
    if(trim(dname_in) == 'location') then
        call unf90_def_dim(oncid, nx, 'xaxis_1')
        call unf90_def_dim(oncid, ny, 'yaxis_1')
    else
      if(trim(dname_in) == 'time') then
        dname_out = 'Time'
      else
        dname_out = dname_in
      endif
      if(iv == uDimId) then
        call unf90_def_dim(oncid, 0, dname_out)
      else
        call unf90_def_dim(oncid, dlen, dname_out)
      endif
    endif
  enddo
  call unf90_def_end(oncid)
  call unf90_close_ncfile(incid)
  call unf90_close_ncfile(oncid)
end program convert_restart_vec2tiles
