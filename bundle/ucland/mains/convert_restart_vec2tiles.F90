program convert_restart_vec2tiles
use module_nf90_utilities
integer            :: ncid
character(len=256) :: fname
  fname = 'a.nc4'
  call op_ncfile('C', fname, ncid, 'netcdf4')
  call close_ncfile(ncid)
end program convert_restart_vec2tiles
