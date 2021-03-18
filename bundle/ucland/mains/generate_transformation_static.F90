program generate_transformation_static
use module_nf90_utilities
implicit none
  integer :: location = 16196
  integer :: nx       = 96
  integer :: ny       = 96
  integer :: tiles    = 6
  integer :: incid, oncid
  character(len=256) :: ifname, ofname
  ifname = '/scratch1/NCEPDEV/stmp2/Michael.Barlage/forcing/gswp3/static/ufs-land_C96_static_fields.nc'
  ofname = 'ufs-land_C96_static_vec2tiles.nc4'
  call unf90_op_ncfile('R', ifname, incid)
  call unf90_op_ncfile('C', ofname, oncid, 'netcdf4')
  call unf90_def_dim(oncid, nx, 'xaxis_1')
  call unf90_def_dim(oncid, ny, 'yaxis_1')
  call unf90_def_dim(oncid, location, 'location')
  call unf90_def_end(oncid)
  call unf90_close_ncfile(incid)
  call unf90_close_ncfile(oncid)
end program generate_transformation_static
