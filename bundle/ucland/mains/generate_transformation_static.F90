program generate_transformation_static
use module_nf90_utilities
implicit none
  integer, parameter :: location   = 16196
  integer, parameter :: nx         = 96
  integer, parameter :: ny         = 96
  integer, parameter :: tiles      = 6
  real               :: rfillValue = 9.96921e+36
  integer            :: ifillValue = -2147483647
  integer            :: ifillMask  = 0
  integer            :: incid, oncid
  integer            :: cube_i(location), cube_j(location), cube_tile(location)
  integer            :: land_mask(nx,ny,tiles), vector_index(nx,ny,tiles)
  integer            :: i, j, tile, ip
  character(len=256) :: ifname, ofname
  ifname = '/scratch1/NCEPDEV/stmp2/Michael.Barlage/forcing/gswp3/static/ufs-land_C96_static_fields.nc'
  ofname = 'ufs-land_C96_static_vec2tiles.nc4'
  call unf90_op_ncfile('R', ifname, incid)
  call unf90_op_ncfile('C', ofname, oncid)
  call unf90_def_dim(oncid, nx,       'xaxis_1')
  call unf90_def_dim(oncid, ny,       'yaxis_1')
  call unf90_def_dim(oncid, tiles,    'tiles')
  call unf90_def_dim(oncid, location, 'location')
  call unf90_def_var(oncid, 'int', 'location', 'cube_i')
  call unf90_io_varatt('w', oncid, 'cube_i', '_FillValue', ifillValue)
  call unf90_def_var(oncid, 'int', 'location', 'cube_j')
  call unf90_io_varatt('w', oncid, 'cube_j', '_FillValue', ifillValue)
  call unf90_def_var(oncid, 'int', 'location', 'cube_tile')
  call unf90_io_varatt('w', oncid, 'cube_tile', '_FillValue', ifillValue)
  call unf90_def_var(oncid, 'int', 'xaxis_1,yaxis_1,tiles', 'land_mask')
  call unf90_io_varatt('w', oncid, 'land_mask', '_FillValue', ifillMask)
  call unf90_def_var(oncid, 'int', 'xaxis_1,yaxis_1,tiles', 'vector_index')
  call unf90_io_varatt('w', oncid, 'vector_index', '_FillValue', ifillValue)
  call unf90_def_end(oncid)
  call unf90_io1d_int('r', incid, location, cube_i,    'cube_i')
  call unf90_io1d_int('r', incid, location, cube_j,    'cube_j')
  call unf90_io1d_int('r', incid, location, cube_tile, 'cube_tile')
  do i = 1, nx
    do j = 1, ny
      do tile = 1, tiles
        land_mask(i,j,tile) = 0
        vector_index(i,j,tile) = -1
      enddo
    enddo
  enddo
  do ip = 1, location
    land_mask(cube_j(ip),cube_i(ip),cube_tile(ip)) = 1
    vector_index(cube_j(ip),cube_i(ip),cube_tile(ip)) = ip
  enddo
  call unf90_io1d_int('w', oncid, location, cube_j,    'cube_i')
  call unf90_io1d_int('w', oncid, location, cube_i,    'cube_j')
  call unf90_io1d_int('w', oncid, location, cube_tile, 'cube_tile')
  call unf90_io_var('w', oncid, nx, ny, tiles, land_mask,    'land_mask') 
  call unf90_io_var('w', oncid, nx, ny, tiles, vector_index, 'vector_index') 
  call unf90_close_ncfile(incid)
  call unf90_close_ncfile(oncid)
end program generate_transformation_static
