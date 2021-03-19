module module_nf90_utilities
  use netcdf
  implicit none
  public unf90_io_varatt, unf90_io_var
  interface unf90_io_varatt
    module procedure unf90_io_varatt_char
    module procedure unf90_io_varatt_real
    module procedure unf90_io_varatt_int
  end interface
  interface unf90_io_var
    module procedure unf90_io1d_int
    module procedure unf90_io1d_real
    module procedure unf90_io1d_double
    module procedure unf90_io2d_real
    module procedure unf90_io2d_double
    module procedure unf90_io1d_time_real
    module procedure unf90_io1d_time_double
    module procedure unf90_io2d_time_real
    module procedure unf90_io2d_time_double
    module procedure unf90_io3d_real
    module procedure unf90_io3d_int
    module procedure unf90_io3d_double
    module procedure unf90_io4d_real
    module procedure unf90_io4d_double
  end interface
  public unf90_get_var_type
  !public utools_subStrCounts
  contains
!-------------------------------------------
  subroutine unf90_op_ncfile(option, fname, ncid, ncformat)
  implicit none
  character(len=*),           intent(in)  :: option, fname
  character(len=*), optional, intent(in)  :: ncformat
  integer,                    intent(out) :: ncid
  integer                                 :: iret, mode
  if(trim(option) == 'C' .or. trim(option) == 'c') then
    if(present(ncformat) .and. trim(ncformat) == 'netcdf4') then
      iret = nf90_create(fname, nf90_netcdf4, ncid)
    else
      iret = nf90_create(fname, nf90_clobber, ncid)
    endif
  else
    if(trim(option) == 'R' .or. trim(option) == 'r') then
      mode = nf90_nowrite
    else if(trim(option) == 'W' .or. trim(option) == 'w') then
      mode = nf90_write
    else
      stop 'invalid mode option in op_ncfile'
    endif
    iret = nf90_open(trim(fname), mode, ncid)
    if(iret /= nf90_noerr) then
      iret = nf90_open(trim(fname)//'4', mode, ncid)
    endif
    if(iret /= nf90_noerr) then
      write(*,*) 'file '//trim(fname)//' not found!'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_op_ncfile
!-------------------------------------------
  subroutine unf90_def_dim(ncid, ii, dname)
  implicit none
  integer,          intent(in) :: ncid, ii
  character(len=*), intent(in) :: dname
  integer                      :: iret, id
  if(ii == 0) then
    iret = nf90_def_dim(ncid, trim(dname), nf90_unlimited, id)
  else
    iret = nf90_def_dim(ncid, trim(dname), ii, id)
  endif
  call unf90_check_err(iret)
  end subroutine unf90_def_dim
!-------------------------------------------
  subroutine unf90_get_dimid(ncid, dname, dimid)
  implicit none
  integer,          intent(in)  :: ncid
  character(len=*), intent(in)  :: dname
  integer,          intent(out) :: dimid
  integer                       :: iret
  iret = nf90_inq_dimid(ncid, trim(dname), dimid)
  call unf90_check_err(iret)
  end subroutine unf90_get_dimid
!-------------------------------------------
  subroutine unf90_get_dimlen(ncid, dname, len)
  implicit none
  integer,          intent(in)  :: ncid
  character(len=*), intent(in)  :: dname
  integer,          intent(out) :: len
  integer                       :: iret, dimid
  character(len=20)             :: dimname
  iret = nf90_inq_dimid(ncid, trim(dname), dimid)
  call unf90_check_err(iret)
  iret = nf90_inquire_dimension(ncid, dimid, dimname, len)
  call unf90_check_err(iret)
  end subroutine unf90_get_dimlen
!----------------------------------------
  subroutine unf90_get_var_dims(ncid, vname, vdims)
  implicit none
  integer,          intent(in)  :: ncid
  character(len=*), intent(in)  :: vname
  integer,          intent(out) :: vdims
  character(len=10)             :: varname
  integer                       :: iret, varid
  iret = nf90_inq_varid(ncid, vname, varid)
  call unf90_check_err(iret)
  iret = nf90_inquire_variable(ncid, varid, name=varname, ndims=vdims)
  call unf90_check_err(iret)
  end subroutine unf90_get_var_dims
!----------------------------------------
  subroutine unf90_get_ncinfo(ncid, ndimensions, nvariables, nattributes, unlimiteddimid)
  implicit none
  integer,           intent(in)  :: ncid
  integer, optional, intent(out) :: ndimensions, nvariables, nattributes, unlimiteddimid
  integer                        :: iret
  iret = nf90_inquire(ncid, ndimensions, nvariables, nattributes, unlimiteddimid)
  call unf90_check_err(iret)
  end subroutine unf90_get_ncinfo
!----------------------------------------
  subroutine unf90_copy_global_att(ncid_in, ncid_out)
  implicit none
  integer, intent(in) :: ncid_in, ncid_out
  integer             :: iret, natts, i
  character(len=128)  :: attname
    do i = 1, 50
      iret = nf90_inq_attname(ncid_in, nf90_global, i, attname)
      if(iret /= nf90_noerr) then
        natts = i
        exit
      endif
      iret = nf90_copy_att(ncid_in, nf90_global, attname, ncid_out, nf90_global)
      call unf90_check_err(iret)
    enddo
  end subroutine unf90_copy_global_att
!----------------------------------------
  subroutine unf90_copy_var(incid, oncid, vname)
  implicit none
  integer, intent(in)           :: incid, oncid
  character(len=*), intent(in)  :: vname
  real, allocatable             :: r1d(:)
  double precision, allocatable :: d1d(:)
  integer, allocatable          :: i1d(:), istart(:), icount(:), vdimids(:), dlen(:)
  integer                       :: xtype, vdims, iret, ivarid, ovarid, loops, loop
  integer                       :: i, iv, blocks, dimid, acc
  loops = 1
  iret = nf90_inq_varid(incid, trim(vname), ivarid)
  call unf90_check_err(iret)
  iret = nf90_inq_varid(oncid, trim(vname), ovarid)
  call unf90_check_err(iret)
  iret = nf90_inquire_variable(incid, ivarid, xtype=xtype, ndims=vdims)
  call unf90_check_err(iret)
  if(vdims > 0) then
    allocate(vdimids(vdims))
    allocate(dlen(vdims))
    allocate(istart(vdims))
    allocate(icount(vdims))
    iret = nf90_inquire_variable(incid, ivarid, dimids=vdimids(:))
    call unf90_check_err(iret)
    do iv = 1, vdims
      iret = nf90_inquire_dimension(incid, vdimids(iv), len=dlen(iv))
      call unf90_check_err(iret)  
      loops = loops*dlen(iv)
    enddo
    loops = loops/dlen(1)
    blocks = 0
    istart = 1
    icount = 1
    icount(1) = dlen(1)
    dimid = 1
    do loop = 1, loops
      blocks = blocks+dlen(1)
      if(xtype == nf90_float) then
        allocate(r1d(dlen(1)))
        call unf90_io1d_real('r', incid, dlen(1), r1d, vname, istart, icount)
        call unf90_io1d_real('w', oncid, dlen(1), r1d, vname, istart, icount)
        deallocate(r1d)
      else if(xtype == nf90_double) then
        allocate(d1d(dlen(1)))
        call unf90_io1d_double('r', incid, dlen(1), d1d, vname, istart, icount)
        call unf90_io1d_double('w', oncid, dlen(1), d1d, vname, istart, icount)
        deallocate(d1d)
      else if(xtype == nf90_int) then
        allocate(i1d(dlen(1)))
        call unf90_io1d_int('r', incid, dlen(1), i1d, vname, istart, icount)
        call unf90_io1d_int('w', oncid, dlen(1), i1d, vname, istart, icount)
        deallocate(i1d)
      else
        write(*,*) 'error: invalid xtype in copy_var'
        stop 
      endif
      acc = 1
      do i = 1, dimid
        acc = acc*dlen(i)
      enddo
      if(blocks >= acc .and. loop /= loops) then
        dimid = dimid + 1
        istart(dimid) = 1
      else
        istart(dimid) = istart(dimid) + 1
      endif
    enddo
    deallocate(vdimids)
    deallocate(dlen)
    deallocate(istart)
    deallocate(icount)
  endif
  end subroutine unf90_copy_var
!----------------------------------------
  subroutine unf90_copy_var_att(ncid_in, vname_in, ncid_out, vname_out)
  implicit none
  character(len=*), intent(in) :: vname_in, vname_out
  integer,          intent(in) :: ncid_in, ncid_out
  integer                      :: iret, varid_in, varid_out, natts, i
  character(len=20)            :: attname
  iret = nf90_inq_varid(ncid_in,  vname_in,  varid_in)
  call unf90_check_err(iret)
  iret = nf90_inquire_variable(ncid_in, varid_in, natts=natts)
  call unf90_check_err(iret)
  iret = nf90_inq_varid(ncid_out, vname_out, varid_out)
  call unf90_check_err(iret)
  do i = 1, natts
    iret = nf90_inq_attname(ncid_in, varid_in, i, attname)
    call unf90_check_err(iret)
    iret = nf90_copy_att(ncid_in, varid_in, attname, ncid_out, varid_out)
    call unf90_check_err(iret)
  enddo
  end subroutine unf90_copy_var_att
!----------------------------------------
  function unf90_get_var_type(option) result(vartype)
  implicit none
  character(len=*), intent(in) :: option
  integer                      :: vartype
  if(trim(option) == 'float' .or. trim(option) == 'FLOAT') then
    vartype = nf90_float
  else if(trim(option) == 'double' .or. trim(option) == 'DOUBLE') then
    vartype = nf90_double
  else if(trim(option) == 'short' .or. trim(option) == 'SHORT') then
    vartype = nf90_short
  else if(trim(option) == 'char' .or. trim(option) == 'CHAR') then
    vartype = nf90_char
  else if(trim(option) == 'int' .or. trim(option) == 'INT') then
    vartype = nf90_int
  else
    stop 'invalid option in get_var_type'
  endif
  end function unf90_get_var_type
!----------------------------------------
  subroutine unf90_def_var(ncid, option, dnames, vname, varattname, varatt)
  implicit none
  integer,                    intent(in) :: ncid
  character(len=*),           intent(in) :: option, dnames, vname
  character(len=*), optional, intent(in) :: varattname, varatt
  character(len=256)                     :: tmpvaratt
  integer                                :: i, iret, varid, vartype, dims
  integer                                :: utools_subStrCounts
  character(len=20), allocatable         :: dname(:)
  integer, allocatable                   :: dimid(:)
  dims = utools_subStrCounts(dnames, ',')
  allocate(dname(dims))
  allocate(dimid(dims))
  call utools_split(dnames, dims, dname)
  do i = 1, dims
    call unf90_get_dimid(ncid, dname(i), dimid(i))
  enddo
  vartype = unf90_get_var_type(option)
  iret = nf90_def_var(ncid, trim(vname), vartype, dimid, varid)
  call unf90_check_err(iret)
  if(present(varattname)) then
    tmpvaratt = varatt
    call unf90_io_varatt_char('w', ncid, vname, varattname, tmpvaratt)
  endif
  deallocate(dname)
  deallocate(dimid)
  end subroutine unf90_def_var
!----------------------------------------
  subroutine unf90_io_varatt_char(option, ncid, vname, aname, note)
  implicit none
  integer, intent(in)             :: ncid
  character(len=*), intent(in)    :: option, vname, aname
  character(len=*), intent(inout) :: note
  integer                         :: varid, iret
  if(trim(vname) == 'nf90_global') then
    if(trim(option) == 'r' .or. trim(option) == 'R') then
      iret = nf90_get_att(ncid, nf90_global, trim(aname), note)
    else if(trim(option) == 'w' .or. trim(option) == 'W') then
      iret = nf90_put_att(ncid, nf90_global, trim(aname), note)
    else
      stop 'invalid option'
    endif
  else
    iret = nf90_inq_varid(ncid, trim(vname), varid)
    call unf90_check_err(iret)
    if(trim(option) == 'r' .or. trim(option) == 'R') then
      iret = nf90_get_att(ncid, varid, trim(aname), note)
    else if(trim(option) == 'w' .or. trim(option) == 'W') then
      iret = nf90_put_att(ncid, varid, trim(aname), note)
    else
      stop 'invalid option'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io_varatt_char
!----------------------------------------
  subroutine unf90_io_varatt_int(option, ncid, vname, aname, value)
  implicit none
  integer, intent(in)           :: ncid
  character(len=*), intent(in)  :: option, vname, aname
  integer, intent(inout)        :: value
  integer                       :: varid, iret
  if(trim(vname) == 'nf90_global') then
    if(trim(option) == 'r') then
      iret = nf90_get_att(ncid, nf90_global, trim(aname), value)
    else if(trim(option) == 'w') then
      iret = nf90_put_att(ncid, nf90_global, trim(aname), value)
    else
      stop 'invalid option'
    endif
  else
    iret = nf90_inq_varid(ncid, trim(vname), varid)
    call unf90_check_err(iret)
    if(trim(option) == 'r') then
      iret = nf90_get_att(ncid, varid, trim(aname), value)
    else if(trim(option) == 'w') then
      iret = nf90_put_att(ncid, varid, trim(aname), value)
    else
      stop 'invalid option'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io_varatt_int
!----------------------------------------
  subroutine unf90_io_varatt_real(option, ncid, varname, vname, value)
  implicit none
  integer,          intent(in)    :: ncid
  character(len=*), intent(in)    :: option, varname, vname
  real,             intent(inout) :: value
  integer                         :: varid, iret
  if(trim(varname) == 'nf90_global') then
    if(trim(option) == 'r') then
      iret = nf90_get_att(ncid, nf90_global, trim(vname), value)
    else if(trim(option) == 'w') then
      iret = nf90_put_att(ncid, nf90_global, trim(vname), value)
    else
      stop 'invalid option'
    endif
  else
    iret = nf90_inq_varid(ncid, trim(varname), varid)
    call unf90_check_err(iret)
    if(trim(option) == 'r') then
      iret = nf90_get_att(ncid, varid, trim(vname), value)
    else if(trim(option) == 'w') then
      iret = nf90_put_att(ncid, varid, trim(vname), value)
    else
      stop 'invalid option'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io_varatt_real
!----------------------------------------
  subroutine unf90_def_end(ncid)
  implicit none
  integer, intent(in) :: ncid
  integer             :: iret
  iret = nf90_enddef(ncid)
  call unf90_check_err(iret)
  end subroutine unf90_def_end
!----------------------------------------
  subroutine unf90_io0d_real(mode, ncid, rvalue, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid
  real,             intent(inout) :: rvalue
  character(len=*), intent(in)    :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, rvalue)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, rvalue)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io0d_real
!----------------------------------------
  subroutine unf90_io0d_double(mode, ncid, dvalue, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid
  double precision, intent(inout) :: dvalue
  character(len=*), intent(in)    :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, dvalue)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, dvalue)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io0d_double
!----------------------------------------
  subroutine unf90_io1d_int(mode, ncid, ii, i1d, vname, istart, icount)
  implicit none
  character(len=1),  intent(in)    :: mode
  integer,           intent(in)    :: ncid, ii
  integer,           intent(inout) :: i1d(ii)
  character(len=*),  intent(in)    :: vname
  integer, optional, intent(in)    :: istart(:), icount(:)
  integer                          :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(present(icount)) then
    if(mode == 'r' .or. mode == 'r') then
      iret = nf90_get_var(ncid, varid, i1d, istart, icount)
    else if(mode == 'w' .or. mode == 'w') then
      iret = nf90_put_var(ncid, varid, i1d, istart, icount)
    else
      stop 'invalid mode'
    endif
  else
    if(mode == 'r' .or. mode == 'r') then
      iret = nf90_get_var(ncid, varid, i1d)
    else if(mode == 'w' .or. mode == 'w') then
      iret = nf90_put_var(ncid, varid, i1d)
    else
      stop 'invalid mode'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io1d_int
!----------------------------------------
  subroutine unf90_io1d_real(mode, ncid, ii, r1d, vname, istart, icount)
  implicit none
  character(len=1),  intent(in)    :: mode
  integer,           intent(in)    :: ncid, ii
  real,              intent(inout) :: r1d(ii)
  character(len=*),  intent(in)    :: vname
  integer, optional, intent(in)    :: istart(:), icount(:)
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(present(icount)) then
    if(mode == 'r' .or. mode == 'R') then
      iret = nf90_get_var(ncid, varid, r1d, istart, icount)
    else if(mode == 'w' .or. mode == 'W') then
      iret = nf90_put_var(ncid, varid, r1d, istart, icount)
    else
      stop 'invalid mode'
    endif
  else
    if(mode == 'r' .or. mode == 'R') then
      iret = nf90_get_var(ncid, varid, r1d)
    else if(mode == 'w' .or. mode == 'W') then
      iret = nf90_put_var(ncid, varid, r1d)
    else
      stop 'invalid mode'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io1d_real
!----------------------------------------
  subroutine unf90_io1d_double(mode, ncid, ii, d1d, vname, istart, icount)
  implicit none
  character(len=1),  intent(in)    :: mode
  integer,           intent(in)    :: ncid, ii
  double precision,  intent(inout) :: d1d(ii)
  character(len=*),  intent(in)    :: vname
  integer, optional, intent(in)    :: istart(:), icount(:)
  integer                          :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(present(icount)) then
    if(mode == 'r' .or. mode == 'R') then
      iret = nf90_get_var(ncid, varid, d1d, istart, icount)
    else if(mode == 'w' .or. mode == 'W') then
      iret = nf90_put_var(ncid, varid, d1d, istart, icount)
    else
      stop 'invalid mode'
    endif
  else
    if(mode == 'r' .or. mode == 'R') then
      iret = nf90_get_var(ncid, varid, d1d)
    else if(mode == 'w' .or. mode == 'W') then
      iret = nf90_put_var(ncid, varid, d1d)
    else
      stop 'invalid mode'
    endif
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io1d_double
!----------------------------------------
  subroutine unf90_io1d_time_real(mode, ncid, lon, tid, r1d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, tid
  real,             intent(inout) :: r1d(lon)
  character(len=*)                :: vname
  integer                         :: iret, varid, istart(2), icount(2)
  istart = 1
  icount = 1
  istart(2) = tid
  icount(1) = lon
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, r1d, istart, icount)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, r1d, istart, icount)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io1d_time_real
!----------------------------------------
  subroutine unf90_io1d_time_double(mode, ncid, lon, tid, d1d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, tid
  double precision, intent(inout) :: d1d(lon)
  character(len=*)                :: vname
  integer                         :: iret, varid, istart(2), icount(2)
  istart = 1
  icount = 1
  istart(2) = tid
  icount(1) = lon
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, d1d, istart, icount)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, d1d, istart, icount)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io1d_time_double
!----------------------------------------
  subroutine unf90_io2d_double(mode, ncid, ii, jj, d2d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ii, jj, ncid
  double precision, intent(inout) :: d2d(ii,jj)
  character(len=*), intent(in)    :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, d2d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, d2d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io2d_double
!----------------------------------------
  subroutine unf90_io2d_real(mode, ncid, ii, jj, r2d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, ii, jj
  real,             intent(inout) :: r2d(ii,jj)
  character(len=*), intent(in)    :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, r2d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, r2d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io2d_real
!----------------------------------------
  subroutine unf90_io2d_time_real(mode, ncid, lon, lat, tid, r2d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, lat, tid
  real,             intent(inout) :: r2d(lon, lat)
  character(len=*), intent(in)    :: vname
  integer                         :: iret, varid, istart(3), icount(3)
  istart = 1
  icount = 1
  istart(3) = tid
  icount(1) = lon
  icount(2) = lat
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, r2d, istart, icount)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, r2d, istart, icount)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io2d_time_real
!----------------------------------------
  subroutine unf90_io2d_time_double(mode, ncid, lon, lat, tid, d2d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, lat, tid
  double precision, intent(inout) :: d2d(lon, lat)
  character(len=*), intent(in)    :: vname
  integer                         :: iret, varid, istart(3), icount(3)
  istart = 1
  icount = 1
  istart(3) = tid
  icount(1) = lon
  icount(2) = lat
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, d2d, istart, icount)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, d2d, istart, icount)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io2d_time_double
!----------------------------------------
  subroutine unf90_io3d_int(mode, ncid, lon, lat, lev, i3d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, lat, lev
  integer,          intent(inout) :: i3d(lon,lat,lev)
  character(len=*)                :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, i3d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, i3d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io3d_int
!----------------------------------------
  subroutine unf90_io3d_real(mode, ncid, lon, lat, lev, r3d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, lat, lev
  real,             intent(inout) :: r3d(lon,lat,lev)
  character(len=*)                :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, r3d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, r3d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io3d_real
!----------------------------------------
  subroutine unf90_io3d_double(mode, ncid, lon, lat, lev, d3d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, lon, lat, lev
  double precision, intent(inout) :: d3d(lon,lat,lev)
  character(len=*)                :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, d3d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, d3d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io3d_double
!----------------------------------------
  subroutine unf90_io4d_real(mode, ncid, xds, yds, zds, tds, r4d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, xds, yds, zds, tds
  real,             intent(inout) :: r4d(xds,yds,zds,tds)
  character(len=*)                :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, r4d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, r4d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io4d_real
!----------------------------------------
  subroutine unf90_io4d_double(mode, ncid, xds, yds, zds, tds, d4d, vname)
  implicit none
  character(len=1), intent(in)    :: mode
  integer,          intent(in)    :: ncid, xds, yds, zds, tds
  double precision, intent(inout) :: d4d(xds,yds,zds,tds)
  character(len=*)                :: vname
  integer                         :: iret, varid
  iret = nf90_inq_varid(ncid, trim(vname), varid)
  call unf90_check_err(iret)
  if(mode == 'r' .or. mode == 'R') then
    iret = nf90_get_var(ncid, varid, d4d)
  else if(mode == 'w' .or. mode == 'W') then
    iret = nf90_put_var(ncid, varid, d4d)
  else
    stop 'invalid mode'
  endif
  call unf90_check_err(iret)
  end subroutine unf90_io4d_double
!-------------------------------------------
  subroutine unf90_close_ncfile(ncid)
  implicit none
  integer, intent(in) :: ncid
  integer             :: iret
  iret = nf90_close(ncid)
  call unf90_check_err(iret)
  end subroutine unf90_close_ncfile
!-------------------------------------------
  subroutine unf90_check_err(iret, nonstop)
  implicit none
  integer,           intent(in) :: iret
  logical, optional, intent(in) :: nonstop
    if(iret /= nf90_noerr) then
      print*, nf90_strerror(iret)
      if(present(nonstop) .and. nonstop) return
      stop
    endif
  end subroutine unf90_check_err
end module module_nf90_utilities
!----------------------------------------
subroutine utools_split(line, dlen, str)
  implicit none
  integer, intent(in)           :: dlen
  character(len=*), intent(in)  :: line
  character(len=*), intent(out) :: str(dlen)
  read(line,*) str(1:dlen)
end subroutine utools_split
!----------------------------------------
function utools_subStrCounts(str, delim)
  implicit none
  character(len=*), intent(in) :: str, delim
  integer                      :: i, utools_subStrCounts
  utools_subStrCounts = COUNT([(str(i:i),i=1,len_trim(str))] .eq. trim(delim)) + 1
end function utools_subStrCounts
