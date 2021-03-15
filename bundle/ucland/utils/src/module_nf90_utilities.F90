module module_nf90_utilities
use netcdf
implicit none
contains
!-------------------------------------------
  subroutine op_ncfile(crw, fname, ncid, ncformat)
  implicit none
  character(len=*),           intent(in)  :: crw, fname
  character(len=*), optional, intent(in)  :: ncformat
  integer,                    intent(out) :: ncid
  integer                                 :: iret, mode
  if(trim(crw) == 'C' .or. trim(crw) == 'c') then
    if(present(ncformat) .and. trim(ncformat) == 'netcdf4') then
      iret = nf90_create(fname, nf90_netcdf4, ncid)
    else
      iret = nf90_create(fname, nf90_clobber, ncid)
    endif
  else
    if(trim(crw) == 'R' .or. trim(crw) == 'r') then
      mode = nf90_nowrite
    else if(trim(crw) == 'W' .or. trim(crw) == 'w') then
      mode = nf90_write
    else
      stop 'invalid mode crw in op_ncfile'
    endif
    iret = nf90_open(trim(fname), mode, ncid)
    if(iret /= nf90_noerr) then
      iret = nf90_open(trim(fname)//'4', mode, ncid)
    endif
  endif
  call check_err(iret)
  end subroutine op_ncfile
!-------------------------------------------
  subroutine close_ncfile(ncid)
  implicit none
  integer, intent(in) :: ncid
  integer             :: iret
  iret = nf90_close(ncid)
  call check_err(iret)
  end subroutine close_ncfile
!---------------------------------------------------------------------
  subroutine check_err(iret, nonstop)
  implicit none
  integer,           intent(in) :: iret
  logical, optional, intent(in) :: nonstop
    if(iret /= nf90_noerr) then
      print*, nf90_strerror(iret)
      if(present(nonstop) .and. nonstop) return
      stop
    endif
  end subroutine check_err
end module module_nf90_utilities
