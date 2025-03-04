module grid_mod
   
contains
   
   subroutine read_dims(filename, gridname, nlon, nlat)
      use netcdf
      implicit none
      character(len=*), intent(in) :: filename
      character(len=*), intent(in) :: gridname
      integer, intent(out) :: nlon, nlat
      integer :: kinfo, ncid, dimids(2), varid

      kinfo = nf90_open(filename, NF90_NOWRITE, ncid)
      kinfo = nf90_inq_varid(ncid, trim(gridname)//'.lon', varid)
      kinfo = nf90_inquire_variable(ncid, varid, dimids=dimids)
      kinfo = nf90_inquire_dimension(ncid, dimids(1), len=nlon)
      kinfo = nf90_inquire_dimension(ncid, dimids(2), len=nlat)
      kinfo = nf90_close(ncid)
   end subroutine read_dims

   subroutine read_coords(filename, gridname, lon, lat)
      use netcdf
      implicit none
      character(len=*), intent(in) :: filename
      character(len=*), intent(in) :: gridname
      real(kind=8), intent(out) :: lon(:, :), lat(:, :)
      integer :: kinfo, ncid, varid

      kinfo = nf90_open(filename, NF90_NOWRITE, ncid)

      kinfo = nf90_inq_varid(ncid, trim(gridname)//'.lon', varid)
      kinfo = nf90_get_var(ncid, varid, lon)

      kinfo = nf90_inq_varid(ncid,trim(gridname)//'.lat', varid)
      kinfo = nf90_get_var(ncid, varid, lat)

      kinfo = nf90_close(ncid)
   end subroutine read_coords

end module grid_mod