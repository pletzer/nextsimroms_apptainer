program receiver
   use mpi
   use mod_oasis
   use netcdf
   implicit none
   integer :: i, j, k, kinfo, date
   integer :: comp_id, part_id, var_id
   integer :: part_params(OASIS_Serial_Params)
   integer :: var_nodims(2)
   character(len=8) :: comp_name = "receiver"
   character(len=8) :: var_name = "FRECVANA"
   real(kind=8) :: error, epsilon
   integer, parameter :: nx_global = 144, ny_global = 143
   real(kind=8) ::  bundle(nx_global,ny_global,2)
   real(kind=8) ::  expected(nx_global,ny_global,2)
   integer :: n_points = nx_global*ny_global
   integer :: ncid, varid
   real(kind=8) :: lon(nx_global,ny_global), lat(nx_global,ny_global)
   integer :: imsk(nx_global,ny_global)
   real(kind=8) :: dp_conv
   logical :: success

   call oasis_init_comp(comp_id, comp_name, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_init_comp: ", rcode=kinfo)
   print '(A,I0)', "receiver: Component ID: ", comp_id

   kinfo = nf90_open('grids.nc',NF90_NOWRITE,ncid)
   kinfo = nf90_inq_varid(ncid,'bggd.lon',varid)
   kinfo = nf90_get_var(ncid,varid,lon)
   kinfo = nf90_inq_varid(ncid,'bggd.lat',varid)
   kinfo = nf90_get_var(ncid,varid,lat)
   kinfo = nf90_close(ncid)

   kinfo = nf90_open('masks.nc',NF90_NOWRITE,ncid)
   kinfo = nf90_inq_varid(ncid,'bggd.msk',varid)
   kinfo = nf90_get_var(ncid,varid,imsk)
   kinfo = nf90_close(ncid)

   part_params(OASIS_Strategy) = OASIS_Serial
   part_params(OASIS_Length)   = n_points
   call oasis_def_partition(part_id, part_params, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_def_partition: ", rcode=kinfo)

   var_nodims=[1, 2]
   print '(2A)', "Receiver: var_name: ", var_name
   call oasis_def_var(var_id, var_name, part_id, var_nodims, OASIS_IN, &
      &              [1], OASIS_DOUBLE, kinfo)
   if(kinfo<0 .or. var_id<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_def_var: ", rcode=kinfo)

   call oasis_enddef(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_enddef: ", rcode=kinfo)

   date=0

   bundle(:,:,:)=0

   call oasis_get(var_id, date, bundle, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_get: ", rcode=kinfo)

   call oasis_terminate(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_terminate: ", rcode=kinfo)

   dp_conv = atan(1.)/45.0
   do j = 1, ny_global
      do i = 1, nx_global
         expected(i,j,1) = 2.0 + (sin(2.*lat(i,j)*dp_conv))**4 * &
            & cos(4.*lon(i,j)*dp_conv)
         expected(i,j,2) = 2.0 - cos(atan(1.0)*4.* &
            & (acos(cos(lon(i,j)*dp_conv)*cos(lat(i,j)*dp_conv))/ &
            & (1.2*atan(1.)*4)))
      end do
   end do

   epsilon=1.e-3
   success = .true.
   do k = 1, 2
      error=0.
      do j = 1, ny_global
         do i = 1, nx_global
            if (imsk(i,j) == 0) &
               & error = error + abs((bundle(i,j,k)-expected(i,j,k))/expected(i,j,k))
         end do
      end do
      success = success .and. (error/dble(n_points) < epsilon)
      if (success) then
         print '(A,I0,A)',"Receiver: Data for bundle ",k," is ok"
      else
         print '(A,I0,A,E12.5)', "Receiver: Error for bundle ",k," is ",error
      end if
   end do

   if(success) print '(A)', "Receiver: Data received successfully"

end program receiver
