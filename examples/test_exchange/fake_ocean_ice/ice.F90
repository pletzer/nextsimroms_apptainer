program ice
   use mpi
   use mod_oasis
   use grid_mod
   use netcdf
   implicit none
   integer :: i, j, k, kinfo, date
   integer :: comp_id, part_id
   integer :: part_params(OASIS_Serial_Params)
   integer :: var_nodims(2)
   character(len=3) :: comp_name = "ice"
   integer :: i_from_ocn_id, i_from_ice_id
   character(len=10) :: i_from_ocn = "I_FROM_OCN"
   character(len=10) :: i_from_ice = "I_FROM_ICE"
   real(kind=8) :: error, epsilon
   integer :: nx_global, ny_global
   real(kind=8), allocatable ::  bundle_export(:, :, :), bundle_import(:, :, :)
   real(kind=8), allocatable ::  expected(:, :, :)
   integer :: n_points
   integer :: ncid, varid
   real(kind=8), allocatable :: lon(:, :), lat(:, :)
   integer, allocatable :: imsk(:, :)
   real(kind=8) :: dp_conv
   logical :: success
   integer :: n_export, n_import

   call oasis_init_comp(comp_id, comp_name, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_init_comp: ", rcode=kinfo)
   print '(A,I0)', "ice: Component ID: ", comp_id

   call read_dims('grids.nc', 'bggd', nx_global, ny_global)
   n_points = nx_global*ny_global
   write(0, *) 'ICE DEBUG nx_global, ny_global = ', nx_global, ny_global
   allocate(bundle_import(nx_global, ny_global, 2), bundle_export(nx_global, ny_global, 2))
   allocate(expected(nx_global, ny_global, 2))
   allocate(lon(nx_global,ny_global), lat(nx_global,ny_global))
   allocate(imsk(nx_global,ny_global))
   call read_coords('grids.nc', 'bggd', lon, lat)

   kinfo = nf90_open('masks.nc',NF90_NOWRITE,ncid)
   kinfo = nf90_inq_varid(ncid,'bggd.msk',varid)
   kinfo = nf90_get_var(ncid,varid,imsk)
   kinfo = nf90_close(ncid)

   part_params(OASIS_Strategy) = OASIS_Serial
   part_params(OASIS_Length)   = n_points
   call oasis_def_partition(part_id, part_params, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_def_partition: ", rcode=kinfo)

   n_import = 2
   var_nodims=[1, n_import]

   call oasis_def_var(i_from_ocn_id, i_from_ocn, part_id, var_nodims, OASIS_IN, &
      &              OASIS_DOUBLE, kinfo)
   if(kinfo<0 .or. i_from_ocn_id<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_def_var: ", rcode=kinfo)

   ! call oasis_def_var(i_from_ice_id, i_from_ice, part_id, var_nodims, OASIS_OUT, &
   !    &              OASIS_DOUBLE, kinfo)
   ! if(kinfo<0 .or. i_from_ice_id<0) call oasis_abort(comp_id, comp_name, &
   !    & "Error in oasis_def_var: ", rcode=kinfo)

      call oasis_enddef(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_enddef: ", rcode=kinfo)

   date=0

   bundle_import(:,:,:)=0

   call oasis_get(i_from_ocn_id, date, bundle_import, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_get: ", rcode=kinfo)

   call oasis_terminate(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_terminate: ", rcode=kinfo)

   dp_conv = atan(1.)/45.0
   do k = 1, n_import
      do j = 1, ny_global
         do i = 1, nx_global
            expected(i,j,k) = k * ( &
               & 2.0 + (sin(2.*lat(i,j)*dp_conv))**4 * &
               & cos(4.*lon(i,j)*dp_conv) &
               & )
         enddo
      enddo
   enddo

   epsilon=1.e-3
   success = .true.
   do k = 1, n_import
      error=0.
      do j = 1, ny_global
         do i = 1, nx_global
            if (imsk(i,j) == 0) &
               & error = error + abs((bundle_import(i,j,k)-expected(i,j,k))/expected(i,j,k))
         end do
      end do
      success = success .and. (error/dble(n_points) < epsilon)
      print '(A,A, E20.10)', comp_name, ": Average regridding error: ", error/dble(n_points)
      if (success) then
         print '(A,A,I0,A)', comp_name, ": Data for bundle_import ",k," is ok"
      else
         print '(A,A,I0,A,E12.5)', comp_name, ": Error for bundle_import ",k," is ",error
      end if
   end do

   if(success) print '(A)', "ice: Data received successfully"

end program ice
