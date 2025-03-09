program ice
   use mpi
   use mod_oasis
   use grid_mod
   use netcdf
   use generic_component_mod
   use exception_mod
   use tovtk_mod
   implicit none
   character(len=3), parameter :: comp_name = 'ice'
   integer :: i, j, k, kinfo, date
   integer :: comp_id, part_id
   integer :: part_params(OASIS_Serial_Params)
   integer :: local_comm, comm_size, comm_rank
   integer :: var_nodims(2)
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

   type(generic_component_type) :: component
   integer :: n_export, n_import

   call oasis_init_comp(comp_id, comp_name, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_init_comp: ", rcode=kinfo)
   print '(A,I0)', "ice: Component ID: ", comp_id

   call oasis_get_localcomm(local_comm, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_get_localcomm: ", rcode=kinfo)
   call mpi_comm_size(local_comm, comm_size, kinfo)
   call mpi_comm_rank(local_comm, comm_rank, kinfo)
   print *, comp_name, ": Component ID: ", comp_id

   call read_dims('grids.nc', 'bggd', nx_global, ny_global)
   n_points = nx_global*ny_global
   write(0, *) 'ICE DEBUG nx_global, ny_global = ', nx_global, ny_global
   allocate(lon(nx_global,ny_global), lat(nx_global,ny_global))
   allocate(imsk(nx_global,ny_global))
   call read_coords('grids.nc', 'bggd', lon, lat)

   kinfo = nf90_open('masks.nc',NF90_NOWRITE,ncid)
   kinfo = nf90_inq_varid(ncid,'bggd.msk',varid)
   kinfo = nf90_get_var(ncid,varid,imsk)
   kinfo = nf90_close(ncid)

   part_params(OASIS_Strategy) = OASIS_Serial
   part_params(OASIS_Length)   = n_points
   
   if (comm_rank == 0) then
      call oasis_def_partition(part_id, part_params, kinfo)
      if(kinfo<0) call oasis_abort(comp_id, comp_name, &
         & "Error in oasis_def_partition: ", rcode=kinfo)
   endif

   call gc_new(component, 'oi_data/ice.nml', kinfo)
   call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   ! debug 
   call gc_print(component, kinfo)

   n_export = size(component % export_field_value)
   n_import = size(component % import_field_value)
      
   ! Only rank 0 contributes to the coupling

   if (n_export > 0 .and. comm_rank == 0) then
      var_nodims=[1, n_export]
      call oasis_def_var(component % export_bundle_id, component % export_bundle_name, &
                  &  part_id, var_nodims, OASIS_OUT, &
                  &  OASIS_DOUBLE, kinfo)
      if(kinfo<0 .or. component % export_bundle_id < 0) &
         &  call oasis_abort(comp_id, comp_name, &
         & "Error in oasis_def_var: ", rcode=kinfo)
   endif

   if (n_import > 0 .and. comm_rank == 0) then
      var_nodims=[1, n_import]
      call oasis_def_var(component % import_bundle_id, component % import_bundle_name, &
                  &  part_id, var_nodims, OASIS_IN, &
                  &  OASIS_DOUBLE, kinfo)
      if(kinfo<0 .or. component % import_bundle_id < 0) &
         &  call oasis_abort(comp_id, comp_name, &
         & "Error in oasis_def_var: ", rcode=kinfo)
   endif

   call oasis_enddef(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_enddef: ", rcode=kinfo)


   if (n_import > 0 .and. comm_rank == 0) then

      allocate(bundle_import(nx_global, ny_global, n_import), &
         & bundle_export(nx_global, ny_global, n_export))
      allocate(expected(nx_global, ny_global, n_import))
         
      date = 0
      do date = 0, component % run_time -1, component % time_step
         ! import the field
         call oasis_get(component % import_bundle_id, date, bundle_import, kinfo)
         if(kinfo<0) call oasis_abort(comp_id, comp_name, &
            & "Error in oasis_get: ", rcode=kinfo)
      enddo

      do k = 1, n_import
         call vtk_write_data(lon, lat, bundle_import(:, :, k), &
            & trim(component % import_field_name(k)), &
            & trim(component % import_field_name(k)) // '.vtk')
      enddo

      ! exact field
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
               ! imsk = 0 means valid
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

      if(success) then
         print '(A)', "ice: Data received successfully"
      else
         print *, 'ice: FAILURE!!!'
      endif
   
   endif

   call oasis_terminate(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_terminate: ", rcode=kinfo)
      
end program ice
