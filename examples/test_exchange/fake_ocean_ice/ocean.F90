program ocean
   use mpi
   use mod_oasis
   use grid_mod
   use generic_component_mod
   use exception_mod
   implicit none
   character(len=5), parameter :: comp_name = 'ocean'
   integer :: i, k, kinfo, date
   integer :: comp_id, part_id
   integer :: part_params(OASIS_Apple_Params), offset, local_size
   integer :: local_comm, comm_size, comm_rank
   integer :: var_nodims(2)
   ! use flat arrays 
   real(kind=8), allocatable :: bundle_export(:,:), bundle_import(:, :)

   integer :: nx_global, ny_global
   integer :: n_points     ! total number of points
   real(kind=8), allocatable :: lon(:, :), lat(:, :)
   integer :: ll_i, ll_j   ! local lon, lat indices
   real(kind=8) :: dp_conv ! used in the analytic formula

   type(generic_component_type) :: component
   integer :: n_export, n_import

   call mpi_init(kinfo)

   call oasis_init_comp(comp_id, comp_name, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_init_comp: ", rcode=kinfo)

   call oasis_get_localcomm(local_comm, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_get_localcomm: ", rcode=kinfo)

   call mpi_comm_size(local_comm, comm_size, kinfo)
   call mpi_comm_rank(local_comm, comm_rank, kinfo)   
   print *, comp_name, ": Component ID: ", comp_id

   call read_dims('../common_data/grids.nc', 'bggd', nx_global, ny_global)
   n_points = nx_global*ny_global
   allocate(lon(nx_global,ny_global), lat(nx_global,ny_global))
   call read_coords('../common_data/grids.nc', 'bggd', lon, lat)

   local_size=n_points/comm_size
   offset=comm_rank*local_size
   if (comm_rank == comm_size - 1) &
      & local_size = n_points - offset

   part_params(OASIS_Strategy) = OASIS_Apple
   part_params(OASIS_Offset)   = offset
   part_params(OASIS_Length)   = local_size
   call oasis_def_partition(part_id, part_params, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_def_partition: ", rcode=kinfo)

   call gc_new(component, 'oi_data/ocean.nml', kinfo)
      call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   ! debug 
   call gc_print(component, kinfo)

   n_export = size(component % export_field_value)
   n_import = size(component % import_field_value)

   var_nodims=[1, 1]
   
   if (n_export > 0) then
      do k = 1, n_export
         call oasis_def_var(component % export_field_id(k), component % export_field_name(k), &
            &               part_id, var_nodims, OASIS_OUT, &
            &               OASIS_DOUBLE, kinfo)
         if(kinfo<0 .or. component % export_field_id(k) < 0) &
            & call oasis_abort(comp_id, comp_name, &
            & "Error in oasis_def_var: ", rcode=kinfo)
      enddo
   endif

   if (n_import > 0) then
      do k = 1, n_import
         call oasis_def_var(component % import_field_id(k), component % import_field_name(k), &
            &               part_id, var_nodims, OASIS_IN, &
          &                 OASIS_DOUBLE, kinfo)
         if(kinfo<0 .or. component % import_field_id(k) < 0) &
            & call oasis_abort(comp_id, comp_name, &
            & "Error in oasis_def_var: ", rcode=kinfo)
      enddo
   endif
   
   call oasis_enddef(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_enddef: ", rcode=kinfo)

   allocate(bundle_export(local_size, n_export), bundle_import(local_size, n_import))

   ! set the values of the export bundle
   dp_conv = atan(1.0)/45.0 ! conversion factor
   do k = 1, n_export
      bundle_export(:, k) = component % export_field_value(k)
      ! do i = 1, local_size
      !    ll_j = int((offset+i-1)/nx_global)+1
      !    ll_i = mod(offset+i-1,nx_global)+1
      !    bundle_export(i, k) = k * ( &
      !       & 2.0 + (sin(2.*lat(ll_i,ll_j)*dp_conv))**4 * &
      !       & cos(4.*lon(ll_i,ll_j)*dp_conv) &
      !       & )
      ! enddo
   enddo

   do k = 1, n_import
      bundle_import(:, k) = component % import_field_value(k)
   enddo
   
   date = 0
   do date = 0, component % run_time - 1, component % time_step

      if (n_export > 0) then
         ! export the field
         do k = 1, n_export
            call oasis_put(component % export_field_id(k), date, bundle_export(:, k), kinfo)
            if(kinfo<0) call oasis_abort(comp_id, comp_name, &
               & "Error in oasis_put: ", rcode=kinfo)
         enddo
      endif

      if (n_import > 0) then
         ! import
         do k = 1, n_import
            call oasis_get(component % import_field_id(k), date, bundle_import(:, k), kinfo)
            if(kinfo<0) call oasis_abort(comp_id, comp_name, &
               & "Error in oasis_put: ", rcode=kinfo)
         enddo
      endif

   enddo

   ! clean up
   call gc_del(component, kinfo)
      call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   call oasis_terminate(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_terminate: ", rcode=kinfo)

   call mpi_finalize(kinfo)
      
end program ocean
