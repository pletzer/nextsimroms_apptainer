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
   real(kind=8) :: total
   real(kind=8), allocatable ::  bundle_export(:, :, :), bundle_import(:, :, :)
   integer :: ncid, varid
   real(kind=8), allocatable :: lon(:, :), lat(:, :)
   integer, allocatable :: imsk(:, :)
   real(kind=8) :: dp_conv

   integer :: nx_global, ny_global
   integer :: n_points

   type(generic_component_type) :: component
   integer :: n_export, n_import

   call mpi_init(kinfo)

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

   call gc_new(component, 'oi_data/ice.nml', kinfo)
   call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   nx_global = component % nx_global
   ny_global = component % ny_global
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


   ! debug 
   call gc_print(component, kinfo)

   n_export = size(component % export_field_value)
   n_import = size(component % import_field_value)

   var_nodims=[1, 1]

   ! Only rank 0 contributes to the coupling

   if (n_export > 0 .and. comm_rank == 0) then
      do k = 1, n_export
         call oasis_def_var(component % export_field_id(k), component % export_field_name(k), &
            &  part_id, var_nodims, OASIS_OUT, &
            &  OASIS_DOUBLE, kinfo)
         if(kinfo<0 .or. component % export_field_id(k) < 0) &
            &  call oasis_abort(comp_id, comp_name, &
            & "Error in oasis_def_var: ", rcode=kinfo)
      enddo
   endif

   if (n_import > 0 .and. comm_rank == 0) then
      do k = 1, n_import
         call oasis_def_var(component % import_field_id(k), component % import_field_name(k), &
                  &  part_id, var_nodims, OASIS_IN, &
                  &  OASIS_DOUBLE, kinfo)
         if(kinfo<0 .or. component % import_field_id(k) < 0) &
                  &  call oasis_abort(comp_id, comp_name, &
                  & "Error in oasis_def_var: ", rcode=kinfo)
      enddo
   endif

   call oasis_enddef(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_enddef: ", rcode=kinfo)


   if (comm_rank == 0) then

      allocate(bundle_import(nx_global, ny_global, n_import), &
             & bundle_export(nx_global, ny_global, n_export))
         
      date = 0
      do date = 0, component % run_time, component % time_step

         ! must import first as ocean sends first 
         if (n_import > 0) then
            do k = 1, n_import
               ! import the field
               call oasis_get(component % import_field_id(k), date, bundle_import(:, :, k), kinfo)
               if(kinfo<0) call oasis_abort(comp_id, comp_name, &
                  & "Error in oasis_get: ", rcode=kinfo)
            enddo
         endif

         if (n_export > 0) then
            do k = 1, n_export
               ! export the field
               call oasis_put(component % export_field_id(k), date, bundle_export(:, :, k), kinfo)
               if(kinfo<0) call oasis_abort(comp_id, comp_name, &
                  & "Error in oasis_get: ", rcode=kinfo)
            enddo
         endif

      enddo

      do k = 1, n_import
         call vtk_write_data(lon, lat, bundle_import(:, :, k), &
            & trim(component % import_field_name(k)), &
            & trim(component % import_field_name(k)) // '.vtk')
      enddo

      ! compute the average values of the imported fields
      do k = 1, n_import
         total = 0.
         do j = 1, ny_global
            do i = 1, nx_global
               ! imsk = 0 means valid
               if (imsk(i,j) == 0) &
                  & total = total + bundle_import(i,j,k)
            end do
         end do
         print *, comp_name, ": Average imported field value for ", &
           &         component % import_field_name(k),  " is ", total/dble(n_points)
      end do
   
      endif

   call oasis_terminate(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_terminate: ", rcode=kinfo)
      
   call mpi_finalize(kinfo)

end program ice
