program ocean
   use mpi
   use mod_oasis
   use generic_component_mod
   use exception_mod
   use tovtk_mod
   implicit none
   character(len=5), parameter :: comp_name = 'ocean'
   character(len=16) :: str_n
   integer :: i, k, kinfo, date
   integer :: comp_id, part_id, export_id, import_id
   integer :: part_params(OASIS_Serial_Params), offset, local_size
   integer :: local_comm, comm_size, comm_rank
   integer :: var_nodims(2)

   integer :: nx, ny, nz
   integer :: n_points     ! total number of points

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

   call gc_new(component, 'oi_data/ocean.nml', kinfo)
   call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   nx = size(component % temperature, 1)
   ny = size(component % temperature, 2)
   nz = size(component % temperature, 3)
   ! number of points in the horizontal plane
   n_points = nx*ny

   ! Domain decomposition

   ! local_size=n_points/comm_size
   ! offset=comm_rank*local_size
   ! if (comm_rank == comm_size - 1) &
   !    & local_size = n_points - offset

   ! part_params(OASIS_Strategy) = OASIS_Apple
   ! part_params(OASIS_Offset)   = offset
   ! part_params(OASIS_Length)   = local_size

   ! no decomp for the time being
   part_params(OASIS_Strategy) = OASIS_Serial
   part_params(OASIS_Length)   = n_points
   
   call oasis_def_partition(part_id, part_params, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_def_partition: ", rcode=kinfo)

   var_nodims=[1, 1]
   
   call oasis_def_var(export_id, 'O_SST_FROM_OCN', &
      &               part_id, var_nodims, OASIS_OUT, &
      &               OASIS_DOUBLE, kinfo)
   if(kinfo<0 .or. export_id < 0) &
      & call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_def_var: ", rcode=kinfo)

   call oasis_def_var(import_id, 'O_SST_FROM_ICE', &
      &               part_id, var_nodims, OASIS_IN, &
      &                 OASIS_DOUBLE, kinfo)
   if(kinfo<0 .or. import_id < 0) &
      & call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_def_var: ", rcode=kinfo)
   
   call oasis_enddef(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_enddef: ", rcode=kinfo)

   ! initialize the temperature of this component
   do k = 1, size(component % temperature, 3)
      component % temperature(:, :, k) = 0
   enddo
   ! perturbation
   component % temperature(floor(real(nx, 8)/2.) + 1, floor(real(ny, 8)/3) + 1, nz) = 1
   component % top_temperature = component % temperature(:, :, nz)
   component % bottom_temperature = component % temperature(:, :, 1)

   call zero_fill(0, 6, str_n)
   call vtk_write_data(component % temperature, 'field', 'ocean'//trim(str_n)//'.vtk')
     
   ! data is the number of seconds into the simulation 
   date = 0
   do date = 0, component % num_steps

      ! Ocean exports first, advances and then imports. Order is important to avoid deadlocks

      ! set the top temperature
      component % top_temperature = component % temperature(:, :, size(component % temperature, 3))

      ! export the top temperature to ice
      call oasis_put(export_id, date, component % top_temperature, kinfo)
      if(kinfo<0) call oasis_abort(comp_id, comp_name, &
            & "Error in oasis_put: ", rcode=kinfo)

      ! advance by one time step
      call gc_step(component, kinfo)
      call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

      call zero_fill(date, 6, str_n)
         call vtk_write_data(component % temperature, 'field', 'ocean'//trim(str_n)//'.vtk')

      ! import the temperature from ice
      call oasis_get(import_id, date, component % top_temperature, kinfo)
      if(kinfo<0) call oasis_abort(comp_id, comp_name, &
            & "Error in oasis_put: ", rcode=kinfo)

      print *,'ocean at step ', date, ' : chksum recv data ', sum(component % top_temperature)

   enddo

   ! clean up
   call gc_del(component, kinfo)
      call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   call oasis_terminate(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_terminate: ", rcode=kinfo)

   call mpi_finalize(kinfo)
      
end program ocean
