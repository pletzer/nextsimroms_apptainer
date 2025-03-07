program main

   use mpi
   use generic_component_mod
   use exception_mod
   use grid_mod

   implicit none

   type(generic_component_type) :: component
   integer :: num_args, ier, i, n_export, n_import
   character(len=STR_LEN) :: namelist_file
   integer :: comp_id, part_id, var_id, kinfo
   integer :: local_comm, comm_size, comm_rank
   character(len=STR_LEN) :: comp_name, var_name
   integer :: var_nodims(2)

   ! grid
   integer :: nx_global, ny_global, n_points, local_size, offset
   integer :: part_params(OASIS_Apple_Params)
   integer :: date

   ! get the namelist file name
   num_args = command_argument_count()
   if (num_args /= 1) stop'ERROR must provide namelist file'
   call get_command_argument(1, namelist_file)
         
   ! just to get the component name
   call gc_new(component, trim(namelist_file), ier)
   call gc_print(component, ier)
   comp_name = trim(component % component_name)
   n_export = size(component % export_field_name)
   n_import = size(component % import_field_name)
   if (n_export /= size(component % export_field_value)) stop'ERROR number of export names/values does not match'
   if (n_export /= size(component % export_field_value)) stop'ERROR number of import names/values does not match'
   call gc_del(component, ier)

   ! start MPI
   call oasis_init_comp(comp_id, comp_name, kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)
   call oasis_get_localcomm(local_comm, kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   call mpi_comm_size(local_comm, comm_size, kinfo)
   call mpi_comm_rank(local_comm, comm_rank, kinfo)   

   call gc_new(component, trim(namelist_file), ier)

   ! get the grid data
   call read_dims('grids.nc', trim(component % grid_name), nx_global, ny_global)
   n_points = nx_global*ny_global

   ! domain decomposition (1D)
   local_size = n_points/comm_size
   offset=comm_rank*local_size
   if (comm_rank == comm_size - 1) local_size = n_points - offset
   
   part_params(OASIS_Strategy) = OASIS_Apple
   part_params(OASIS_Offset)   = offset
   part_params(OASIS_Length)   = local_size
   call oasis_def_partition(part_id, part_params, kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   ! set the export fields
   if (n_export > 0) then
      allocate(component % export_bundle_data(local_size, n_export))
      do i = 1, n_export
         component % export_bundle_data(:, i) = component % export_field_value(i)
      enddo
   endif
         
   ! initialize the import fields
   if (n_import > 0) then
      allocate(component % import_bundle_data(local_size, n_import))
      do i = 1, n_import
         component % import_bundle_data(:, i) = component % import_field_value(i)
      enddo
   endif

   ! define export bundle
   if (n_export > 0) then
      var_nodims = [1, n_export] ! cannot be passed directly to oasis_def_var. 1st number is not used. 2nd number is the bundle size 
      var_name = trim(component % export_bundle_name)
      call oasis_def_var(component % export_bundle_id, &
            & var_name, part_id, var_nodims, OASIS_OUT, OASIS_DOUBLE, &
            & kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)
   endif

   ! define import bundle
   if (n_import > 0) then
      var_nodims = [1, n_import] ! cannot be passed directly to oasis_def_var. 1st number is not used. 2nd number is the bundle size 
      var_name = trim(component % import_bundle_name)
      call oasis_def_var(component % import_bundle_id, &
            & var_name, part_id, var_nodims, OASIS_IN, OASIS_DOUBLE, &
            & kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)
   endif

   ! done with definition
   call oasis_enddef(kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   ! for the time being, a one off exchange
   date = 0 ! number of seconds

   ! export
   if (n_export > 0) then
      write(0, *) '$$$$$ ', comp_name, ' exports ', trim(component % export_bundle_name), &
         ' dims: ', size(component % export_bundle_data, 1), size(component % export_bundle_data, 2)
      call oasis_put(component % export_bundle_id, &
         & date, component % export_bundle_data, &
         & kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)
      print *, comp_name, ' export was successful'
   endif

   ! import
   if (n_import > 0) then
      write(0, *) '$$$$$ ', comp_name, ' imports ', trim(component % import_bundle_name), &
         ' dims: ', size(component % import_bundle_data, 1), size(component % import_bundle_data, 2)
      call oasis_get(component % import_bundle_id, &
         & date, component % import_bundle_data, &
         & kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)
      print *, comp_name, ' import was successful'
   endif
      
   ! clean up
   call gc_del(component, ier)
   
   call oasis_terminate(kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

end program main
