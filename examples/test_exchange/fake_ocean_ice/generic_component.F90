module generic_component_mod

use mod_oasis
implicit none
integer, parameter :: STR_LEN = 16

type generic_component_type
   integer :: ncid
   character(len=STR_LEN) :: component_name
   character(len=STR_LEN) :: grid_name
   integer, allocatable :: export_field_id(:)
   integer, allocatable :: import_field_id(:)

   character(len=STR_LEN), allocatable :: export_field_name(:)
   character(len=STR_LEN), allocatable :: import_field_name(:)
   real(8), allocatable :: export_field_value(:)
   real(8), allocatable :: import_field_value(:)
   ! dimension: flat index, field index
   real(8), allocatable :: export_field_data(:, :)
   real(8), allocatable :: import_field_data(:, :)

end type generic_component_type

contains

   subroutine gc_new(self, namelist_file, ier)

      implicit none

      type(generic_component_type), intent(inout) :: self
      character(len=*), intent(in) :: namelist_file
      integer, intent(out) :: ier
      
      character(len=STR_LEN) :: grid_name, component_name
      integer :: num_export, num_import, iu
      character(len=STR_LEN), allocatable :: export_field_name(:)
      character(len=STR_LEN), allocatable :: import_field_name(:)
      real(8), allocatable :: export_field_value(:)
      real(8), allocatable :: import_field_value(:)
   
      namelist /dims/ num_export, num_import

      namelist /values/ component_name, grid_name, &
         & export_field_name, import_field_name, export_field_value, import_field_value

      ier = 0

      open(newunit=iu, file=namelist_file, status='old', iostat=ier); if (ier /= 0) return
      read(unit=iu, nml=dims, iostat=ier); if (ier /= 0) return

      allocate(export_field_name(num_export), export_field_value(num_export))
      allocate(import_field_name(num_import), import_field_value(num_import))
      
      rewind(unit=iu)
      read(unit=iu, nml=values, iostat=ier); if (ier /= 0) return   
      close(iu)
   
      self % component_name = component_name
      self % grid_name = grid_name
      self % export_field_name = export_field_name
      self % import_field_name = import_field_name
      self % export_field_value = export_field_value
      self % import_field_value = import_field_value
      allocate(self % export_field_id(num_export))
      allocate(self % import_field_id(num_import))
            
   end subroutine gc_new

   subroutine gc_print(self, ier)
      implicit none
      type(generic_component_type), intent(inout) :: self
      integer, intent(out) :: ier

      integer :: i

      ier = 0

      print *, 'component name: ', self % grid_name
      print *, 'grid name: ', self % grid_name

      print *, 'number of export field: ', size(self % export_field_name)
      do i = 1, size(self % export_field_name)
         print *, '    export field ', self % export_field_name(i), ' value: ', self % export_field_value(i)
      enddo

      print *, 'number of import field: ', size(self % import_field_name)
      do i = 1, size(self % import_field_name)
         print *, '    import field ', self % import_field_name(i), ' value: ', self % import_field_value(i)
      enddo
   end subroutine gc_print

   subroutine gc_define_fields(self, ier)
      implicit none
      type(generic_component_type), intent(inout) :: self
      integer, intent(out) :: ier
      ier = 0
   end subroutine gc_define_fields

   subroutine gc_export(self, ier)
      implicit none
      type(generic_component_type), intent(inout) :: self
      integer, intent(out) :: ier
         ier = 0
      end subroutine gc_export

   subroutine gc_import(self, ier)
      implicit none
      type(generic_component_type), intent(inout) :: self
      integer, intent(out) :: ier
      ier = 0
   end subroutine gc_import

   subroutine gc_del(self, ier)
      implicit none
      type(generic_component_type), intent(inout) :: self
      integer, intent(out) :: ier
      ier = 0
      deallocate(self % export_field_id, stat=ier)
      deallocate(self % export_field_name, stat=ier)
      deallocate(self % export_field_value, stat=ier)
      deallocate(self % export_field_data, stat=ier)   
      deallocate(self % import_field_id, stat=ier)
      deallocate(self % import_field_name, stat=ier)
      deallocate(self % import_field_value, stat=ier)     
      deallocate(self % import_field_data, stat=ier)   
   end subroutine gc_del

end module generic_component_mod

module exception_mod
   contains
   subroutine check_err(kinfo, comp_id, comp_name, filename, line)
      use mod_oasis
      implicit none
      integer, intent(in) :: kinfo, comp_id
      character(len=*), intent(in) :: comp_name
      character(len=*), intent(in) :: filename
      integer, intent(in) :: line
      if (kinfo /= 0) then
         call oasis_abort(comp_id, comp_name, &
         & "OASIS error: ", rcode=kinfo)
      endif
   end subroutine check_err
end module exception_mod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program main

   use mpi
   use generic_component_mod
   use exception_mod
   use grid_mod

   implicit none

   type(generic_component_type) :: component
   integer :: num_args, ier, i, n
   character(len=STR_LEN) :: namelist_file
   integer :: comp_id, part_id, var_id, kinfo
   integer :: local_comm, comm_size, comm_rank
   character(len=STR_LEN) :: comp_name, var_name
   integer :: var_nodims(2)

   ! grid
   integer :: nx_global, ny_global, n_points, local_size, offset
   integer :: part_params(OASIS_Apple_Params)
   real(8), allocatable :: lon(:, :), lat(:, :)
   integer :: date

   ! get the namelist file name
   num_args = command_argument_count()
   if (num_args /= 1) stop'ERROR must provide namelist file'
   call get_command_argument(1, namelist_file)
         
   ! just to get the component name
   call gc_new(component, namelist_file, ier)
   call gc_print(component, ier)
   comp_name = component % component_name  
   call gc_del(component, ier)

   ! start MPI
   call oasis_init_comp(comp_id, comp_name, kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)
   call oasis_get_localcomm(local_comm, kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   call mpi_comm_size(local_comm, comm_size, kinfo)
   call mpi_comm_rank(local_comm, comm_rank, kinfo)   

   call gc_new(component, namelist_file, ier)

   ! get the grid data
   call read_dims('grids.nc', component % grid_name, nx_global, ny_global)
   n_points = nx_global*ny_global
   allocate(lon(nx_global,ny_global), lat(nx_global, ny_global))

   call read_coords('grids.nc', component % grid_name, lon, lat)

   ! domain decomposition (1D)
   local_size = n_points/comm_size
   offset=comm_rank*local_size
   if (comm_rank == comm_size - 1) local_size = n_points - offset
   
   part_params(OASIS_Strategy) = OASIS_Apple
   part_params(OASIS_Offset)   = offset
   part_params(OASIS_Length)   = local_size
   call oasis_def_partition(part_id, part_params, kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   var_nodims = [1, 1] ! cannot be passed directly to oasis_def_var. 1st number is not used. 2nd number is the bundle size
   ! set the export fields
   n =  size(component % export_field_id)
   allocate(component % export_field_data(local_size, n))
   do i = 1, n
      component % export_field_data(:, i) = component % export_field_value(i)
   enddo
         
   ! initialize the import fields
   n =  size(component % import_field_id)
   allocate(component % import_field_data(local_size, n))
   do i = 1, n
      component % import_field_data(:, i) = component % import_field_value(i)
   enddo
      
   ! define export fields
   n = size(component % export_field_id)
   do i = 1, n
      var_name = trim(component % export_field_name(i))
      call oasis_def_var(component % export_field_id(i), &
         & var_name, part_id, var_nodims, OASIS_OUT, OASIS_DOUBLE, &
         & kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)
   enddo

   ! define import fields
   n = size(component % import_field_id)
   do i = 1, n
      var_name = trim(component % import_field_name(i))
      call oasis_def_var(component % import_field_id(i), &
         & var_name, part_id, var_nodims, OASIS_IN, OASIS_DOUBLE, &
         & kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)
   enddo

   ! done with definition
   call oasis_enddef(kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

   ! for the time being, a one off exchange
   date = 0 ! number of seconds

   ! MAY NEED TO ENSURE THAT COMP1 exports first (?)

   ! export
   n = size(component % export_field_id)
   do i = 1, n
      print *, component % component_name, ' exports ', component % export_field_name(i)
      call oasis_put(component % export_field_id(i), date, component % export_field_data(:, i), &
         & kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)
      print *, component % component_name, ' success'
   enddo

   ! import
   n = size(component % import_field_id)
   do i = 1, n
      print *, component % component_name, ' imports ', component % import_field_name(i)
      call oasis_get(component % import_field_id(i), date, component % import_field_data(:, i), &
         & kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)
      print *, component % component_name, ' success'
   enddo
      
   ! clean up
   call gc_del(component, ier)
   
   call oasis_terminate(kinfo); call check_err(kinfo, comp_id, comp_name, __FILE__, __LINE__)

end program main
