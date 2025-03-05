module generic_component_mod

use mod_oasis
implicit none
integer, parameter :: STR_LEN = 16

type generic_component_type
   integer :: ncid
   character(len=STR_LEN) :: grid_name
   integer, allocatable :: export_field_id(:)
   integer, allocatable :: import_field_id(:)

   character(len=STR_LEN), allocatable :: export_field_name(:)
   character(len=STR_LEN), allocatable :: import_field_name(:)
   real(8), allocatable :: export_field_value(:)
   real(8), allocatable :: import_field_value(:)
end type generic_component_type

contains

   subroutine gc_new(self, namelist_file)

      implicit none

      type(generic_component_type), intent(inout) :: self
      character(len=*), intent(in) :: namelist_file

      character(len=STR_LEN) :: grid_name
      integer :: num_export, num_import, iu
      character(len=STR_LEN), allocatable :: export_field_name(:)
      character(len=STR_LEN), allocatable :: import_field_name(:)
      real(8), allocatable :: export_field_value(:)
      real(8), allocatable :: import_field_value(:) 
      namelist /dims/ num_export, num_import

      namelist /values/ grid_name, &
         & export_field_name, import_field_name, export_field_value, import_field_value

      open(newunit=iu, file=namelist_file)
      read(unit=iu, nml=dims)
      allocate(export_field_name(num_export), export_field_value(num_export))
      allocate(import_field_name(num_import), import_field_value(num_import))
      

      rewind(unit=iu)
      read(unit=iu, nml=values)   
      close(iu)
   
      self % grid_name = grid_name
      self % export_field_name = export_field_name
      self % import_field_name = import_field_name
      self % export_field_value = export_field_value
      self % import_field_value = import_field_value
            
   end subroutine gc_new

   subroutine gc_define_fields(self)
      implicit none
      type(generic_component_type), intent(inout) :: self
   end subroutine gc_define_fields

   subroutine gc_export(self)
      implicit none
      type(generic_component_type), intent(inout) :: self
   end subroutine gc_export

   subroutine gc_import(self)
      implicit none
      type(generic_component_type), intent(inout) :: self
   end subroutine gc_import

   subroutine gc_del(self)
      implicit none
      type(generic_component_type), intent(inout) :: self
      deallocate(self % export_field_id)
      deallocate(self % export_field_name)
      deallocate(self % export_field_value)     
      deallocate(self % import_field_id)
      deallocate(self % import_field_name)
      deallocate(self % import_field_value)     
   end subroutine gc_del

end module generic_component_mod


program main
   use mpi
   use generic_component_mod
   implicit none
   type(generic_component_type) :: component


end program main
