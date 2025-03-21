module generic_component_mod

    use mod_oasis
    implicit none
    integer, parameter :: STR_LEN = 16
    
    type generic_component_type
        character(len=STR_LEN) :: grid_name
        integer(8) :: run_time
        integer(8) :: time_step
        integer, allocatable :: export_field_id(:)
        integer, allocatable :: import_field_id(:)
        character(len=STR_LEN), allocatable :: export_field_name(:)
        character(len=STR_LEN), allocatable :: import_field_name(:)
        real(8), allocatable :: export_field_value(:)
        real(8), allocatable :: import_field_value(:)
    end type generic_component_type

    contains

    subroutine gc_new(self, namelist_file, ier)

        implicit none

        type(generic_component_type), intent(inout) :: self
        character(len=*), intent(in) :: namelist_file
        integer, intent(out) :: ier
        
        character(len=STR_LEN) :: grid_name
        integer :: num_export, num_import, iu
        character(len=STR_LEN), allocatable :: export_field_name(:)
        character(len=STR_LEN), allocatable :: import_field_name(:)
        real(8), allocatable :: export_field_value(:)
        real(8), allocatable :: import_field_value(:)
        integer(8) :: run_time, time_step
    
        namelist /dims/ num_export, num_import, run_time, time_step

        namelist /values/ grid_name, &
            & export_field_name, import_field_name, export_field_value, import_field_value

        ier = 0
        run_time = 0
        time_step = 0
        num_import = 0
        num_export = 0

        open(newunit=iu, file=namelist_file, status='old', iostat=ier); if (ier /= 0) return
        read(unit=iu, nml=dims, iostat=ier); if (ier /= 0) return

        allocate(export_field_name(num_export), &
            &    export_field_value(num_export), &
            &    self % export_field_id(num_export))
        allocate(import_field_name(num_import), &
            &    import_field_value(num_import), &
            &    self % import_field_id(num_import))
        
        rewind(unit=iu)
        read(unit=iu, nml=values, iostat=ier); if (ier /= 0) return   
        close(iu)
    
        self % run_time = run_time
        self % time_step = time_step
        self % grid_name = grid_name
        self % export_field_name = export_field_name
        self % import_field_name = import_field_name
        self % export_field_value = export_field_value
        self % import_field_value = import_field_value
            
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

    subroutine gc_del(self, ier)
        implicit none
        type(generic_component_type), intent(inout) :: self
        integer, intent(out) :: ier
        integer :: status
        ier = 0
        ! do not generate an error if deallocating an array that is not allocated
        deallocate(self % export_field_name, stat=status)
        deallocate(self % export_field_value, stat=status)
        deallocate(self % export_field_id, stat=status)
        deallocate(self % import_field_name, stat=status)
        deallocate(self % import_field_value, stat=status)
        deallocate(self % import_field_id, stat=status)  
    end subroutine gc_del

end module generic_component_mod
    