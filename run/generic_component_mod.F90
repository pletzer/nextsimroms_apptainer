module generic_component_mod

    use mod_oasis
    use exception_mod
    implicit none
    integer, parameter :: STR_LEN = 16
    
    type generic_component_type
        integer(8) :: run_time
        integer(8) :: time_step
        integer :: nx_global
        integer :: ny_global
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
        
        integer :: num_export, num_import, iu
        character(len=STR_LEN), allocatable :: export_field_name(:)
        character(len=STR_LEN), allocatable :: import_field_name(:)
        real(8), allocatable :: export_field_value(:)
        real(8), allocatable :: import_field_value(:)
        integer(8) :: run_time, time_step
        integer :: nx_global, ny_global
    
        namelist /dims/ num_export, num_import, run_time, time_step, nx_global, ny_global

        namelist /values/ &
            & export_field_name, import_field_name, export_field_value, import_field_value

        ier = 0
        run_time = 0
        time_step = 0
        num_import = 0
        num_export = 0
        nx_global = 0
        ny_global = 0

        open(newunit=iu, file=namelist_file, status='old', iostat=ier); 
        if (ier /= 0) call check_err_generic(ier, __FILE__, __LINE__)
        read(unit=iu, nml=dims, iostat=ier); 
        if (ier /= 0) call check_err_generic(ier, __FILE__, __LINE__)

        allocate(export_field_name(num_export), &
            &    export_field_value(num_export), &
            &    self % export_field_id(num_export))
        allocate(import_field_name(num_import), &
            &    import_field_value(num_import), &
            &    self % import_field_id(num_import))
        
        rewind(unit=iu)
        read(unit=iu, nml=values, iostat=ier); 
        if (ier /= 0) call check_err_generic(ier, __FILE__, __LINE__)
        close(iu)
    
        self % run_time = run_time
        self % time_step = time_step
        self % export_field_name = export_field_name
        self % import_field_name = import_field_name
        self % export_field_value = export_field_value
        self % import_field_value = import_field_value
        self % nx_global = nx_global
        self % ny_global = ny_global
            
    end subroutine gc_new

    subroutine gc_print(self, ier)
        implicit none
        type(generic_component_type), intent(inout) :: self
        integer, intent(out) :: ier

        integer :: i

        ier = 0

        print *, 'number of export fields: ', size(self % export_field_name)
        do i = 1, size(self % export_field_name)
            print *, '    export field ', self % export_field_name(i), ' value: ', self % export_field_value(i)
        enddo

        print *, 'number of import fields: ', size(self % import_field_name)
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
    
