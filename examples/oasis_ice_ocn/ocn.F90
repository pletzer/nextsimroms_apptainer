module ocn_mod

    implicit none

    type ocn_type
        ! density to be computed by the ocn component
        real(8), allocatable :: rho(:, :)
    end type ocn_type

contains

subroutine ocn_initialize(self, namelist_file)
    type(ocn_type), intent(inout) :: self
    character(len=*), intent(in)  :: namelist_file

    integer :: iunit, nx, ny
    namelist /ocn/ nx, ny

    open(newunit=iunit, file=namelist_file)
    read(iunit, nml=ocn)
    close(iunit)

    allocate(self%rho(nx, ny))
    self%rho = 0

end subroutine ocn_initialize

subroutine ocn_update(self)
    type(ocn_type), intent(inout) :: self
    ! nothing to do...
end subroutine ocn_update

subroutine ocn_finalize(self)
    type(ocn_type), intent(inout) :: self

    deallocate(self%rho)
end subroutine ocn_finalize

end module ocn_mod

program ocn
    use mpi
    use ocn_mod
    implicit none

    type(ocn_type) :: ocn_comp
    integer :: ier, local_comm

    call mpi_init(ier)
    local_comm = MPI_COMM_WORLD

    call ocn_initialize(ocn_comp, 'input/ocn.nml')

    call ocn_finalize(ocn_comp)

    call mpi_finalize(ier)
end program ocn