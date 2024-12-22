module ice_mod

    implicit none

    type ice_type
        ! density increment to be computed by the ice component
        real(8), allocatable :: dRho(:, :)
    end type ice_type

contains

subroutine ice_initialize(self, namelist_file)
    type(ice_type), intent(inout) :: self
    character(len=*), intent(in)  :: namelist_file

    integer :: iunit, nx, ny
    namelist /ice/ nx, ny

    open(newunit=iunit, file=namelist_file)
    read(iunit, nml=ice)
    close(iunit)

    allocate(self%dRho(nx, ny))
    self%dRho = 0

end subroutine ice_initialize

subroutine ice_update(self)
    type(ice_type), intent(inout) :: self
    ! nothing to do...
end subroutine ice_update

subroutine ice_finalize(self)
    type(ice_type), intent(inout) :: self

    deallocate(self%dRho)
end subroutine ice_finalize

end module ice_mod

program ice
    use mpi
    use ice_mod
    implicit none

    type(ice_type) :: ice_comp
    integer :: ier, local_comm

    call mpi_init(ier)
    local_comm = MPI_COMM_WORLD

    call ice_initialize(ice_comp, 'input/ice.nml')

    call ice_finalize(ice_comp)

    call mpi_finalize(ier)
end program ice