module ocn_mod

    implicit none

    type ocn_type
        ! density to be computed by the ocn component
        real(8), allocatable :: rho(:, :)
        real(8), allocatable :: lons(:)
        real(8), allocatable :: lats(:)    
    end type ocn_type

contains

subroutine ocn_initialize(self, namelist_file)
    type(ocn_type), intent(inout) :: self
    character(len=*), intent(in)  :: namelist_file

    integer :: iunit, nx, ny, i, j
    real(8) :: lon_min, lon_max, lat_min, lat_max
    namelist /ocn/ nx, ny, lon_min, lon_max, lat_min, lat_max

    open(newunit=iunit, file=namelist_file)
    read(iunit, nml=ocn)
    close(iunit)

    allocate(self%rho(nx, ny))
    allocate(self%lons(nx + 1))
    allocate(self%lats(ny + 1))
    do j = 1, ny + 1
        self%lats(j) = lat_min + (lat_max - lat_min) * real(j, 8) / real(ny, 8)
    enddo
    do i = 1, nx + 1
        self%lons(i) = lon_min + (lon_max - lon_min) * real(i, 8) / real(nx, 8)
    enddo
    ! set the field's values
    do j = 1, ny
        do i = 1, nx
            self%rho(i, j) = 0
        enddo
    enddo


end subroutine ocn_initialize

subroutine ocn_update(self)
    type(ocn_type), intent(inout) :: self
    ! nothing to do...
end subroutine ocn_update

subroutine ocn_finalize(self)
    type(ocn_type), intent(inout) :: self

    deallocate(self%rho)
    deallocate(self%lats)
    deallocate(self%lons)
end subroutine ocn_finalize

end module ocn_mod

program ocn
    use mpi
    use mod_oasis
    use ocn_mod
    implicit none

    type(ocn_type) :: ocn_comp
    integer :: ier, local_comm, compid, npes, pe

    call mpi_init(ier)
    local_comm = MPI_COMM_WORLD

    ! call oasis_init_comp(compid, 'ocn', ier)
    ! call oasis_get_localcomm(local_comm, ier)

    ! call mpi_comm_size(local_comm, npes, ier)
    ! call mpi_comm_rank(local_comm, pe, ier)
    ! print *,'ocn pe: ', pe, ' out of ', npes

    call ocn_initialize(ocn_comp, 'input/ocn.nml')

    call ocn_finalize(ocn_comp)

    ! call oasis_terminate(ier)
    call mpi_finalize(ier)
end program ocn