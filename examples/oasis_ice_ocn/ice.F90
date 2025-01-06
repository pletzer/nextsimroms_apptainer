module ice_mod

    implicit none

    type ice_type
        ! density increment to be computed by the ice component
        real(8), allocatable :: dRho(:, :)
        real(8), allocatable :: lons(:)
        real(8), allocatable :: lats(:)    
    end type ice_type

contains

subroutine ice_initialize(self, namelist_file)
    type(ice_type), intent(inout) :: self
    character(len=*), intent(in)  :: namelist_file

    integer :: iunit, nx, ny, i, j
    real(8) :: lon_min, lon_max, lat_min, lat_max
    namelist /ice/ nx, ny, lon_min, lon_max, lat_min, lat_max

    open(newunit=iunit, file=namelist_file)
    read(iunit, nml=ice)
    close(iunit)

    ! create the grid
    allocate(self%dRho(nx, ny))
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
            self%dRho(i, j) = 0
        enddo
    enddo

end subroutine ice_initialize

subroutine ice_update(self)
    type(ice_type), intent(inout) :: self
    ! nothing to do...
end subroutine ice_update

subroutine ice_finalize(self)
    type(ice_type), intent(inout) :: self

    deallocate(self%dRho)
    deallocate(self%lats)
    deallocate(self%lons)
end subroutine ice_finalize

end module ice_mod

program ice
    use mpi
    use mod_oasis
    use ice_mod
    implicit none

    type(ice_type) :: ice_comp
    integer :: ier, local_comm, compid, pe, npes

    call mpi_init(ier)
    local_comm = MPI_COMM_WORLD
    call mpi_comm_size(local_comm, npes, ier)
    call mpi_comm_rank(local_comm, pe, ier)
    if (pe == 0) print *, 'ice pe: ', pe, ' out of ', npes

    if (pe == 0) print *, 'calling call oasis_init_comp...'
    call oasis_init_comp(compid, 'ice', ier)
    if (pe == 0) print *, '...done'
    ! call oasis_get_localcomm(local_comm, ier)

    ! call mpi_comm_size(local_comm, npes, ier)
    ! call mpi_comm_rank(local_comm, pe, ier)
    ! print *,'ice pe: ', pe, ' out of ', npes

    call ice_initialize(ice_comp, 'input/ice.nml')

    call ice_finalize(ice_comp)

    call oasis_terminate(ier)
    call mpi_finalize(ier)
end program ice
