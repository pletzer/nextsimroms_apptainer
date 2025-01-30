PROGRAM ocean
  !
  use mpi
  use mod_oasis

  IMPLICIT NONE

  integer :: ier, local_comm, coupl_comm, npes, mype
  integer :: comp_id, partition_id, paral(3), flag
  integer :: var_id(1) ! number of coupling fields
  integer :: var_nodims(2) 
  integer :: var_actual_shape(2)
  integer, parameter :: nlon = 2, nlat = 3
  integer :: i, j
  real(8) :: lon_min = 0, lon_max = 10, lat_min = -80, lat_max = -70, dlon, dlat
  real(8) :: lons(nlon, nlat), lats(nlon, nlat)
  
  character(len=32) :: comp_name = 'ocean'

  call MPI_Init(ier)
  local_comm = MPI_COMM_WORLD
  CALL MPI_Comm_Size (local_comm, npes, ier)
  CALL MPI_Comm_Rank (local_comm, mype, ier)
  print *, comp_name, ' [', mype, '] out of ', npes
  !
  ! coupled = .FALSE. to start with
  ! call oasis_init_comp(comp_id, comp_name, ier, .FALSE., local_comm)
  call oasis_init_comp(comp_id, comp_name, ier, .TRUE., local_comm)

  call oasis_get_localcomm(local_comm, ier)

  ! all ranks are involved in the coupling (1)
  call oasis_create_couplcomm(1, local_comm, coupl_comm, ier)

  ! single partition
  paral(1) = 1
  paral(2) = 0
  paral(3) = nlon * nlat
  call oasis_def_partition(partition_id, paral, ier)

  ! grid
  dlon = (lon_max - lon_min) / real(nlon - 1, 8)
  dlat = (lat_max - lat_min) / real(nlat - 1, 8)
  do j = 1, nlat
    do i = 1, nlon
      lons(i, j) = lon_min + dlon*i
      lats(i, j) = lat_min + dlat*j
    enddo
  enddo

  call oasis_start_grids_writing(flag)
  call oasis_write_grid(comp_name, nlon, nlat, lons, lats, partition_id)
  call oasis_terminate_grids_writing()

  ! variables
  var_nodims(1) = 2 ! 2d array
  var_nodims(2) = 1 ! not used
  var_actual_shape(1) = 0 ! not used anymore
  var_actual_shape(2) = 0 ! not used anymore
  call oasis_def_var(var_id(1), 'FIELD_SEND_OCEAN', partition_id, var_nodims, OASIS_Out, &
                   & var_actual_shape, OASIS_Real, ier)

  call oasis_enddef(ier)

  call oasis_terminate(ier)

  CALL MPI_Finalize(ier)
  !
END PROGRAM ocean
!
