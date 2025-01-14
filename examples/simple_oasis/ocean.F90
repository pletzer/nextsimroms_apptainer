PROGRAM ocean
  !
  use mpi
  use mod_oasis
  

  integer :: ier, local_comm, coupl_comm, npes, mype
  integer :: comp_id, partition_id, paral(3)
  integer :: nlon = 3, nlat = 2

  call MPI_Init(ier)
  local_comm =  MPI_COMM_WORLD
  CALL MPI_Comm_Size (local_comm, npes, ier)
  CALL MPI_Comm_Rank (local_comm, mype, ier)
  print *,'ocean [', mype, '] out of ', npes

  ! coupled = .FALSE. to start with
  call oasis_init_comp(comp_id, 'ocean', ier, .FALSE., local_comm)

  call oasis_get_localcomm(local_comm, ier)

  ! call oasis_create_couplcomm(1, local_comm, coupl_comm, ier)

  paral(1) = 1
  paral(2) = 0
  paral(3) = nlon * nlat
  call oasis_def_partition(partition_id, paral, ier)

  call oasis_enddef(ier)

  call oasis_terminate(ier)
  !
  CALL MPI_Finalize(ier)
  !
END PROGRAM ocean
!
