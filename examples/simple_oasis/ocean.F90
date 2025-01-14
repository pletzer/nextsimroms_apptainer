PROGRAM ocean
  !
  use mpi
  use mod_oasis
  

  integer :: ier, local_comm, npes, mype
  integer :: comp_id
  integer :: nlon, nlat, il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset

  call MPI_Init(ier)
  local_comm =  MPI_COMM_WORLD
  CALL MPI_Comm_Size (local_comm, npes, ier)
  CALL MPI_Comm_Rank (local_comm, mype, ier)
  print *,'ocean [', mype, '] out of ', npes
  !
  !
  CALL MPI_Finalize(ier)
  !
END PROGRAM ocean
!
