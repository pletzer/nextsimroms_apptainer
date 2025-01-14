PROGRAM ocean
  !
  use mpi
  use mod_oasis

  IMPLICIT NONE

  integer :: ier, local_comm, npes, mype

  call MPI_Init(ier)
  local_comm = MPI_COMM_WORLD
  CALL MPI_Comm_Size (local_comm, npes, ier)
  CALL MPI_Comm_Rank (local_comm, mype, ier)
  print *,'atmos [', mype, '] out of ', npes
  !




  CALL MPI_Finalize(ier)
  !
END PROGRAM ocean
!
