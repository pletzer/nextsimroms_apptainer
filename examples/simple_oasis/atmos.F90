PROGRAM ocean
  !
  ! Use for netCDF library
  USE netcdf
  use mpi
  IMPLICIT NONE

  integer :: ierror, local_comm, npes, mype

  call MPI_Init(ierror)
  !
  local_comm =  MPI_COMM_WORLD
  !
  !!!!!!!!!!!!!!!!! OASIS_INIT_COMP !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !!!!!!!!!!!!!!!!! OASIS_GET_LOCALCOMM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Get rank in local communicator
  CALL MPI_Comm_Size ( local_comm, npes, ierror )
  CALL MPI_Comm_Rank ( local_comm, mype, ierror )

  print *,'atmos [', mype, ']'
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!!! OASIS_TERMINATE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !
  CALL MPI_Finalize(ierror)
  !
END PROGRAM ocean
!
