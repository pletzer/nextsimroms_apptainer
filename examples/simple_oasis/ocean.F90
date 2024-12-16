PROGRAM ocean
  !
  ! Use for netCDF library
  USE netcdf
  use mpi
  use mod_oasis
  use def_parallel_decomposition
  IMPLICIT NONE

  integer :: ierror, local_comm, npes, mype
  integer :: comp_id
  integer :: nlon, nlat, il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset

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

  call oasis_init_comp(comp_id, 'ocn', ierror)

  nlon = 4
  nlat = 2
  call def_local_partition(nlon, nlat, npes, mype, &
    & il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset)
  print *,'ocean [', mype, '] il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset = ', &
                           &  il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset

  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!!! OASIS_TERMINATE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call oasis_terminate()
  !
  !
  CALL MPI_Finalize(ierror)
  !
END PROGRAM ocean
!
