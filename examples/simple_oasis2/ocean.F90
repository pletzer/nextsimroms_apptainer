PROGRAM ocean
  !
  ! Use for netCDF library
  USE netcdf
  !
  USE def_parallel_decomposition
  !!!!!!!!!!!!!!!!! USE mod_oasis !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  USE mod_oasis
  !
  IMPLICIT NONE
  !
  INCLUDE 'mpif.h'   ! Include for MPI
  !
  INTEGER :: mype, npes ! rank and number of pe
  INTEGER :: local_comm  ! local communicator for ocean processes
  CHARACTER(len=128) :: comp_out_ocean ! name of the output log file 
  CHARACTER(len=3)   :: chout
  INTEGER :: ierror, w_unit
  INTEGER :: info
  !
  ! Global grid parameters
  INTEGER, PARAMETER :: nlon_ocean = 182, nlat_ocean = 149    ! dimensions in the 2 spatial directions
  INTEGER, PARAMETER :: nc_ocean = 4 ! number of grid cell vertices in the (i,j) plan
  !
  ! Local grid dimensions and arrays
  INTEGER :: il_extentx, il_extenty, il_offsetx, il_offsety
  INTEGER :: il_size, il_offset
  DOUBLE PRECISION, DIMENSION(:,:),   POINTER   :: grid_lon_ocean, grid_lat_ocean ! lon, lat of the cell centers
  DOUBLE PRECISION, DIMENSION(:,:,:), POINTER   :: grid_clo_ocean, grid_cla_ocean ! lon, lat of the cell corners
  DOUBLE PRECISION, DIMENSION(:,:),   POINTER   :: grid_srf_ocean ! surface of the grid meshes
  INTEGER, DIMENSION(:,:),            POINTER   :: grid_msk_ocean ! mask, 0 == valid point, 1 == masked point
  !
  ! For time step loop
  INTEGER               ::  ib
  INTEGER, PARAMETER    ::  il_nb_time_steps = 4 ! number of time steps
  INTEGER, PARAMETER    ::  delta_t = 3600       ! time step
  INTEGER               ::  itap_sec ! time in seconds
  DOUBLE PRECISION, PARAMETER    :: dp_pi=3.14159265359
  DOUBLE PRECISION, PARAMETER    :: dp_length= 1.2*dp_pi
  !
  ! Local coupling fields arrays exchanged via oasis_get and oasis_put
  DOUBLE PRECISION, POINTER :: field_recv_ocean(:,:)
  DOUBLE PRECISION, POINTER :: field_send_ocean(:,:)
  !
  ! Used in OASIS3-MCT definition calls
  INTEGER               :: compid
  INTEGER               :: il_part_id
  INTEGER               :: ig_paral_size
  INTEGER, DIMENSION(:), ALLOCATABLE :: ig_paral
  INTEGER               :: flag          ! Flag for grid writing
  INTEGER               :: var_id(2)
  INTEGER               :: var_nodims(2)
  INTEGER               :: var_actual_shape(1)
  INTEGER               :: var_type
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  INITIALISATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  call MPI_Init(ierror)
  !
  local_comm =  MPI_COMM_WORLD
  !
  !!!!!!!!!!!!!!!!! OASIS_INIT_COMP !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL oasis_init_comp (compid,'ocean_component',ierror)
  !
  !!!!!!!!!!!!!!!!! OASIS_GET_LOCALCOMM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL oasis_get_localcomm ( local_comm, ierror )
  !
  ! Get rank in local communicator
  CALL MPI_Comm_Size ( local_comm, npes, ierror )
  CALL MPI_Comm_Rank ( local_comm, mype, ierror )
  !
  ! Unit for output messages : one file for each process
  w_unit = 100 + mype
  WRITE(chout,'(I3)') w_unit
  comp_out_ocean='ocean.out_'//chout
  !
  OPEN(w_unit,file=TRIM(comp_out_ocean),form='formatted')
  WRITE (w_unit,*) '-----------------------------------------------------------'
  WRITE (w_unit,*) 'I am ocean process with rank :',mype
  WRITE (w_unit,*) 'in my local communicator gathering ', npes, 'processes'
  WRITE (w_unit,*) '----------------------------------------------------------'
  CALL flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  PARTITION DEFINITION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !
  ! Definition of the local partition
  call def_local_partition(nlon_ocean, nlat_ocean, npes, mype, &
  	     		 il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset)
  WRITE(w_unit,*) 'Local partition definition'
  WRITE(w_unit,*) 'il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset = ', &
                   il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset
  !
  !!!!!!!!!!!!!!!!! OASIS_DEF_PARTITION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call def_paral_size (ig_paral_size)
  ALLOCATE(ig_paral(ig_paral_size))
  call def_paral (il_offset, il_size, il_extentx, il_extenty, nlon_ocean, ig_paral_size, ig_paral)
  WRITE(w_unit,*) 'ig_paral = ', ig_paral(:)
  call flush(w_unit)
  CALL oasis_def_partition (il_part_id, ig_paral, ierror)
  DEALLOCATE(ig_paral)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  GRID DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocation of local grid arrays
  ALLOCATE(grid_lon_ocean(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(grid_lat_ocean(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(grid_clo_ocean(il_extentx, il_extenty, nc_ocean), STAT=ierror )
  ALLOCATE(grid_cla_ocean(il_extentx, il_extenty, nc_ocean), STAT=ierror )
  ALLOCATE(grid_srf_ocean(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(grid_msk_ocean(il_extentx, il_extenty), STAT=ierror )
  !
  ! Reading local grid arrays from input file ocean_mesh.nc
  CALL read_grid(nlon_ocean, nlat_ocean, nc_ocean, il_offsetx+1, il_offsety+1, il_extentx, il_extenty, &
                'ocean_mesh.nc', w_unit, grid_lon_ocean, grid_lat_ocean, grid_clo_ocean, &
                grid_cla_ocean, grid_srf_ocean, grid_msk_ocean)
  !
  !!!!!!!!!!!!!!!!! OASIS_WRITE_GRID  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL oasis_start_grids_writing(flag)
  CALL oasis_write_grid('torc', nlon_ocean, nlat_ocean, grid_lon_ocean, grid_lat_ocean, il_part_id)
  CALL oasis_write_corner('torc', nlon_ocean, nlat_ocean, 4, grid_clo_ocean, grid_cla_ocean, il_part_id)
  CALL oasis_write_mask('torc', nlon_ocean, nlat_ocean, grid_msk_ocean(:,:), il_part_id)
  CALL oasis_terminate_grids_writing()
  WRITE(w_unit,*) 'grid_lat_ocean maximum and minimum', MAXVAL(grid_lat_ocean), MINVAL(grid_lat_ocean)
  call flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  DEFINITION OF THE LOCAL FIELDS  
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocate local coupling fields
  ALLOCATE(field_send_ocean(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(field_recv_ocean(il_extentx, il_extenty), STAT=ierror )
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  DECLARATION OF THE COUPLING FIELDS  
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! 
  !!!!!!!!!!!!!!!!!! OASIS_DEF_VAR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  var_nodims(1) = 2    ! Rank of the field array ; not used anymore in OASIS3-MCT
  var_nodims(2) = 1    ! Number of bundle fields
  var_actual_shape(1) = 1 ! Not used anymore in OASIS3-MCT
  var_type = OASIS_Real
  !
  ! Declaration of the coupling fields
  CALL oasis_def_var (var_id(1),'FIELD_RECV_OCN', il_part_id, var_nodims, OASIS_In, var_actual_shape, var_type, ierror)
  CALL oasis_def_var (var_id(2),'FIELD_SEND_OCN', il_part_id, var_nodims, OASIS_Out, var_actual_shape, var_type, ierror)
  WRITE(w_unit,*)'var_id FRECVOCN, var_id FSENDOCN', var_id(1), var_id(2)
  call flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION OF DEFINITION PHASE 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  WRITE(w_unit,*) 'End of initialisation phase'
  call flush(w_unit)
  !
  !!!!!!!!!!!!!!!!!! OASIS_ENDDEF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL oasis_enddef (ierror)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  TIME STEP LOOP
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  write(w_unit,*) 'Timestep, field min and max value'
  call flush(w_unit)
  DO ib = 1,il_nb_time_steps
    !
    itap_sec = delta_t * (ib-1) ! time in seconds
    field_recv_ocean=-1.0
    !
    !!!!!!!!!!!!!!!!!!!!!!!! OASIS_GET !!!!!!!!!!!!!!!!!!!!!!
    CALL oasis_get(var_id(1),itap_sec, field_recv_ocean, info)
    write(w_unit,*) itap_sec,minval(field_recv_ocean),maxval(field_recv_ocean)
    !
    ! Definition of field produced by the component
    field_send_ocean(:,:) =  ib*(2.-COS(dp_pi*(ACOS(COS(grid_lat_ocean(:,:)*dp_pi/180.)* &
                           COS(grid_lon_ocean(:,:)*dp_pi/180.))/dp_length)))
    !write(w_unit,*) itap_sec,minval(field_send_ocean),maxval(field_send_ocean)
    !
    !!!!!!!!!!!!!!!!!!!!!!!! OASIS_PUT !!!!!!!!!!!!!!!!!!!!!! 
    CALL oasis_put(var_id(2),itap_sec, field_send_ocean, info)
    !
  ENDDO
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!!! OASIS_TERMINATE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL oasis_terminate (ierror)
  !
  WRITE (w_unit,*) 'End of the program'
  CALL flush(w_unit)
  CLOSE(w_unit)
  !
  CALL MPI_Finalize(ierror)
  !
END PROGRAM ocean
!
