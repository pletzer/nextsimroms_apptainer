PROGRAM ice
  !
  ! Use for netCDF library
  USE netcdf
  use tovtk_mod, only : vtk_write_data_xyz, zero_fill
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
  INTEGER :: local_comm  ! local communicator for ice processes
  CHARACTER(len=128) :: comp_out_ice ! name of the output log file 
  CHARACTER(len=3)   :: chout
  INTEGER :: ierror, w_unit
  INTEGER :: info
  !
  ! Global grid parameters
  INTEGER :: nlon_ice, nlat_ice    ! dimensions in the 2 spatial directions
  INTEGER, PARAMETER :: nc_ice = 4 ! number of grid cell vertices in the (i,j) plan
  !
  ! Local grid dimensions and arrays
  INTEGER :: il_extentx, il_extenty, il_offsetx, il_offsety
  INTEGER :: il_size, il_offset
  DOUBLE PRECISION, DIMENSION(:,:),   POINTER   :: grid_lon_ice, grid_lat_ice ! lon, lat of the cell centers
  DOUBLE PRECISION, DIMENSION(:,:,:), POINTER   :: grid_clo_ice, grid_cla_ice ! lon, lat of the cell corners
  DOUBLE PRECISION, DIMENSION(:,:),   POINTER   :: grid_srf_ice ! surface of the grid meshes
  INTEGER, DIMENSION(:,:),            POINTER   :: grid_msk_ice ! mask, 0 == valid point, 1 == masked point
  !
  ! For time step loop
  INTEGER               ::  ib
  INTEGER, PARAMETER    ::  il_nb_time_steps = 8 ! number of time steps
  INTEGER, PARAMETER    ::  delta_t = 1800       ! time step
  INTEGER               ::  itap_sec ! time in seconds
  DOUBLE PRECISION, PARAMETER    :: dp_pi=3.14159265359
  DOUBLE PRECISION, PARAMETER    :: dp_length= 1.2*dp_pi
  !
  ! Local coupling fields arrays exchanged via oasis_get and oasis_put
  DOUBLE PRECISION, POINTER :: field_recv_ice(:,:)
  DOUBLE PRECISION, POINTER :: field_send_ice(:,:)
  !
  ! Used in OASIS3-MCT definition calls
  INTEGER               :: compid 
  INTEGER               :: il_part_id
  INTEGER               :: ig_paral_size
  INTEGER, DIMENSION(:), ALLOCATABLE :: ig_paral
  INTEGER               :: flag          ! Flag for grid writing
  INTEGER               :: var_id(2)
  INTEGER               :: var_nodims(2)
  INTEGER               :: var_actual_shape(1) ! not used anymore in OASIS3-MCT
  INTEGER               :: var_type
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  INITIALISATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  call MPI_Init(ierror)
  !
  local_comm =  MPI_COMM_WORLD
  !
  !!!!!!!!!!!!!!!!! OASIS_INIT_COMP !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL oasis_init_comp (compid,'ice_component',ierror)
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
  comp_out_ice='ice.out_'//chout
  !
  OPEN(w_unit,file=TRIM(comp_out_ice),form='formatted')
  WRITE (w_unit,*) '-----------------------------------------------------------'
  WRITE (w_unit,*) 'I am ice process with rank :', mype
  WRITE (w_unit,*) 'in my local communicator gathering ', npes, 'processes'
  WRITE (w_unit,*) '----------------------------------------------------------'
  CALL flush(w_unit)

  call read_dims('ice_mesh.nc', 'lon', nlon_ice, nlat_ice)
  WRITE(w_unit,'(A, I10, A, I10)') 'nlon_ice = ', nlon_ice, ' nlat_ice = ', nlat_ice
  
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  PARTITION DEFINITION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !
  ! Definition of the local partition
  call def_local_partition(nlon_ice, nlat_ice, npes, mype, &
  	     		 il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset)
  WRITE(w_unit,*) 'Local partition definition'
  WRITE(w_unit,*) 'il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset = ', &
                   il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset
  !
  !!!!!!!!!!!!!!!!! OASIS_DEF_PARTITION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call def_paral_size (ig_paral_size)
  ALLOCATE(ig_paral(ig_paral_size))
  call def_paral (il_offset, il_size, il_extentx, il_extenty, nlon_ice, ig_paral_size, ig_paral)
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
  ALLOCATE(grid_lon_ice(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(grid_lat_ice(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(grid_clo_ice(il_extentx, il_extenty, nc_ice), STAT=ierror )
  ALLOCATE(grid_cla_ice(il_extentx, il_extenty, nc_ice), STAT=ierror )
  ALLOCATE(grid_srf_ice(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(grid_msk_ice(il_extentx, il_extenty), STAT=ierror )
  !
  ! Reading local grid arrays from input file ocean_mesh.nc
  CALL read_grid(nlon_ice, nlat_ice, nc_ice, il_offsetx+1, il_offsety+1, il_extentx, il_extenty, &
                'ice_mesh.nc', w_unit, grid_lon_ice, grid_lat_ice, grid_clo_ice, &
                grid_cla_ice, grid_srf_ice, grid_msk_ice)
  !
  !!!!!!!!!!!!!!!!! OASIS_WRITE_GRID  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CALL oasis_start_grids_writing(flag)
  CALL oasis_write_grid('lmdz', nlon_ice, nlat_ice, grid_lon_ice, grid_lat_ice, il_part_id)
  CALL oasis_write_corner('lmdz', nlon_ice, nlat_ice, 4, grid_clo_ice, grid_cla_ice, il_part_id)
  CALL oasis_write_mask('lmdz', nlon_ice, nlat_ice, grid_msk_ice(:,:), il_part_id)
  CALL oasis_terminate_grids_writing()
  WRITE(w_unit,*) 'grid_lat_ice maximum and minimum', MAXVAL(grid_lat_ice), MINVAL(grid_lat_ice)
  call flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  DEFINITION OF THE LOCAL FIELDS  
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocate local coupling fields
  ALLOCATE(field_send_ice(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(field_recv_ice(il_extentx, il_extenty), STAT=ierror )
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  DECLARATION OF THE COUPLING FIELDS  
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! 
  !!!!!!!!!!!!!!!!!! OASIS_DEF_VAR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  var_nodims(1) = 2    ! Rank of the field array ; not used anymore in OASIS3-MCT
  var_nodims(2) = 1    ! Number of bundle fields
  var_actual_shape(1) = 1 ! Not used anymore in OASIS3-MCT
  var_type = OASIS_Real
  !
  ! Declaration of the coupling fields
  CALL oasis_def_var (var_id(1),'FIELD_RECV_ATM', il_part_id, var_nodims, OASIS_In, var_actual_shape, var_type, ierror)
  CALL oasis_def_var (var_id(2),'FIELD_SEND_ATM', il_part_id, var_nodims, OASIS_Out, var_actual_shape, var_type, ierror)
  WRITE(w_unit,*)'var_id FRECVATM, var_id FSENDATM', var_id(1), var_id(2)
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
  DO ib = 1, il_nb_time_steps
    !
    itap_sec = delta_t * (ib-1) ! time in seconds

    ! initialize
    field_recv_ice=-1.0
    !
    !!!!!!!!!!!!!!!!!!!!!!!! OASIS_GET !!!!!!!!!!!!!!!!!!!!!!
    CALL oasis_get(var_id(1), itap_sec, field_recv_ice, info)
  
    call vtk_write_data_xyz(grid_clo_ice, grid_cla_ice, &
       & field_recv_ice, 'field_recv_ice', &
       & 'field_recv_ice_pe' // trim(zero_fill(mype, 2)) // '_t' // trim(zero_fill(ib, 3)) // '.vtk')

    write(w_unit,*) 'Done writing field_recv_ice in file ', 'field_recv_ice' // trim(zero_fill(ib, 5)) // '.vtk'
    ! 
    ! Definition of field produced by the component
    field_send_ice(:,:) =  ib*(2.-COS(dp_pi*(ACOS(COS(grid_lat_ice(:,:)*dp_pi/90.)* &
                           COS(grid_lon_ice(:,:)*dp_pi/90.))/dp_length)))
    !write(w_unit,*) itap_sec,minval(field_send_ice),maxval(field_send_ice)
    !
    !!!!!!!!!!!!!!!!!!!!!!!! OASIS_PUT !!!!!!!!!!!!!!!!!!!!!!
    CALL oasis_put(var_id(2), itap_sec, field_send_ice, info) 
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
END PROGRAM ice
!
