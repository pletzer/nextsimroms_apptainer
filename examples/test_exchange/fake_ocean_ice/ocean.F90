program ocean
   use mpi
   use mod_oasis
   use grid_mod
   implicit none
   integer :: i, kinfo, date
   integer :: comp_id, part_id, var_id
   integer :: part_params(OASIS_Apple_Params), offset, local_size
   integer :: local_comm, comm_size, comm_rank
   integer :: var_nodims(2)
   character(len=5) :: comp_name = "ocean"
   character(len=10) :: o_from_ocn = "O_FROM_OCN"
   ! use flat arrays 
   real(kind=8), allocatable :: bundle_export(:,:), bundle_import(:, :)

   integer :: nx_global, ny_global
   integer :: n_points
   integer :: ncid, varid
   real(kind=8), allocatable :: lon(:, :), lat(:, :)
   integer :: ll_i, ll_j
   real(kind=8) :: dp_conv

   call oasis_init_comp(comp_id, comp_name, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_init_comp: ", rcode=kinfo)

   call oasis_get_localcomm(local_comm, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_get_localcomm: ", rcode=kinfo)

   call mpi_comm_size(local_comm, comm_size, kinfo)
   call mpi_comm_rank(local_comm, comm_rank, kinfo)   
   print '(A,I0)', "ocean: Component ID: ", comp_id

   call read_dims('grids.nc', 'bggd', nx_global, ny_global)
   n_points = nx_global*ny_global
   allocate(lon(nx_global,ny_global), lat(nx_global,ny_global))
   call read_coords('grids.nc', 'bggd', lon, lat)

   local_size=n_points/comm_size
   offset=comm_rank*local_size
   if (comm_rank == comm_size - 1) &
      & local_size = n_points - offset

   part_params(OASIS_Strategy) = OASIS_Apple
   part_params(OASIS_Offset)   = offset
   part_params(OASIS_Length)   = local_size
   call oasis_def_partition(part_id, part_params, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_def_partition: ", rcode=kinfo)

   var_nodims=[1, 2]
   print '(A,I0,2A)', "ocean rank(",comm_rank,"): var_name: ", o_from_ocn
   call oasis_def_var(var_id, o_from_ocn, part_id, var_nodims, OASIS_OUT, &
      &               [1], OASIS_DOUBLE, kinfo)
   if(kinfo<0 .or. var_id<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_def_var: ", rcode=kinfo)

   call oasis_enddef(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_enddef: ", rcode=kinfo)

   dp_conv = atan(1.0)/45.0
   allocate(bundle_export(local_size,2), bundle_import(local_size,2))
   do i = 1, local_size
      ll_j = int((offset+i-1)/nx_global)+1
      ll_i = mod(offset+i-1,nx_global)+1
      bundle_export(i,1) = 2.0 + (sin(2.*lat(ll_i,ll_j)*dp_conv))**4 * &
         & cos(4.*lon(ll_i,ll_j)*dp_conv)
      bundle_export(i,2) = 2.0 - cos(atan(1.0)*4.* &
          & (acos(cos(lon(ll_i,ll_j)*dp_conv)*cos(lat(ll_i,ll_j)*dp_conv))/ &
          & (1.2*atan(1.)*4)))
   end do

   date=0

   call oasis_put(var_id, date, bundle_export, kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_put: ", rcode=kinfo)

   call oasis_terminate(kinfo)
   if(kinfo<0) call oasis_abort(comp_id, comp_name, &
      & "Error in oasis_terminate: ", rcode=kinfo)

end program ocean
