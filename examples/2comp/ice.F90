!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ICE

  !-----------------------------------------------------------------------------
  ! ICE Component.
  !-----------------------------------------------------------------------------

  use esmfutils_mod

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices


  implicit none

  type ice_type
    real(8) :: u, v, dx, dy
    integer :: nx, ny, dt, time_iteration
  end type

  private

  type(ice_type) :: self

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize model
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      call NUOPC_CompSpecialize(model, specLabel=label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advertise(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.
#define WITHIMPORTFIELDS
#ifdef WITHIMPORTFIELDS
    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_density", name="rho_ocn", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

    ! exportable field: 
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_density", name="rho_ice", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Field)        :: fieldOcn, field_ice
    type(ESMF_Grid)         :: grid

    integer :: fu
    character(len=256) :: msg
    real(8), pointer   :: rho_ice(:, :)

    real(8) :: xmin, xmax, ymin, ymax, u, v
    integer :: nx, ny, dt

    namelist /ice/ nx, ny, xmin, xmax, ymin, ymax, u, v
    namelist /time_interval/ dt

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! default values, when not specified in the namelist file
    nx = 10
    ny = 20
    xmin = 0
    xmax = 1
    ymin = 0
    ymax = 1
    u = 0
    v = 0
    dt = 0
    open(newunit=fu, file='esmApp.nml', action='read')
    read(unit=fu, nml=ice)
    rewind(unit=fu)
    read(unit=fu, nml=time_interval)
    close(unit=fu)
    write(msg, *) 'ice grid ', nx, '*', ny, &
     & ' xmin, ymin = ', xmin, ymin, ' xmax, ymax = ', xmax, ymax
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)

      ! create a Grid object for Fields
    grid = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/nx, ny/), &
      minCornerCoord=(/xmin, ymin/), &
      maxCornerCoord=(/xmax, ymax/), &
      coordSys=ESMF_COORDSYS_CART, &
      staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &

    call ESMF_GridWriteVTK(grid, &
                        & staggerloc=ESMF_STAGGERLOC_CORNER, &
                        & filename="ice_grid", rc=rc)

    ! importable field: sea_surface_density from ocean
    field_ocn = ESMF_FieldCreate(name="rho_ocn", &
      grid=grid, &
      typekind=ESMF_TYPEKIND_R8, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      rc=rc)
    call NUOPC_Realize(importState, field=field_ocn, rc=rc)

    ! exportable field: sea_surface_density from ice
    field_ice = ESMF_FieldCreate(name="rho_ice", &
      grid=grid, &
      typekind=ESMF_TYPEKIND_R8, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      rc=rc)
    call NUOPC_Realize(exportState, field=field_ice, rc=rc)

    call ESMF_FieldGet(field_ice, farrayPtr=rho_ice, rc=rc)

    do j = lbound(rho_ice, 2), ubound(rho_ice, 2)
      do i = lbound(rho_ice, 1), ubound(rho_ice, 1)
        ! zero initial condition
        rho_ice(i, j) = 0
      enddo
    enddo

    ! store
    self%u = u
    self%v = v
    self%dx = (xmax - xmin)/real(nx, 8)
    self%dy = (ymax - ymin)/real(ny, 8)
    self%dt = dt
    self%nx = nx
    self%ny = ny
    self%time_iteration = 0

  end subroutine

    !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep
    type(ESMF_State)              :: state
    type(ESMF_Field)              :: field

    rc = ESMF_SUCCESS

    ! query for clock
    call NUOPC_ModelGet(model, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    !TODO: stabilityTimeStep should be read in from configuation
    !TODO: or computed from internal Grid information
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=5, rc=rc) ! 5 minute steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: state
    character(len=160)          :: msgString, filename
    integer                     :: numImportStates, i, ndims, j, i1, i2, localDeCount
    character(len=128), allocatable :: importStateNames(:)
    type(ESMF_Field)            :: field_ocn, field_ice
    integer, allocatable        :: localMinIndex(:), localMaxIndex(:)
    type(ESMF_Array)            :: array
    type(ESMF_Grid)             :: grid
    type(ESMF_VM)               :: vm
    real(ESMF_KIND_r8), pointer      :: rho_ocn(:,:), rho_ice(:, :)
    real(ESMF_KIND_R8)               :: total_rho, drhodx, drhody, usign, vsign
    logical, parameter               :: importFlag = .TRUE., exportFlag = .FALSE.
    integer :: i0, j0, im, jm
    real(8) :: cx, cy

    integer :: rc2

    rc = ESMF_SUCCESS

    call esmfutils_getAreaIntegratedField(model, importFlag, 'rho_ocn', total_rho, rc=rc)
    print *,'ice integrated rho from ocean: ', total_rho
    
    call NUOPC_ModelGet(model,  importState=state, rc=rc)
    call ESMF_StateGet(state, itemName='rho_ocn', field=field_ocn, rc=rc)
    call ESMF_FieldGet(field_ocn, farrayPtr=rho_ocn, rc=rc)

    call NUOPC_ModelGet(model,  exportState=state, rc=rc)
    call ESMF_StateGet(state, itemName='rho_ice', field=field_ice, rc=rc)
    call ESMF_FieldGet(field_ice, farrayPtr=rho_ice, rc=rc)

    usign = sign(1._8, self%u) ! 1 with the isgn of self%u
    vsign = sign(1._8, self%v)

    cx = self%u * real(self%dt, 8) / self%dx
    cy = self%v * real(self%dt, 8) / self%dy

    ! advance the solution by one time step...
    do j0 = lbound(rho_ice, 2), ubound(rho_ice, 2)

      jm = j0 - int(vsign)
      if (jm == 0) jm = self%ny ! periodic
      if (jm > self%ny) jm = self%ny ! periodic

      do i0 = lbound(rho_ice, 1), ubound(rho_ice, 1)

        im = i0 - int(usign) ! sign(a, b) returns the value of a with the sign of b
        if (im == 0) im = self%nx  
        if (im > self%nx) im = 1 ! periodic

        ! upwind update of the field
        drhodx = usign * (rho_ocn(i0, j0) - rho_ocn(im, j0))
        drhody = vsign * (rho_ocn(i0, j0) - rho_ocn(i0, jm))
        rho_ice(i0, j0) = rho_ocn(i0,j0) - cx*drhodx - cy*drhody
        
      enddo
    enddo

    filename = 'ice_' // esmfutils_int_to_string_with_zeros(self%time_iteration, 4) // '.vtk'
    call esmfutils_write2DStructFieldVTK(field_ice, filename)

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the Advance() routine.
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ICE from: ", unit=msgString, rc=rc)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, rc=rc)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    ! update the interation counter
    self%time_iteration = self%time_iteration + 1

  end subroutine

  !-----------------------------------------------------------------------------

end module
