!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module OCN

  !-----------------------------------------------------------------------------
  ! OCN Component.
  !-----------------------------------------------------------------------------

  use esmfutils_mod

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices

  implicit none

  private

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

    ! importable field: density correction
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_density_inc", name="drho", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: density
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_density", name="rho", rc=rc)
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
    type(ESMF_TimeInterval) :: stabilityTimeStep
    type(ESMF_Field)        :: field_ocn, field_ice
    type(ESMF_Grid)         :: grid

    real(8), pointer        :: rho(:, :)
    integer :: i, j
    integer :: nx, ny, fu
    real(8) :: xmin, xmax, ymin, ymax, x, y, dx, dy, elx, ely, x0, y0
    character(len=256) :: msg

    namelist /ocn/ nx, ny, xmin, xmax, ymin, ymax

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    nx = 10
    ny = 20
    xmin = 0
    xmax = 1
    ymin = 0
    ymax = 1
    open(newunit=fu, file='esmApp.nml', action='read')
    read(unit=fu, nml=ocn) 
    close(unit=fu)
    write(msg, *) 'ocn grid ', nx, '*', ny, &
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
      return  ! bail out    

    call ESMF_GridWriteVTK(grid, &
      & staggerloc=ESMF_STAGGERLOC_CORNER, &
      & filename="ocn_grid", rc=rc)

    ! importable field
    field_ice = ESMF_FieldCreate(name="drho", grid=grid, &
      typekind=ESMF_TYPEKIND_R8, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      rc=rc)
    call NUOPC_Realize(importState, field=field_ice, rc=rc)

    ! exportable field
    field_ocn = ESMF_FieldCreate(name="rho", grid=grid, &
      typekind=ESMF_TYPEKIND_R8,  & 
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      rc=rc)
    call NUOPC_Realize(exportState, field=field_ocn, rc=rc)

    call ESMF_FieldGet(field_ocn, farrayPtr=rho, rc=rc)
    if (rc /= ESMF_SUCCESS) print *,'failed to access rho ocn array'
    
    ! initial conditions
    elx = xmax - xmin
    ely = ymax - ymin
    dx = elx / real(nx, 8)
    dy = ely / real(ny, 8)
    x0 = xmin + 0.5*elx
    y0 = ymin + 0.5*ely
    do j = lbound(rho, 2), ubound(rho, 2)
      y = ymin + j*dy
      do i = lbound(rho, 1), ubound(rho, 1)
        x = xmin + i*dx
        ! Gaussian bump
        rho(i, j) = exp( -0.5 * ( (x - x0)**2 / (0.3*elx)**2  + (y - y0)**2 / (0.3*ely)**2 ) )
      enddo
    enddo

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
    type(ESMF_State)            :: state
    type(ESMF_Field)            :: field_ice, field_ocn
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: currTime
    type(ESMF_TimeInterval)     :: timeStep
    character(len=160)          :: msgString
    real(ESMF_KIND_r8), pointer :: rho(:,:), drho(:, :)
    real(ESMF_KIND_R8)          :: total_rho
    logical                     :: import=.TRUE., export=.FALSE.
    integer                     :: i, j

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in
    ! multiple calls to the Advance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing OCN from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimePrint(currTime + timeStep, &
      preString="---------------------> to: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_ModelGet(model,  exportState=state, rc=rc)
    call ESMF_StateGet(state, itemName='rho', field=field_ocn, rc=rc)
    call ESMF_FieldGet(field_ocn, farrayPtr=rho, rc=rc)

    call NUOPC_ModelGet(model,  importState=state, rc=rc)
    call ESMF_StateGet(state, itemName='drho', field=field_ice, rc=rc)
    call ESMF_FieldGet(field_ice, farrayPtr=drho, rc=rc)

    ! add the correction coming from the ice
    do j = lbound(rho, 2), ubound(rho, 2)
      do i = lbound(rho, 1), ubound(rho, 1)
        rho(i, j) = rho(i, j) + drho(i, j)
      enddo
    enddo
  
    call esmfutils_getAreaIntegratedField(model, export, 'rho', total_rho, rc=rc)
    print *,'ocn integrated rho after completing an ocean time step: ', total_rho


  end subroutine

  !-----------------------------------------------------------------------------

end module
