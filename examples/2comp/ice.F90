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
    real(8) :: u, v, dt, dx, dy
    integer :: nx, ny
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
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: grid

    integer :: fu
    character(len=256) :: msg

    real(8) :: xmin, xmax, ymin, ymax, dt, u, v
    integer :: nx, ny, nt

    namelist /ice/ nx, ny, xmin, xmax, ymin, ymax
    namelist /numerics/ dt, nt
    namelist /physics/ u, v

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
    nt = 1
    dt = 0
    u = 0
    v = 0
    open(newunit=fu, file='mainApp.nml', action='read')
    read(unit=fu, nml=ice)
    close(unit=fu)
    open(newunit=fu, file='mainApp.nml', action='read')
    read(unit=fu, nml=numerics)
    close(unit=fu)
    open(newunit=fu, file='mainApp.nml', action='read')
    read(unit=fu, nml=physics)
    close(unit=fu)
    write(msg, *) 'ice grid ', nx, '*', ny, &
     & ' xmin, ymin = ', xmin, ymin, ' xmax, ymax = ', xmax, ymax
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)

      ! create a Grid object for Fields
    grid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/nx, ny/), &
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
                        & filename="ice_grid", rc=rc)

    ! importable field: sea_surface_density from ocean
    field = ESMF_FieldCreate(name="rho_ocn", grid=grid, &
      typekind=ESMF_TYPEKIND_R8, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      rc=rc)
    !call ESMF_FieldGet(field, rc=rc) ! ????
    call NUOPC_Realize(importState, field=field, rc=rc)

    ! exportable field: sea_surface_density from ice
    field = ESMF_FieldCreate(name="rho_ice", grid=grid, &
      typekind=ESMF_TYPEKIND_R8, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      rc=rc)
    call NUOPC_Realize(exportState, field=field, rc=rc)

    ! store
    self%u = u
    self%v = v
    self%dx = (xmax - xmin)/real(nx, 8)
    self%dy = (ymax - ymin)/real(ny, 8)
    self%dt = dt
    self%nx = nx
    self%ny = ny

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    character(len=160)          :: msgString
    integer                     :: numImportStates, i, ndims, j, i1, i2, localDeCount
    character(len=128), allocatable :: importStateNames(:)
    type(ESMF_Field)            :: field
    integer, allocatable        :: localMinIndex(:), localMaxIndex(:)
    type(ESMF_Array)            :: array
    type(ESMF_Grid)             :: grid
    type(ESMF_VM)               :: vm
    real(ESMF_KIND_r8), pointer      :: rho(:,:)
    real(ESMF_KIND_R8)               :: total_rho
    logical                          :: import = .TRUE., export = .FALSE.
    integer :: i0, j0, im, jm, ip, jp
    real(8) :: cx, cy

    type(ESMF_State)        :: state
    integer :: rc2

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call esmfutils_getAreaIntegratedField(model, import, 'rho_ocn', total_rho, rc=rc)
    print *,'ice integrated rho from ocean: ', total_rho
    
    call NUOPC_ModelGet(model,  importState=state, rc=rc)
    call ESMF_StateGet(state, itemName='rho_ocn', field=field, rc=rc)
    call ESMF_ArrayGet(array, farrayPtr=rho, rc=rc)

    cx = self%u * self%dt / self%dx
    cy = self%v * self%dt / self%dy

    do j0 = lbound(rho, 2), ubound(rho, 2)

      jp = j0 + 1
      if (jp > self%ny) jp = 1 ! periodic
      jm = j0 - 1
      if (jm == 0) jm = self%ny ! periodic

      do i0 = lbound(rho, 1), ubound(rho, 1)

        ip = i0 + 1
        if (ip > self%nx) ip = 1 ! periodic
        im = i0 - 1
        if (im == 0) im = self%nx  

        ! update the field
        rho(i0, j0) = rho(i0,j0) - cx*(rho(i0, j0) - rho(im, j0)) - cy*(rho(i0, j0) - rho(i0, jm))
        
      enddo
    enddo


    call esmfutils_write2DStructFieldVTK(field, 'ice_rho.vtk')

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

  end subroutine

  !-----------------------------------------------------------------------------

end module
