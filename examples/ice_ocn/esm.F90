!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  ! Enabling the followng macro, i.e. setting it to WITHPETLISTS_on, will
  ! activate sections of code that demonstrate how
  ! the ICE and OCN components can run on exclusive sets of PETs. Turning this
  ! on/off does not affect how the Connector component is specialized.
#define WITHPETLISTS_on

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driverSS             => SetServices

  use ICE, only: iceSS => SetServices
  use OCN, only: ocnSS => SetServices

  use CON, only: cplSS => SetServices

  implicit none

  private

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Driver
    call NUOPC_CompDerive(driver, driverSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize driver
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! ! set driver verbosity
    ! call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)
    ! if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    !   line=__LINE__, &
    !   file=__FILE__)) &
    !   return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Grid)               :: grid
    type(ESMF_Field)              :: field
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: conn
    integer                       :: petCount, i
    integer, allocatable          :: petListIce(:), petListOcn(:)
    integer                       :: verbosity
    character(len=10)             :: attrStr
    character(len=256)            :: msg

    rc = ESMF_SUCCESS

   ! get the petCount
    call ESMF_GridCompGet(driver, petCount=petCount, rc=rc)

    write(msg, *) 'esm: running on ', petCount, ' PEs'
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)

    ! SetServices for OCN and ICE
    allocate(petListIce(petCount/2))
    allocate(petListOcn(petCount - SIZE(petListIce)))
    do i=1, size(petListIce)
      petListIce(i) = i-1
    enddo
    do i=1, size(petListOcn)
      petListOcn(i) = SIZE(petListIce) + i-1
    enddo

    write(msg, *) 'esm: ICE running on ', petListIce
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)

    write(msg, *) 'esm: OCN running on ', petListOcn
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)

    call NUOPC_DriverAddComp(driver, "ICE", iceSS, petList=petListIce, comp=child, rc=rc)
    call NUOPC_DriverAddComp(driver, "OCN", ocnSS, petList=petListOcn, comp=child, rc=rc)


    ! SetServices for ice2ocn
    call NUOPC_DriverAddComp(driver, srcCompLabel="ICE", dstCompLabel="OCN", &
      compSetServicesRoutine=cplSS, comp=conn, rc=rc)

    ! SetServices for ocn2ice
    call NUOPC_DriverAddComp(driver, srcCompLabel="OCN", dstCompLabel="ICE", &
      compSetServicesRoutine=cplSS, comp=conn, rc=rc)

    ! Set the driver clock
    call ESMF_TimeIntervalSet(timeStep, m=10, rc=rc) ! 10 minute steps

    call ESMF_TimeSet(startTime, yy=2010, mm=6, dd=1, h=0, m=0, &
      calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)

    call ESMF_TimeSet(stopTime, yy=2010, mm=6, dd=1, h=1, m=0, &
      calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)

    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)

    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

end module
