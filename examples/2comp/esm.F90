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

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driverSS             => SetServices

  use ICE, only: iceSS => SetServices
  use OCN, only: ocnSS => SetServices

  use CON, only: cplSS => SetServices

  implicit none

  type calendar
      integer :: year, month, day, hour, minute, second
  end type

  private

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  function readCalendars(filename) result(cal)
    character(len=*), intent(in) :: filename
    type(calendar) :: cal(2)
    integer :: year, month, day, hour, minute, second
    integer :: fu
    namelist /start/ year, month, day, hour, minute, second
    namelist /end/ year, month, day, hour, minute, second

    year = 2000
    month = 1
    day = 1
    hour = 0
    minute = 0
    second = 0
    
    open(newunit=fu, file=filename)
    read(fu, nml=start)
    cal(1)%year = year
    cal(1)%month = month
    cal(1)%day = day
    cal(1)%hour = hour
    cal(1)%minute = minute
    cal(1)%second = second
    rewind(fu)
    read(fu, nml=end)
    cal(2)%year = year
    cal(2)%month = month
    cal(2)%day = day
    cal(2)%hour = hour
    cal(2)%minute = minute
    cal(2)%second = second
    close(fu)

  end function readCalendars

  function getTimeIntervalSeconds(filename) result(dt)
    character(len=*), intent(in) :: filename
    integer :: dt, fu
    namelist /time_interval/ dt

    dt = 0
    open(newunit=fu, file=filename)
    read(fu, nml=time_interval)
    close(fu)

  end function getTimeIntervalSeconds


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
    type(calendar)                :: cal(2)

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

    ! Populate the list of predefined fields from YAML file
    call NUOPC_FieldDictionarySetup('nextsimroms.yml', rc)
    if (rc /= ESMF_SUCCESS) then
      write(msg, *) 'esm: Could not read file "nextsimroms.yml" rc=', rc
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
      return
    endif

    call NUOPC_DriverAddComp(driver, "ICE", iceSS, petList=petListIce, comp=child, rc=rc)
    call NUOPC_DriverAddComp(driver, "OCN", ocnSS, petList=petListOcn, comp=child, rc=rc)


    ! SetServices for ice2ocn
    call NUOPC_DriverAddComp(driver, srcCompLabel="ICE", dstCompLabel="OCN", &
      compSetServicesRoutine=cplSS, comp=conn, rc=rc)

    ! SetServices for ocn2ice
    call NUOPC_DriverAddComp(driver, srcCompLabel="OCN", dstCompLabel="ICE", &
      compSetServicesRoutine=cplSS, comp=conn, rc=rc)

    ! Set the driver clock
    call ESMF_TimeIntervalSet(timeStep, s=getTimeIntervalSeconds("esmApp.nml"), rc=rc) ! time step in seconds

    cal = readCalendars("esmApp.nml")
    call ESMF_TimeSet(startTime, &
                                 yy=cal(1)%year, &
                                 mm=cal(1)%month, &
                                 dd=cal(1)%day,  &
                                 h=cal(1)%hour, &
                                 m=cal(1)%minute, &
                                 s=cal(1)%second, &
                                 calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)

    call ESMF_TimeSet(stopTime, &
                                 yy=cal(2)%year, &
                                 mm=cal(2)%month, &
                                 dd=cal(2)%day,  &
                                 h=cal(2)%hour, &
                                 m=cal(2)%minute, &
                                 s=cal(2)%second, &
                                 calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)

    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)

    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)

  end subroutine

  !-----------------------------------------------------------------------------

end module
