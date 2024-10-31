module datetime_mod

   use ESMF
   implicit none

   type datetime
   integer :: start_year, start_month, start_day
   integer :: start_hour, start_minute, start_second
   integer :: end_year, end_month, end_day
   integer :: end_hour, end_minute, end_second
   integer :: dt_seconds
   end type datetime

contains

    subroutine datetime_init_from_file(self, filename)
        implicit none
        type(datetime), intent(inout) :: self
        character(len=*), intent(in) ::  filename

        integer :: start_year, start_month, start_day
        integer :: start_hour, start_minute, start_second
        integer :: end_year, end_month, end_day
        integer :: end_hour, end_minute, end_second
        integer :: dt_seconds

        namelist/date/ start_year, start_month, start_day, &
                    &  start_hour, start_minute, start_second, &
                    &  end_year, end_month, end_day, &
                    &  end_hour, end_minute, end_second, &
                    &  dt_seconds


        start_year = -9999
        start_month = -9999
        start_day = -9999
        start_hour = -9999
        start_minute = -9999
        start_second = -9999

        end_year = -9999
        end_month = -9999
        end_day = -9999
        end_hour = -9999
        end_minute = -9999
        end_second = -9999

        dt_seconds = 0


        open(10, file=filename)
        read(10, nml=date)
        close(10)

        self%start_year = start_year
        self%start_month = start_month
        self%start_day = start_day
        self%start_hour = start_hour
        self%start_minute = start_minute
        self%start_second = start_second

        self%end_year = end_year
        self%end_month = end_month
        self%end_day = end_day
        self%end_hour = end_hour
        self%end_minute = end_minute
        self%end_second = end_second

    end subroutine datetime_init_from_file

    function datetime_get_start(self) result(res)
        type(datetime), intent(inout) :: self
        type(ESMF_Time) :: res
        call ESMF_TimeSet(res, yy=self%start_year, mm=self%start_month, &
                         & dd=self%start_day, h=self%start_hour, &
                         & m=self%start_minute, s=self%start_second)
    end function 

    function datetime_get_end(self) result(res)
        type(datetime), intent(inout) :: self
        type(ESMF_Time) :: res
        call ESMF_TimeSet(res, yy=self%end_year, mm=self%end_month, &
                         & dd=self%end_day, h=self%end_hour, &
                         & m=self%end_minute, s=self%end_second)
    end function 

    function datetime_get_dt_seconds(self) result(res)
        type(datetime), intent(inout) :: self
        TYPE(ESMF_TimeInterval) :: res
        call ESMF_TimeIntervalSet(res, s=self%dt_seconds)
    end function 

end module datetime_mod


program main
    use ESMF
    use datetime_mod
    implicit none

    TYPE(ESMF_VM) :: vm
    type(ESMF_GridComp) :: ice, ocn
    type(ESMF_CplComp) :: cpl

    TYPE(ESMF_State) :: importStateIce, exportStateIce
    TYPE(ESMF_State) :: importStateOcn, exportStateOcn

    ! A clock, some times, and a time step
    TYPE(ESMF_Clock) :: driverClock
    TYPE(ESMF_Time) :: startTime
    TYPE(ESMF_Time) :: stopTime
    TYPE(ESMF_TimeInterval) :: dTime

    type(datetime) :: start_end

    INTEGER :: rc

    CALL ESMF_Initialize(vm=vm, defaultCalKind=ESMF_CALKIND_GREGORIAN, &
        & logkindflag=ESMF_LOGKIND_MULTI, rc=rc)

    ! create the components
    ice = ESMF_GridCompCreate(name='ice', rc=rc)
    ocn = ESMF_GridCompCreate(name='ocn', rc=rc)
    cpl = ESMF_CplCompCreate(name='cpl', rc=rc)

    ! create the states
    importStateIce = ESMF_StateCreate(name='import ice', &
                     & stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    importStateOcn = ESMF_StateCreate(name='import ocn', &
                     & stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
    exportStateIce = ESMF_StateCreate(name='export ice', &
                     & stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
    exportStateOcn = ESMF_StateCreate(name='export ocn', &
                     & stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)

    ! read the start/end times and set the clocks
    call datetime_init_from_file(start_end, 'mainApp.nml') 
    startTime = datetime_get_start(start_end)
    stopTime = datetime_get_end(start_end)
    dTime = datetime_get_dt_seconds(start_end)
    driverClock = ESMF_ClockCreate(timeStep=dTime, &
                  & startTime=startTime,       &
                  & stopTime=stopTime, rc=rc)


    call ESMF_StateDestroy(exportStateOcn, rc=rc)
    call ESMF_StateDestroy(exportStateIce, rc=rc)
    call ESMF_StateDestroy(importStateOcn, rc=rc)
    call ESMF_StateDestroy(importStateIce, rc=rc)

    call ESMF_CplCompDestroy(cpl, rc=rc)
    call ESMF_GridCompDestroy(ocn, rc=rc)
    call ESMF_GridCompDestroy(ice, rc=rc)

    CALL ESMF_Finalize(rc=rc)
 
end program
