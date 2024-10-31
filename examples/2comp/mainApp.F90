program main
    use ESMF
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
    TYPE(ESMF_TimeInterval) :: couplingInterval

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

    call ESMF_StateDestroy(exportStateOcn, rc=rc)
    call ESMF_StateDestroy(exportStateIce, rc=rc)
    call ESMF_StateDestroy(importStateOcn, rc=rc)
    call ESMF_StateDestroy(importStateIce, rc=rc)

    call ESMF_CplCompDestroy(cpl, rc=rc)
    call ESMF_GridCompDestroy(ocn, rc=rc)
    call ESMF_GridCompDestroy(ice, rc=rc)

    CALL ESMF_Finalize(rc=rc)
 
end program
