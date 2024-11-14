program dist_array
    use ESMF
    implicit none

    type(ESMF_VM) :: vm
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_FieldBundle) :: fieldBundle
    type(ESMF_Field) :: rho
    integer :: rc

    CALL ESMF_Initialize(vm=vm, defaultCalKind=ESMF_CALKIND_GREGORIAN, &
    & logkindflag=ESMF_LOGKIND_MULTI, rc=rc)

    ! create a 2x2 distributed grid
    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), &
        maxIndex=(/256,256/), &
        regDecomp=(/2,2/), &
        rc=rc)
    
    grid = ESMF_GridCreate(distgrid=distgrid, name="grid", rc=rc)

    ! create the field (2d)
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
    fieldBundle = ESMF_FieldBundleCreate(rc=rc)

    rho = ESMF_FieldCreate(grid, arrayspec, &
                totalLWidth=(/1, 1/), &
                totalUWidth=(/1, 1/), &
                staggerloc=ESMF_STAGGERLOC_CENTER, name="rho", &
                rc=rc)

    call ESMF_GridDestroy(grid)
    call ESMF_DistGridDestroy(distgrid)

    CALL ESMF_Finalize(rc=rc)



end program 