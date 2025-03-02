program dist_array
    use ESMF
    implicit none

    type(ESMF_VM) :: vm
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_FieldBundle) :: fieldBundle
    type(ESMF_Field) :: rho
    character(len=256) :: msg
    integer :: rc, localPet, i, j
    integer :: localMinIndex(2), localMaxIndex(2), minIndex(2), maxIndex(2), &
        excllb(2), exclub(2), complb(2), compub(2), totlb(2), totub(2)
    real(8), pointer :: rho_data(:, :)

    CALL ESMF_Initialize(vm=vm, defaultCalKind=ESMF_CALKIND_GREGORIAN, &
    & logkindflag=ESMF_LOGKIND_MULTI, rc=rc)

    call ESMF_VMGet(vm, localPet=localPet, rc=rc)

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
    
    call ESMF_FieldBundleAdd(fieldBundle, (/rho/), rc=rc)

    ! get info about the field
    call ESMF_FieldGet(rho, &
         minIndex=minIndex, maxIndex=maxIndex, &
         localMinIndex=localMinIndex, localMaxIndex=localMaxIndex, &
         rc=rc)
    write(msg, *) '[', localPet, '] inds       min: ', minIndex, ' max: ', maxIndex
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    write(msg, *) '[', localPet, '] inds local min: ', localMinIndex, ' max: ', localMaxIndex
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_FieldGetBounds(rho, &
          exclusiveLBound=excllb, exclusiveUBound=exclub, &
          computationalLBound=complb, computationalUBound=compub, &
          totalLBound=totlb, totalUBound=totub, &
          rc=rc)
    write(msg, *) '[', localPet, '] inds excls min: ', excllb, ' max: ', exclub
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    write(msg, *) '[', localPet, '] inds compt min: ', complb, ' max: ', compub
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    write(msg, *) '[', localPet, '] inds total min: ', totlb, ' max: ', totub
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)

    ! clean up
    call ESMF_GridDestroy(grid)
    call ESMF_DistGridDestroy(distgrid)

    CALL ESMF_Finalize(rc=rc)



end program 