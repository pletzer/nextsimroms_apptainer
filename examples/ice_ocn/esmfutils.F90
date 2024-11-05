 module esmfutils_mod

    use ESMF
    use NUOPC
    use NUOPC_Model, only: NUOPC_ModelGet

contains

    subroutine esmfutils_getImportDataPtr(model, name, ptr, rc)

        implicit none
        type(ESMF_GridComp)  :: model
        character(len=*), intent(in) :: name
        real(8), pointer, intent(out) :: ptr(:, :)
        integer, intent(out) :: rc

        type(ESMF_State)        :: state
        type(ESMF_Field)        :: field
        type(ESMF_Array)        :: array
        integer :: rc2
    
        rc = ESMF_SUCCESS
    
        ! query for importState and exportState
        call NUOPC_ModelGet(model,  importState=state, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1

        ! retrieve the field
        call ESMF_StateGet(state, itemName=name, field=field, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1

        ! get the array
        call ESMF_FieldGet(field, array=array, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1
  
        ! get local pointer to the data. We only have one domain per PE
        call ESMF_ArrayGet(array, localDe=0, farrayPtr=ptr, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1
    
    end subroutine esmfutils_getImportDataPtr

 end module esmfutils_mod