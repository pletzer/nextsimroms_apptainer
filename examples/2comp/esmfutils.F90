 module esmfutils_mod

    use ESMF
    use NUOPC
    use NUOPC_Model, only: NUOPC_ModelGet

contains

    function esmfutils_int_to_string_with_zeros(num, n) result(str)
        implicit none
        integer, intent(in) :: num
        integer, intent(in) :: n
        character(len=n) :: str
        character(len=32) :: temp_str

        ! Convert the integer to a string
        write(temp_str, '(I0)') num

        ! Check the length of the converted string
        if (len_trim(temp_str) > n) then
            ! If the number is too large to fit, truncate it
            str = temp_str(len_trim(temp_str)-n+1:len_trim(temp_str))
        else
            ! Otherwise, prepend zeros
            str = repeat('0', n - len_trim(temp_str)) // temp_str(1:len_trim(temp_str))
        end if
    end function esmfutils_int_to_string_with_zeros

    subroutine esmfutils_getDataPtrFromState(state, name, ptr, rc)

        implicit none
        type(ESMF_State), intent(in) :: state
        character(len=*), intent(in) :: name
        real(8), pointer, intent(out) :: ptr(:, :)
        integer, intent(out) :: rc

        type(ESMF_Field)        :: field
        type(ESMF_Array)        :: array
        integer :: rc2
    
        rc = ESMF_SUCCESS
    
        ! retrieve the field
        call ESMF_StateGet(state, itemName=name, field=field, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1

        ! get the array
        call ESMF_FieldGet(field, array=array, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1
  
        ! get local pointer to the data. We only have one domain per PE
        call ESMF_ArrayGet(array, localDe=0, farrayPtr=ptr, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1

    end subroutine esmfutils_getDataPtrFromState

    subroutine esmfutils_getExportDataPtr(model, name, ptr, rc)

        implicit none
        type(ESMF_GridComp)  :: model
        character(len=*), intent(in) :: name
        real(8), pointer, intent(out) :: ptr(:, :)
        integer, intent(out) :: rc

        type(ESMF_State)        :: state
        integer :: rc2
    
        rc = ESMF_SUCCESS
    
        ! query for state
        call NUOPC_ModelGet(model,  exportState=state, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1

        call esmfutils_getDataPtrFromState(state, name, ptr, rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1
    
    end subroutine esmfutils_getExportDataPtr

    subroutine esmfutils_getDataPtr(model, import, name, ptr, rc)

        implicit none
        type(ESMF_GridComp)  :: model
        logical              :: import ! true if an import field
        character(len=*), intent(in) :: name
        real(8), pointer, intent(out) :: ptr(:, :)
        integer, intent(out) :: rc

        if (import) then
            call esmfutils_getImportDataPtr(model, name, ptr, rc)
        else
            call esmfutils_getExportDataPtr(model, name, ptr, rc)
        endif

    end subroutine esmfutils_getDataPtr

    subroutine esmfutils_getImportDataPtr(model, name, ptr, rc)

        implicit none
        type(ESMF_GridComp)  :: model
        character(len=*), intent(in) :: name
        real(8), pointer, intent(out) :: ptr(:, :)
        integer, intent(out) :: rc

        type(ESMF_State)        :: state
        integer :: rc2
    
        rc = ESMF_SUCCESS
    
        ! query for state
        call NUOPC_ModelGet(model,  importState=state, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1

        call esmfutils_getDataPtrFromState(state, name, ptr, rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1
    
    end subroutine esmfutils_getImportDataPtr

    subroutine esmfutils_getAreaIntegratedField(model, import, name, res, rc)

        type(ESMF_GridComp), intent(in) :: model
        logical, intent(in) :: import ! true for an import field
        character(len=*), intent(in) :: name
        real(8), intent(out) :: res
        integer, intent(out) :: rc

        integer :: rc2, i, j
        real(8), pointer :: xPtr(:), yPtr(:), dataPtr(:, :)
        real(8) :: area
        type(ESMF_State) :: state
        type(ESMF_Field) :: field
        type(ESMF_Grid)  :: grid
        type(ESMF_Array) :: array

        rc = ESMF_SUCCESS

        if (import) then
            call NUOPC_ModelGet(model,  importState=state, rc=rc2)
        else
            call NUOPC_ModelGet(model,  exportState=state, rc=rc2)
        endif
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1

        ! retrieve the field
        call ESMF_StateGet(state, itemName=name, field=field, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1

        ! get the array
        call ESMF_FieldGet(field, array=array, grid=grid, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1
  
        ! get local pointer to the data. We only have one domain per PE
        call ESMF_ArrayGet(array, localDe=0, farrayPtr=dataPtr, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1

        ! x coordinate
        call ESMF_GridGetCoord(grid, &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            coordDim=1, farrayPtr=xPtr, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1

        ! y coordinate
        call ESMF_GridGetCoord(grid, &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            coordDim=2, farrayPtr=yPtr, rc=rc2)
        if (rc2 /= ESMF_SUCCESS) rc = rc + 1
        
        res = 0
        do j = 1, size(yPtr) - 1
            do i = 1, size(xPtr) - 1
                area = (xPtr(i + 1) - xPtr(i)) * (yPtr(j + 1) - yPtr(j))
                ! assume cell centred
                res = res + area*dataPtr(i, j)
            enddo
        enddo

    end subroutine esmfutils_getAreaIntegratedField

    subroutine esmfutils_write2DStructFieldVTK(field, filename)

        type(ESMF_Field) :: field
        character(len=*) :: filename

        integer :: iu, rc, i, j, numPoints, numCells
        type(ESMF_Grid) :: grid
        type(ESMF_StaggerLoc) :: staggerloc
        integer :: minIndex(2), maxIndex(2)
        real(ESMF_KIND_R8), pointer :: xPtr(:), yPtr(:), dataPtr(:, :)
        character(len=128) :: fieldName
        type(ESMF_Array)        :: array

        call ESMF_FieldGet(field, &
         & grid=grid, staggerloc=staggerloc, &
         & minIndex=minIndex, maxIndex=maxIndex, &
         & name=fieldName, &
         & rc=rc)

        call ESMF_GridGetCoord(grid, &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            coordDim=1, farrayPtr=xPtr)
        call ESMF_GridGetCoord(grid, &
            staggerloc=ESMF_STAGGERLOC_CORNER, &
            coordDim=2, farrayPtr=yPtr)

        numPoints = size(xPtr) * size(yPtr)
        numCells = (size(xPtr) - 1) * (size(yPtr) - 1)

        iu = 10
        open(unit=iu, file=filename)
        write(iu, '(A)') '# vtk DataFile Version 3.0'
        write(iu, '(A)') 'File generated by esmfutils'
        write(iu, '(A)') 'ASCII'
        write(iu, '(A)') 'DATASET STRUCTURED_GRID'
        write(iu, '(A, I6, I6, I6)') 'DIMENSIONS ', size(xPtr), size(yPtr), 1
        write(iu, '(A, I6, A)') 'POINTS ', numPoints, ' double'
        do j = 1, size(yPtr)
            do i = 1, size(xPtr)
            write(iu, '(2E20.8, A)') &
                 xPtr(i), yPtr(j), ' 0.E0'
            enddo
        enddo

        call ESMF_FieldGet(field, array=array, rc=rc)
        call ESMF_ArrayGet(array, localDe=0, farrayPtr=dataPtr, rc=rc)

        if (staggerloc == ESMF_STAGGERLOC_CENTER) then
            write(iu, '(A, I6)') 'CELL_DATA', numCells
        else
            write(iu, '(A, I6)') 'POINT_DATA', numPoints
        endif
        write(iu, '(A, A, A)') 'SCALARS ', trim(fieldName), ' double 1'
        write(iu, '(A)') 'LOOKUP_TABLE default'
        do j = lbound(dataPtr, 2), ubound(dataPtr, 2)
            do i = lbound(dataPtr, 1), ubound(dataPtr, 1)
                write(iu, '(E20.8)') dataPtr(i, j)
            enddo
        enddo

        close(unit=iu)

    end subroutine esmfutils_write2DStructFieldVTK

 end module esmfutils_mod