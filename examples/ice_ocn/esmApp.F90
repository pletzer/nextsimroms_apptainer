!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

program esmApp

  !-----------------------------------------------------------------------------
  ! Generic ESM application driver
  !-----------------------------------------------------------------------------

  use ESMF
  use ESM, only: esmSS => SetServices

  implicit none

  integer                 :: rc, urc
  type(ESMF_GridComp)     :: esmComp

  ! Initialize ESMF
  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, rc=rc)

  call ESMF_LogWrite("esmApp STARTING", ESMF_LOGMSG_INFO, rc=rc)

  ! Create the earth system Component
  esmComp = ESMF_GridCompCreate(name="esm", rc=rc)

  ! SetServices for the earth system Component
  call ESMF_GridCompSetServices(esmComp, esmSS, userRc=urc, rc=rc)

  ! Call Initialize for the earth system Component
  call ESMF_GridCompInitialize(esmComp, userRc=urc, rc=rc)

  ! Call Run  for earth the system Component
  call ESMF_GridCompRun(esmComp, userRc=urc, rc=rc)

  ! Call Finalize for the earth system Component
  call ESMF_GridCompFinalize(esmComp, userRc=urc, rc=rc)

  ! Destroy the earth system Component
  call ESMF_GridCompDestroy(esmComp, rc=rc)

  call ESMF_LogWrite("esmApp FINISHED", ESMF_LOGMSG_INFO, rc=rc)

  ! Finalize ESMF
  call ESMF_Finalize()

end program
