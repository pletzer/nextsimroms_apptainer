module exception_mod
   contains
   subroutine check_err(kinfo, comp_id, comp_name, filename, line)
      use mod_oasis
      implicit none
      integer, intent(in) :: kinfo, comp_id
      character(len=*), intent(in) :: comp_name
      character(len=*), intent(in) :: filename
      integer, intent(in) :: line
      if (kinfo /= 0) then
         write(0, *), 'ERROR in file ', filename, ' at line ', line
         call oasis_abort(comp_id, comp_name, &
         & "OASIS error: ", rcode=kinfo)
      endif
   end subroutine check_err
end module exception_mod
