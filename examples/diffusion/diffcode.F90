module diff_mod

    implicit none

    type diff_type
        ! x grid
        real(8), allocatable :: x(:)
        ! temperature 
        real(8), allocatable :: temperature(:)
        real(8) :: dx
    end type diff_type

contains 
 
    subroutine diff_init(self, xmin, xmax, nx)
        implicit none
        type(diff_type), intent(inout) :: self
        real(8), intent(in) :: xmin, xmax
        integer, intent(in) :: nx
        integer :: nx1, i
        real(8) :: dx

        nx1 = nx + 1
        dx = (xmax - xmin) / real(nx, 8)
        allocate(self%x(nx1))
        allocate(self%temperature(nx1))
        do i = 1, nx1
            self%x(i) = xmin + (i - 1)*dx
            self%temperature(i) = 0
        enddo
        self%dx = dx
        ! initial condition
        self%temperature(1) = 1
    end subroutine diff_init

    subroutine diff_del(self)
        implicit none
        type(diff_type), intent(inout) :: self

        deallocate(self%temperature)
        deallocate(self%x)
    end subroutine diff_del

    subroutine diff_step(self, dt, diff_coeff)
        implicit none
        type(diff_type), intent(inout) :: self
        real(8), intent(in) :: dt, diff_coeff
        integer :: nx1, i, nx

        nx1 = size(self%x)
        do i = 1, nx
            self%temperature(i) = self%temperature(i) + &
               & dt*diff_coeff*(self%temperature(i-1) - 2*self%temperature(i) + self%temperature(i+1)) &
               & / self%dx**2
        enddo

    end subroutine diff_step



end module diff_mod