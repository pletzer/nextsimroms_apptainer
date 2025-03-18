module generic_component_mod

    use mod_oasis
    implicit none
    integer, parameter :: STR_LEN = 16
    
    type generic_component_type
        real(8), allocatable :: temperature(:, :, :)
        real(8), allocatable :: new_temperature(:, :, :)
        real(8), allocatable :: top_temperature(:, :)
        real(8), allocatable :: bottom_temperature(:, :)
        real(8) :: dx
        real(8) :: dt
        real(8) :: kappa
        integer :: num_steps
    end type generic_component_type

    contains

    subroutine gc_new(self, namelist_file, ier)

        implicit none

        type(generic_component_type), intent(inout) :: self
        character(len=*), intent(in) :: namelist_file
        integer, intent(out) :: ier

        integer :: nx, ny, nz, num_steps, iu
        real(8) :: kappa, dx, dt, top_temperature, bottom_temperature
        
        namelist /model/ nx, ny, nz, num_steps, kappa, dt, dx, top_temperature, bottom_temperature

        dx = 0
        dt = 0
        kappa = 0.
        num_steps = 0
        nx = 1
        ny = 1
        nz = 1
        open(newunit=iu, file=namelist_file, status='old', iostat=ier); if (ier /= 0) return
        read(unit=iu, nml=model, iostat=ier); if (ier /= 0) return        
        close(iu)
    
        self % dx = dx
        self % dt = dt
        self % kappa = kappa
        self % num_steps = num_steps
        allocate(self % temperature(nx, ny, nz))
        allocate(self % new_temperature(nx, ny, nz))
        allocate(self % bottom_temperature(nx, ny))  
        allocate(self % top_temperature(nx, ny))
        self % bottom_temperature = bottom_temperature
        self % top_temperature = top_temperature 
            
    end subroutine gc_new

    subroutine gc_step(self, ier)
        implicit none
        type(generic_component_type), intent(inout) :: self
        integer, intent(out) :: ier

        integer :: i, j, k, ip, jp, kp, im, jm, km, nx, ny, nz
        real(8) :: coeff

        ier = 0
        
        coeff = self % kappa * self % dt / self % dx**2
        nx = size(self % temperature, 1)
        ny = size(self % temperature, 2)
        nz = size(self % temperature, 3)
    
        ! inner domain
        do k = 2, size(self % temperature, 3) - 1 ! note: we skip the top and bottom layers
            km = k - 1
            kp = k + 1
            do j = 1, size(self % temperature, 2)
                jm = j - 1
                if (jm <= 0) jm = jm + 1 ! periodic boundaries
                jp = j + 1
                if (jp > ny) jp = jp - ny  ! periodic boundaries
                do i = 1, size(self % temperature, 1)
                    im = i - 1
                    if (im <= 0) im = im + nx  ! periodic boundaries
                    ip = i + 1
                    if (ip > nx) ip = ip - nx  ! periodic boundaries

                    ! new field 
                    self % new_temperature(i, j, k) = self % temperature(i, j, k) + &
                    & coeff * ( & 
                    &   self % temperature(ip, j, k) + self % temperature(im, j, k) +  &
                    &   self % temperature(i, jp, k) + self % temperature(i, jm, k) +  &
                    &   self % temperature(i, j, kp) + self % temperature(i, j, km) +  &
                    &  - 6*self % temperature(i, j, k)                                 &
                        )

                enddo
            enddo
        enddo

        ! bottom and top. Field is set by the boundary conditions. Depending on the 
        ! component, either bottom or top field will not change and the other is 
        ! determined by the other component
        do j = 1, size(self % temperature, 2)
            jm = j - 1
            if (jm <= 0) jm = jm + 1
            jp = j + 1
            if (jp > ny) jp = jp - ny
            do i = 1, size(self % temperature, 1)
                im = i - 1
                if (im <= 0) im = im + nx
                ip = i + 1
                if (ip > nx) ip = ip - nx
                ! bottom
                self % new_temperature(i, j, 1) = self % bottom_temperature(i, j)
                ! top
                self % new_temperature(i, j, nz) = self % top_temperature(i, j)
            enddo
        enddo
    
        ! store the new solution across entire domain
        do k = 1, size(self% temperature, 3)
            do j = 1, size(self % temperature, 2)
                do i = 1, size(self % temperature, 1)
                    self % temperature(i, j, k) = self % new_temperature(i, j, k)
                enddo
            enddo
        enddo
                        
           
    end subroutine gc_step

    subroutine gc_del(self, ier)
        implicit none
        type(generic_component_type), intent(inout) :: self
        integer, intent(out) :: ier
        integer :: status
        ier = 0
        ! do not generate an error if deallocating an array that is not allocated
        deallocate(self % temperature, stat=status)
        deallocate(self % new_temperature, stat=status)
        deallocate(self % bottom_temperature, stat=status)
        deallocate(self % top_temperature, stat=status)
    end subroutine gc_del

end module generic_component_mod
    