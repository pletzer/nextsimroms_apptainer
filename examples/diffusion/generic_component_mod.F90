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

    subroutine gc_new(self, namelist_file, is_ocean, ier)

        implicit none

        type(generic_component_type), intent(inout) :: self
        character(len=*), intent(in) :: namelist_file
        logical, intent(in) :: is_ocean
        integer, intent(out) :: ier

        integer :: nx, ny, nz, num_steps, iu
        real(8) :: kappa, dx, dt

        integer :: nx1, ny1, nz1
        
        namelist /model/ nx, ny, num_steps, dt, dx
        namelist /ice/ kappa, nz
        namelist /ocean/ kappa, nz

        dx = 0
        dt = 0
        kappa = 0.
        num_steps = 0
        nx = 1
        ny = 1
        nz = 1
        open(newunit=iu, file=namelist_file, status='old', iostat=ier); if (ier /= 0) return
        read(unit=iu, nml=model, iostat=ier); if (ier /= 0) return
        if (is_ocean) then
            read(unit=iu, nml=ocean, iostat=ier); if (ier /= 0) return
        else
            read(unit=iu, nml=ice, iostat=ier); if (ier /= 0) return
        end if        
        close(iu)

        nx1 = nx + 1
        ny1 = ny + 1
        nz1 = nz + 1
    
        self % dx = dx
        self % dt = dt
        self % kappa = kappa
        self % num_steps = num_steps
        allocate(self % temperature(nx1, ny1, nz1))
        allocate(self % new_temperature(nx1, ny1, nz1))
        allocate(self % bottom_temperature(nx1, ny1))  
        allocate(self % top_temperature(nx1, ny1))
        ! initialize
        self % bottom_temperature = 0
        self % top_temperature = 0
            
    end subroutine gc_new

    subroutine gc_step(self, ier)
        implicit none
        type(generic_component_type), intent(inout) :: self
        integer, intent(out) :: ier

        integer :: i, j, k, ip, jp, kp, im, jm, km, nx1, ny1, nz1, nx, ny
        real(8) :: coeff

        ier = 0
        
        coeff = self % kappa * self % dt / self % dx**2
    
        nx1 = size(self % temperature, 1)
        ny1 = size(self % temperature, 2)
        nz1 = size(self % temperature, 3)
        nx = nx1 - 1
        ny = ny1 - 1
    
        ! interior domain
        do k = 2, nz1 - 1 ! note: we skip the top and bottom layers
            km = k - 1
            kp = k + 1
            do j = 1, ny1
                jm = j - 1
                if (jm <= 0) jm = jm + ny ! periodic boundaries
                jp = j + 1
                if (jp > ny1) jp = jp - ny  ! periodic boundaries
                do i = 1, nx1
                    im = i - 1
                    if (im <= 0) im = im + nx  ! periodic boundaries
                    ip = i + 1
                    if (ip > nx1) ip = ip - nx  ! periodic boundaries

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
        do j = 1, ny1
            jm = j - 1
            if (jm <= 0) jm = jm + ny
            jp = j + 1
            if (jp > ny1) jp = jp - ny
            do i = 1, nx1
                im = i - 1
                if (im <= 0) im = im + nx
                ip = i + 1
                if (ip > nx) ip = ip - nx

                ! bottom
                k = 1
                kp = k + 1
                self % new_temperature(i, j, k) = self % temperature(i, j, k) + &
                & coeff * ( & 
                &   self % temperature(ip, j, k) + self % temperature(im, j, k) +  &
                &   self % temperature(i, jp, k) + self % temperature(i, jm, k) +  &
                &   self % temperature(i, j, kp) + self % bottom_temperature(i, j) +  &
                &  - 6*self % temperature(i, j, k)                                 &
                    )

                ! top
                k = nz1
                km = k - 1
                self % new_temperature(i, j, k) = self % temperature(i, j, k) + &
                & coeff * ( & 
                &   self % temperature(ip, j, k) + self % temperature(im, j, k) +  &
                &   self % temperature(i, jp, k) + self % temperature(i, jm, k) +  &
                &   self % top_temperature(i, j) + self % temperature(i, j, km) +  &
                &  - 6*self % temperature(i, j, k)                                 &
                    )

            enddo
        enddo
    
        ! store the new solution
        do k = 1, nz1
            do j = 1, ny1
                do i = 1, nx1
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
    