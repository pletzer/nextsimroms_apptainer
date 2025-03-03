program test_tovtk
    use tovtk_mod
    implicit none

    real(8), allocatable :: lon(:, :, :), lat(:, :, :), var(:, :)
    real(8) :: lonmin, lonmax, latmin, latmax, lonmid, latmid, dlon, dlat
    integer :: i, j, k, nx, ny

    nx = 2
    ny = 3
    allocate(lon(nx, ny, 4), lat(nx, ny, 4), var(nx, ny))

    lonmin = 0
    lonmax = nx
    latmin = 0
    latmax = ny
    dlon = (lonmax - lonmin)/real(nx, 8)
    dlat = (latmax - latmin)/real(ny, 8)

    do j = 1, size(lon, 2)
        do i = 1, size(lon, 1)
        
            lon(i, j, 1) = lonmin + (i-1)*dlon
            lat(i, j, 1) = latmin + (j-1)*dlat
            
            lon(i, j, 2) = lon(i, j, 1) + dlon
            lat(i, j, 2) = lat(i, j, 1)

            lon(i, j, 3) = lon(i, j, 2)
            lat(i, j, 3) = lat(i, j, 2) + dlat

            lon(i, j, 4) = lon(i, j, 1)
            lat(i, j, 4) = lat(i, j, 3)

            lonmid = 0.25*sum(lon(i, j, :))
            latmid = 0.25*sum(lon(i, j, :))

            var(i, j) = lonmid
        enddo
    enddo

    call vtk_write_data(lon, lat, var, 'myvar', 'myvar.vtk')

end program
    