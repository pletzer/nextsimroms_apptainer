import numpy
import netCDF4
import defopt

def main(filename: str='grid.nc', nx: int=2, ny: int=3):
    """
    :param filename: netcdf file name
    :param nx: number of zonal cells
    :param ny: number of meridional cells
    """
    rootgrp = netCDF4.Dataset(filename, 'w')
    
    # dimensions
    y_lmdz = rootgrp.createDimension('y_lmdz', ny)
    x_lmdz = rootgrp.createDimension('x_lmdz', nx)
    crn_lmdz = rootgrp.createDimension('crn_lmdz', 4)
    
    # coordinates and fields
    lon = rootgrp.createVariable('lon', 'f8', ('y_lmdz', 'x_lmdz'))
    lat = rootgrp.createVariable('lat', 'f8', ('y_lmdz', 'x_lmdz'))
    srf = rootgrp.createVariable('srf', 'f8', ('y_lmdz', 'x_lmdz'))
    imask = rootgrp.createVariable('imask', 'int', ('y_lmdz', 'x_lmdz'))
    clo = rootgrp.createVariable('clo', 'f8', ('crn_lmdz', 'y_lmdz', 'x_lmdz'))
    cla = rootgrp.createVariable('cla', 'f8', ('crn_lmdz', 'y_lmdz', 'x_lmdz'))
    
    # write the field values
    dlon = 360. / nx
    dlat = 180. / ny
    xc = numpy.linspace(-180. + dlon/2, 180. - dlon/2, nx)
    yc = numpy.linspace(-90. + dlat/2, 90. - dlat/2, ny)
    xb = numpy.linspace(-180., 180., nx + 1)
    yb = numpy.linspace(-90., 90., ny + 1)
    
    for j in range(ny):
        lon[j, :] = xc
        clo[0, j, :] = xb[:-1]
        clo[1, j, :] = xb[1:]
        clo[2, j, :] = xb[1:]
        clo[3, j, :] = xb[:-1]
       
    lat2d = yc.reshape((ny, 1)) @ numpy.ones((nx,), numpy.float64).reshape((1, nx)) 
    for i in range(nx):
        lat[:, i] = yc
        cla[0, :, i] = yb[:-1]
        cla[1, :, i] = yb[:-1]
        cla[2, :, i] = yb[1:]
        cla[3, :, i] = yb[1:]
    
    imask[...] = 0 # no masking
    
    a_earth = 6.378e6 
    srf[:] = a_earth**2 * numpy.cos( lat2d * numpy.pi/180) * dlon * dlat 
    
    rootgrp.close()
    
if __name__ == '__main__':
    defopt.run(main)
    
    

