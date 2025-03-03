import numpy
import netCDF4
import defopt
import matplotlib.pyplot as plt

def main(filename: str='ice_mesh.nc'):
    """
    :param filename: netcdf file name
    """
    rootgrp = netCDF4.Dataset(filename, 'r')
    
    clo = rootgrp.variables['clo'][:]
    cla = rootgrp.variables['cla'][:]
    
    rootgrp.close()
    
    ny, nx = clo.shape[:2]
    nx1, ny1 = nx + 1, ny + 1
    vert_lon = numpy.empty((ny1, nx1), float)
    vert_lat = numpy.empty((ny1, nx1), float)
    
    for j in range(ny):
        for i in range(nx):
            vert_lon[j+0, i+0] = clo[j, i, 0]
            vert_lon[j+0, i+1] = clo[j, i, 1]
            vert_lon[j+1, i+1] = clo[j, i, 2]
            vert_lon[j+1, i+0] = clo[j, i, 3]
            
            vert_lat[j+0, i+0] = cla[j, i, 0]
            vert_lat[j+0, i+1] = cla[j, i, 1]
            vert_lat[j+1, i+1] = cla[j, i, 2]
            vert_lat[j+1, i+0] = cla[j, i, 3]
    
    # # store the lower left corner
    # vert_lon[:-1, :-1] = clo[:, :, 0]
    # vert_lon[:-1, -1] = clo[:, -1, 1]
    # vert_lon[-1, -1] = clo[-1, -1, 2]
    # vert_lon[-1, :-1] = clo[-1, :-1, 3]
    
    plt.pcolormesh(vert_lon, vert_lat, vert_lon, edgecolors='black')
    plt.show()
    
if __name__ == '__main__':
    defopt.run(main)
