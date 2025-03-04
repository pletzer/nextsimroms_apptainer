import numpy
import netCDF4
import defopt
import re

def get_coords(gridfile, lonname, latname):
    ds = netCDF4.Dataset(gridfile)
    lon = ds.variables[lonname][:]
    lat = ds.variables[latname][:]
    return lon, lat
             

def main(filename: str='fdatm.nc', *, gridfile: str, gridname: str, varname: str, lonname: str='lon', latname: str='lat'):
    """
    :param filename: netcdf file name
    :param gridfile: netcdf grid file, used to infer the dimensions
    :param gridname: grid name
    :param varname: variable name
    :param lonname: name of the longitude coordinate
    :param latname: name of the latitude coordinate
    """
    lon, lat = get_coords(gridfile, lonname, latname)
    dims = lon.shape
    
    rootgrp = netCDF4.Dataset(filename, 'w')
    
    # dimensions
    jDim = rootgrp.createDimension('j', dims[0])
    iDim = rootgrp.createDimension('i', dims[1])
    
    # coordinates and fields
    lonVar = rootgrp.createVariable(f'{gridname}.lon', 'f8', ('j', 'i'))
    latVar = rootgrp.createVariable(f'{gridname}.lat', 'f8', ('j', 'i'))
    var = rootgrp.createVariable(varname, 'f8', ('j', 'i'))
    
    # write the field
    lonVar[:] = lon
    latVar[:] = lat
    var[:] = 0.
    
    rootgrp.close()
    
if __name__ == '__main__':
    defopt.run(main)
    
    

