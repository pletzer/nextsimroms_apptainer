cmake_minimum_required (VERSION 3.1)
project(diffusion)

enable_language(Fortran)
enable_testing()

set(ESMF_LIBDIR "/home/pletzera/esmf/esmf-8.7.0/lib/libO/Linux.gfortran.64.openmpi.default/" CACHE STRING "Path to esmf library directory")
set(ESMF_MODDIR "/home/pletzera/esmf/esmf-8.7.0/mod/modO/Linux.gfortran.64.openmpi.default/" CACHE STRING "Path to esmf module directory")

include_directories(${ESMF_MODDIR})
link_directories(${ESMF_LIBDIR})

# create a shared library
add_executable(mainApp diffcode.F90 mainApp.F90)
target_link_libraries(mainApp esmf)
