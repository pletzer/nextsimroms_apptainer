cmake_minimum_required (VERSION 3.1)
project(diffusion)

enable_language(Fortran)
enable_testing()

set(ESMFMAKEFILE "" CACHE STRING "Path to esmf.mk")

# create a shared library
add_library(difflib SHARED diffcode.F90)