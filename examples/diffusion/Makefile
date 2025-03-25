#include ../../../../util/make_dir/make.inc
include /usr/local/ifort/build/oasis3-mct/util/make_dir/make.inc
FLAGS=$(F90FLAGS) -L$(ARCHDIR)/lib

OASIS_DIR=/usr/local/ifort/oasis3-mct/debug/
NETCDF_LIBRARY = -L/usr/local/ifort/lib -lnetcdff -lnetcdf
LIBS = -L$(OASIS_DIR)/lib -lpsmile.MPI1 -lscrip -lmct -lmpeu $(NETCDF_LIBRARY)

all: ocean ice

%.o: %.F90
	${F90} -check all -g -O0 -traceback -I/usr/local/ifort/include -I $(OASIS_DIR)/include -c $<

ocean: ocean.F90 grid_mod.o  generic_component_mod.o exception_mod.o tovtk_mod.o
	${F90} ocean.F90 ${FLAGS} -o ocean grid_mod.o generic_component_mod.o tovtk_mod.o exception_mod.o $(LIBS)

ice: ice.F90  grid_mod.o generic_component_mod.o exception_mod.o tovtk_mod.o
	${F90} ice.F90 ${FLAGS} -o ice grid_mod.o generic_component_mod.o tovtk_mod.o exception_mod.o $(LIBS)

run:
	rm -f grids.nc namcouple
	ln -s ../common_data/grids.nc .
	ln -s oi_data/namcouple .
	rm -f nout.?????? debug*
	mpiexec -n 1 ocean : -n 1 ice

clean:
	rm -f ocean ice *.o *.mod
	rm -f debug* nout* *.nc *.vtk
	rm -f namcouple
	rm -f *~
	rm -fr work
