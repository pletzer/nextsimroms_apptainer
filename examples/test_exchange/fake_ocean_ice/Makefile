#include ../../../../util/make_dir/make.inc
include /usr/local/ifort/build/oasis3-mct/util/make_dir/make.inc
FLAGS=$(F90FLAGS) -L$(ARCHDIR)/lib

all: ocean ice

grid_mod.o: grid_mod.F90
	${F90} ${FLAGS} -c $<

ocean: ocean.F90 grid_mod.o
	${F90} ocean.F90 ${FLAGS} -o ocean grid_mod.o  -lpsmile.MPI1 -lscrip -lmct -lmpeu $(NETCDF_LIBRARY)

ice: ice.F90  grid_mod.o
	${F90} ice.F90 ${FLAGS} -o ice grid_mod.o -lpsmile.MPI1 -lscrip -lmct -lmpeu $(NETCDF_LIBRARY)

run:
	ln -s ../common_data/grids.nc .
	mpiexec -n 4 ocean : -n 1 ice

clean:
	rm -f ocean ice
	rm -f *~
	rm -fr work
