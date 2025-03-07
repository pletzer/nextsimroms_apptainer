#include ../../../../util/make_dir/make.inc
include /usr/local/ifort/build/oasis3-mct/util/make_dir/make.inc
FLAGS=$(F90FLAGS) -L$(ARCHDIR)/lib

OASIS_DIR=/usr/local/ifort/oasis3-mct/debug/
NETCDF_LIBRARY = -L/usr/local/ifort/lib -lnetcdff -lnetcdf
LIBS = -L$(OASIS_DIR)/lib -lpsmile.MPI1 -lscrip -lmct -lmpeu $(NETCDF_LIBRARY)

all: ocean ice generic_component

%.o: %.F90
	${F90} -check all -g -O0 -traceback -I $(OASIS_DIR)/include -c $<

generic_component: generic_component.o grid_mod.o generic_component_mod.o exception_mod.o
	${F90} $< -g -O0 -o $@ grid_mod.o generic_component_mod.o exception_mod.o $(LIBS)


ocean: ocean.F90 grid_mod.o  generic_component_mod.o exception_mod.o
	${F90} ocean.F90 ${FLAGS} -o ocean grid_mod.o generic_component_mod.o  exception_mod.o $(LIBS)

ice: ice.F90  grid_mod.o generic_component_mod.o exception_mod.o
	${F90} ice.F90 ${FLAGS} -o ice grid_mod.o generic_component_mod.o  exception_mod.o $(LIBS)

runoi:
	rm -f grids.nc namcouple
	ln -s ../common_data/grids.nc .
	ln -s oi_data/namcouple .
	rm -f nout.?????? debug*
	mpiexec -n 4 ocean : -n 1 ice

rungc:
	rm -f grids.nc namcouple
	ln -s ../common_data/grids.nc .
	ln -s gc_data/namcouple .
	rm -f nout.?????? debug*
	mpiexec -n 1 generic_component gc_data/ocn.nml : -n 1 generic_component gc_data/ice.nml

rungc2x2:
	rm -f grids.nc namcouple
	ln -s ../common_data/grids.nc .
	ln -s gc_data/namcouple .
	rm -f nout.?????? debug*
	mpiexec -n 2 generic_component gc_data/ocn.nml : -n 2 generic_component gc_data/ice.nml

clean:
	rm -f ocean ice generic_component
	rm -f *~
	rm -fr work
