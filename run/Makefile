include /usr/local/ifort/build/oasis3-mct/util/make_dir/make.inc
FLAGS=$(F90FLAGS) -L$(ARCHDIR)/lib

OASIS_DIR=/usr/local/ifort/oasis3-mct/opt/
NETCDF_LIBRARY = -L/usr/local/ifort/lib -lnetcdff -lnetcdf
LIBS = -L$(OASIS_DIR)/lib -lpsmile.MPI1 -lscrip -lmct -lmpeu $(NETCDF_LIBRARY)

all: ocean 

%.o: %.F90
	${F90} -check all -g -O2 -traceback -I/usr/local/ifort/include -I $(OASIS_DIR)/include -c $<


generic_component_mod.o: exception_mod.o

ocean: ocean.F90 grid_mod.o  generic_component_mod.o exception_mod.o tovtk_mod.o
	${F90} ocean.F90 ${FLAGS} -o ocean grid_mod.o generic_component_mod.o tovtk_mod.o exception_mod.o $(LIBS)

run:
	rm -f grids.nc namcouple
	ln -s ../common_data/grids.nc .
	ln -s input/namcouple .
	rm -f nout.?????? debug*
#	mpiexec -n 4 ocean : -n 2 


clean:
	rm -f ocean *.o *.mod
	rm -f *~
	rm -fr work
