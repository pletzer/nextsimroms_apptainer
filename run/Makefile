# Run the make command inside the apptainer container
include /usr/local/ifort/build/oasis3-mct/util/make_dir/make.inc
FLAGS=$(F90FLAGS) -L$(ARCHDIR)/lib

OASIS_DIR=/usr/local/ifort/oasis3-mct/opt/
NETCDF_LIBRARY = -L/usr/local/ifort/lib -lnetcdff -lnetcdf
LIBS = -L$(OASIS_DIR)/lib -lpsmile.MPI1 -lscrip -lmct -lmpeu $(NETCDF_LIBRARY)

# nextsim is not threadsafe
export OMP_NUM_THREADS=1
# Intel MPI on Mahuika requires this
export I_MPI_FABRICS=ofi

# neXtSIM
# this requires one bind the /data and /mesh directories whan launching apptainer shell
export NEXTSIM_MESH_DIR=${NEXTSIM_DATA_DIR}
export NEXTSIM_EXEC=/usr/local/ifort/build/oasis/nextsim/model/bin/nextsim.exec

all: ocean 

%.o: %.F90
	${F90} -check all -g -O2 -traceback -I/usr/local/ifort/include -I $(OASIS_DIR)/include -c $<

ocean.o: generic_component_mod.o

generic_component_mod.o: exception_mod.o

ocean: ocean.F90 grid_mod.o  generic_component_mod.o exception_mod.o tovtk_mod.o
	${F90} ocean.F90 ${FLAGS} -o ocean grid_mod.o generic_component_mod.o tovtk_mod.o exception_mod.o $(LIBS)

clean:
	rm -f ocean *.o *.mod
	rm -f *~
	rm -fr work
