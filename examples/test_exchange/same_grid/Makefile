#include ../../../../util/make_dir/make.inc
include /usr/local/ifort/build/oasis3-mct/util/make_dir/make.inc
FLAGS=$(F90FLAGS) -L$(ARCHDIR)/lib

all: sender-apple receiver

grid_mod.o: grid_mod.F90
	${F90} ${FLAGS} -c $<

sender-apple: sender-apple.F90 grid_mod.o
	${F90} sender-apple.F90 ${FLAGS} -o sender-apple grid_mod.o  -lpsmile.MPI1 -lscrip -lmct -lmpeu $(NETCDF_LIBRARY)

receiver: receiver.F90  grid_mod.o
	${F90} receiver.F90 ${FLAGS} -o receiver grid_mod.o -lpsmile.MPI1 -lscrip -lmct -lmpeu $(NETCDF_LIBRARY)

clean:
	rm -f sender-apple receiver non_oasis
	rm -f *~
	rm -fr work
