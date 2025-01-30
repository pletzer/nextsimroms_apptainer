#
COUPLE := /usr/local/build/oasis3-mct
include  $(COUPLE)/util/make_dir/make.inc
ARCHDIR :=  /usr/local/oasis3-mct
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libscrip.a
#
##### User configurable options #####
#
# CPP keys for type of decomposition for ocean (_M1) and for atmos (_M2)
# DECOMP_APPLE for 1D decomposition, DECOMP_BOX for 2D decomposition
CPPKEYDECOMP_M1=DECOMP_APPLE
CPPKEYDECOMP_M2=DECOMP_APPLE
#
### End User configurable options ###
#
CPPLOCAL_M1 =  -D${CPPKEYDECOMP_M1}
CPPLOCAL_M2 = -D${CPPKEYDECOMP_M2}
#
#-------------------------------------------------------------------------------
# General rules
#-------------------------------------------------------------------------------
#
default: all
#
all: ocean atmos 
#
run: ocean atmos
	-make clean
	mpiexec -n 1 ./ocean : -n 1 ./atmos
#
#-------------------------------------------------------------------------------
# Rules for executables
#-------------------------------------------------------------------------------
#
ocean: ocean.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ ocean.o $(LIBPSMILE) $(FLIBS) $(NETCDF_LIBRARY)
atmos: atmos.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ atmos.o $(LIBPSMILE) $(FLIBS) $(NETCDF_LIBRARY)
#
#-------------------------------------------------------------------------------
# Rules for compilation
#-------------------------------------------------------------------------------
#
ocean.o :	ocean.F90 Makefile
		$(F90) $(F90FLAGS) $(CPPLOCAL_M1) -c ocean.F90
atmos.o :	atmos.F90 Makefile
		$(F90) $(F90FLAGS) $(CPPLOCAL_M2) -c atmos.F90
#
#-------------------------------------------------------------------------------
# Utilities
#-------------------------------------------------------------------------------
#
help:
	more Make.help
#
# Clean directory
#
clean:
	-rm -f *.o *.mod *.MOD
	-rm -f i.*.F90 *.L
	-rm -f core core.* 
	-rm -f debug.??.*
	-rm nout.*
