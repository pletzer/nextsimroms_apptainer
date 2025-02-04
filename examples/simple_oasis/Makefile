#
include $(COUPLE)/util/make_dir/make.inc
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
OBJ_M1 =  read_grid.o def_parallel_decomposition.o tovtk.o
OBJ_M2 =  read_grid.o def_parallel_decomposition_m2.o tovtk.o
#-------------------------------------------------------------------------------
# General rules
#-------------------------------------------------------------------------------
#
default: all
#
all: ocean atmos 
#
#
#-------------------------------------------------------------------------------
# Rules for executables
#-------------------------------------------------------------------------------
#
ocean: $(OBJ_M1) tovtk.o ocean.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ $(OBJ_M1) ocean.o $(LIBPSMILE) $(FLIBS) $(NETCDF_LIBRARY)
atmos: $(OBJ_M2) tovtk.o atmos.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ $(OBJ_M2) atmos.o $(LIBPSMILE) $(FLIBS) $(NETCDF_LIBRARY)
#
#-------------------------------------------------------------------------------
# Rules for compilation
#-------------------------------------------------------------------------------
#
tovtk.o : tovtk.F90
		 $(F90) $(F90FLAGS) $(CPPLOCAL_M1) -c $<

read_grid.o :		read_grid.F90
				$(F90) $(F90FLAGS) -c read_grid.F90
def_parallel_decomposition.o : 		def_parallel_decomposition.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M1) -c def_parallel_decomposition.F90
def_parallel_decomposition_m2.o : 	def_parallel_decomposition.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M2) -o def_parallel_decomposition_m2.o -c def_parallel_decomposition.F90
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
	-rm -f *.o *.mod *.MOD ocean atmos
	-rm -f i.*.F90 *.L
	-rm -f core core.* 
#
# Make sure to recompile everything
#
realclean: clean remove
remove:
	(cd ../../util/make_dir ; $(MAKE) realclean -f TopMakefileOasis3)
