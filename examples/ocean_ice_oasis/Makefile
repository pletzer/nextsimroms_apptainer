#
include $(COUPLE)/util/make_dir/make.inc
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libscrip.a
#
##### User configurable options #####
#
# CPP keys for type of decomposition for ocean (_M1) and for ice (_M2)
# DECOMP_APPLE for 1D decomposition, DECOMP_BOX for 2D decomposition
CPPKEYDECOMP_M1=DECOMP_APPLE
CPPKEYDECOMP_M2=DECOMP_APPLE
#
### End User configurable options ###
#
CPPLOCAL_M1 =  -D${CPPKEYDECOMP_M1}
CPPLOCAL_M2 = -D${CPPKEYDECOMP_M2}
#
OBJ_M1 =  read_grid.o tovtk.o def_parallel_decomposition.o
OBJ_M2 =  read_grid.o tovtk.o def_parallel_decomposition_m2.o
#-------------------------------------------------------------------------------
# General rules
#-------------------------------------------------------------------------------
#
default: all
#
all: ocean ice
#
#
#-------------------------------------------------------------------------------
# Rules for executables
#-------------------------------------------------------------------------------
#
ocean: $(OBJ_M1) ocean.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ $(OBJ_M1) ocean.o $(LIBPSMILE) $(FLIBS) $(NETCDF_LIBRARY)
ice: $(OBJ_M2) ice.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ $(OBJ_M2) ice.o $(LIBPSMILE) $(FLIBS) $(NETCDF_LIBRARY)

test_tovtk: test_tovtk.o tovtk.o
	$(LD) $(LDFLAGS) -o $@ test_tovtk.o tovtk.o
#
#-------------------------------------------------------------------------------
# Rules for compilation
#-------------------------------------------------------------------------------
#
%.o : %.F90
		$(F90) $(F90FLAGS) -c $<

def_parallel_decomposition.o : 		def_parallel_decomposition.F90
		$(F90) $(F90FLAGS) $(CPPLOCAL_M1) -c $<
def_parallel_decomposition_m2.o : 	def_parallel_decomposition.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M2) -o def_parallel_decomposition_m2.o -c $<

ocean.o :	ocean.F90 tovtk.o
		$(F90) $(F90FLAGS) $(CPPLOCAL_M1) -c $<
ice.o :	ice.F90 Makefile tovtk.o
		$(F90) $(F90FLAGS) $(CPPLOCAL_M2) -c $<
#
#
# Clean directory
#
clean:
	-rm -f *.o *.mod *.MOD ocean atmos
	-rm -f i.*.F90 *.L
	-rm -f core core.* 
