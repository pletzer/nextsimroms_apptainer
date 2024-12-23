#
COUPLE := /usr/local/build/oasis3-mct
include  $(COUPLE)/util/make_dir/make.inc
ARCHDIR :=  /usr/local/oasis3-mct
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libscrip.a
#
default: all
#
all: ice ocn 
#
run: ice ocn
	mpiexec --verbose -n 1 ./ice : -n 1 ./ocn
#
#-------------------------------------------------------------------------------
# Rules for executables
#-------------------------------------------------------------------------------
#
ice: ice.o
	$(LD) $(LDFLAGS) -o $@ $< $(LIBPSMILE) $(FLIBS) $(NETCDF_LIBRARY)

ocn: ocn.o
	$(LD) $(LDFLAGS) -o $@ $< $(LIBPSMILE) $(FLIBS) $(NETCDF_LIBRARY)

%.o: %.F90
	$(F90) $(F90FLAGS) -fbacktrace -fcheck=bounds -g -O0 -c $<

run:
	mpiexec -n 1 ./ice : -n 1 ./ocn
#
# Clean directory
#
clean:
	-rm -f *.o *.mod ice ocn
