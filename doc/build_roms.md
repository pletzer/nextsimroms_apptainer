= Building ROMS with using a container environment =

== Launch the container ==

You must have `apptainer` or `singularity` installed
```
apptainer shell netcdf-dev.sif
```
Notes:
 * Replace `apptainer` with `singularity` if you have Singularity installed
 * On mahuika, type `ml purge; ml Apptainer` to access the apptainer command

== Requirements ==

Check that the following commands are installed in the container
 * git
 * wget
 * gfortran
 * mpif90
 * make
 * perl
 * nc-config
 * mpiexec


== Compile ROMS ==

Assume ROMS is in directory `$ROMS_SRC_DIR`:
```
cd $ROMS_SRC_DIR
mv makefile makefile.ori
wget --no-check-certificate https://raw.githubusercontent.com/pletzer/nextsimroms_apptainer/refs/heads/main/packages/iceshelf_roms/makefile
cd Compilers
mv Linux-gfortran.mk Linux-gfortran.mk.ori
wget --no-check-certificate https://raw.githubusercontent.com/pletzer/nextsimroms_apptainer/refs/heads/main/packages/iceshelf_roms/Compilers/Linux-gfortran.mk
cd ..
make clean
make
cp ./oceanM $INSTALL_DIR/bin

```

