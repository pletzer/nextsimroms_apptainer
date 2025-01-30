# nextsimroms_apptainer

Definition file for creating a nextsim/ROMS/OASIS3-MCT coupling environment.

## Introduction

Containerisation allows you to compile code that can be easily ported from one platform to another (with some restrictions). As the name suggests, all the dependencies are contained.

The container `oasis3-mct.sif` described below comes with an operating system, compilers, libraries (MPI, NetCDF) and tools (git, vim, python, ...). It can be thought as a replacement for the 
modules used on many high performance computers to load in dependencies (toolchain, NetCDF, etc).

## Prerequisites

First you need to have Apptainer or Singularity installed. If you're running Linux, you're in luck, just follow these [instructions](https://apptainer.org/docs/user/latest/). On Windows, you can use Windows Linux Subsystem (WSL) and on Mac you might have to install Apptainer within a Docker environment. 

## How to build a container

A container needs a definition file, e.g. `oasis3-mct.def`. This file lists the operating system, the compilers and the steps to build the libraries. It is a recipe for building the container.

To build the container, type
```
apptainer build oasis3-mct.sif oasis3-mct.def
```
or, on a local laptop if you encounter the error `FATAL: ...permission denied`,
```
sudo apptainer build --force oasis3-mct.sif oasis3-mct.def
```
Note: if you're using Singularity you may replace `apptainer` with `singularity` in the above and below commands.

### Building the container on NeSI

If you don't have access to a Linux system with Apptainer installed, you can also [build the container on Mahuika](https://support.nesi.org.nz/hc/en-gb/articles/6008779241999-Build-an-Apptainer-container-on-a-Milan-compute-node) by submitting the follwowing SLURM job
```
#!/bin/bash -e
#SBATCH --job-name=apptainer_build
#SBATCH --partition=milan
#SBATCH --time=0-08:00:00
#SBATCH --mem=30GB
#SBATCH --cpus-per-task=4

# load environment module
module purge
module load Apptainer

# recent Apptainer modules set APPTAINER_BIND, which typically breaks
# container builds, so unset it here
unset APPTAINER_BIND

# create a build and cache directory on nobackup storage
export APPTAINER_CACHEDIR="/nesi/nobackup/$SLURM_JOB_ACCOUNT/$USER/apptainer_cache"
export APPTAINER_TMPDIR="/nesi/nobackup/$SLURM_JOB_ACCOUNT/$USER/apptainer_tmpdir"
mkdir -p $APPTAINER_CACHEDIR $APPTAINER_TMPDIR
setfacl -b $APPTAINER_TMPDIR

apptainer build --force --fakeroot oasis3-mct.sif oasis3-mct.def
```

Once the build completes you will end up with a file `oasis3-mct.sif`, which you can copy across platforms.

## How to run a shell within a container

Assuming you have loaded the `Apptainer` module on Mahuika (or have the command `apptainer` available on your system),
```
apptainer shell oasis3-mct.sif
```
will land you in an environment with compilers
```
Apptainer> 
```
Note that you may need to bind some directories to access external data inside the container. This is achieved with the `-B` option. For instance,

```
apptainer shell -B/scale_wlg_nobackup/filesets/nobackup,/nesi/nobackup,$HOME,/opt/niwa /nesi/nobackup/pletzera/oasis3-mct.sif
```

Once you're inside the container, you can check that your compilers are there and working:
```
Apptainer> which mpif90
/usr/bin/mpif90
Apptainer> mpif90 --version
GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
Copyright (C) 2021 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

## Compiling NeXTSim

```
apptainer shell nextsim-dev.sif
git clone git@github.com:nansencenter/nextsim.git
cd nextsim
git checkout apptainer 
source env_compile_intel_linux.bash

cd contrib/mapx/src
make -j 4

cd ../../bamg/src
make -j 4

cd ../../../core/src
make -j 4

```

## Compiling the OASIS3-MCT examples on Mahuika

### Start an Apptainer shell
```
module purge
module load Apptainer
apptainer shell /nesi/nobackup/pletzera/oasis3-mct.sif
```

### Check out the oasis3-mct code

On any locally mounted directory,
```
Apptainer>  git clone -b OASIS3-MCT_5.2  https://gitlab.com/cerfacs/oasis3-mct.git
```

### Edit the Makefile 
```
Apptainer> cd oasis3-mct/examples/tutorial_communication
```
and replace the lines
```
include ../../util/make_dir/make.inc
```
and
```
all: oasis3_psmile ocean atmos
```

with
```
include $(COUPLE)/util/make_dir/make.gcc
```
and
```
all: ocean atmos
```
respectively

### Compile 

```
Apptainer> make
```


### Run
```
Apptainer> mkdir test_run
Apptainer> cd test_run/
Apptainer> cp ../data_tutorial/* .
Apptainer> cp ../atmos .
Apptainer> cp ../ocean .
Apptainer> mpiexec -n 4 ./ocean : -n 4 ./atmos
```

Alternatively, you can run the code outside the container:
```
apptainer exec <PATH>/oasis3-mct.sif mpiexec -n 4 ./ocean : -n 4 ./atmos
```

## Building neXTSIM with the container tools

TO DO

## Building ROMS with the container tools

TO DO
