# nextsimroms_apptainer

Definition file for creating a nextsim/ROMS/OASIS3-MCT coupling environment.

## Introduction

Containerisation allows you to compile code that can be easily ported from one platform to another (with some restrictions). As the name suggests, all the dependencies are contained.

The container `oasis3-mct.sif` described below comes with an operating system, compilers, libraries (MPI, NetCDF) and tools (git, vim, python, ...). It can be thought as a replacement for the 
modules used on many high performance computers to load in dependencies (toolchain, NetCDF, etc).

## Prerequisites

First you need to have Apptainer or Singularity installed. If you're running Linux, you're in luck, just follow these [instructions](https://apptainer.org/docs/user/latest/). On Windows, you can use Windows Linux Subsystem (WSL) and on Mac you might have to install Apptainer within a Docker environment. 

## How to build a container on your personal computer

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
singularity shell -B/scale_wlg_nobackup/filesets/nobackup,/nesi/nobackup,$HOME,/opt/niwa /nesi/nobackup/pletzera/oasis3-mct.sif
```

## Compiling the OASIS3-MCT examples

## Building neXTSIM with the container tools

## Building ROMS with the container tools
