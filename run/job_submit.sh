#!/bin/bash -e
#SBATCH --job-name=mpmd_mpi_example
#SBATCH --ntasks=3
#SBATCH --time=00:05:00

# Load necessary modules
ml purge
ml Apptainer intel

# path to the Apptainer image, ADJUST
export SIFFILE=/nesi/nobackup/pletzera/nextsim.sif
# where the data reside, ADJUST
export NEXTSIM_DATA_DIR=/nesi/nobackup/nesi99999/pletzera/nextsim_oasis_run3/50km_oasis_20130102/ 
export NEXTSIM_MESH_DIR=$NEXTSIM_DATA_DIR

mkdir -p ${NEXTSIM_DATA_DIR}/data
mkdir -p ${NEXTSIM_MESH_DIR}/data

# default is shm:ofi which causes an MPI init error on mahuika/milan
#export I_MPI_FABRICS=shm
export I_MPI_FABRICS=ofi

srun --ntasks=2 ./nextsim.sh : --ntasks=1 ./ocean.sh
