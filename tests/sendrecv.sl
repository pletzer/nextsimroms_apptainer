#!/bin/bash -e
#SBATCH --job-name=test      # job name (shows up in the queue)
#SBATCH --time=00:05:00      # Walltime (HH:MM:SS)
#SBATCH --ntasks=10

ml purge
ml Apptainer

#export I_MPI_FABRICS=ofi
export I_MPI_FABRICS=shm
export I_MPI_DEBUG=2
echo "communication fabric: $I_MPI_FABRICS"

SIF=/nesi/nobackup/nesi99999/pletzera/sifs/nextsim.sif
PROG=sendrecv
COMPILER=mpiifort

# compile
apptainer exec $SIF $COMPILER ${PROG}.f90 -o ${PROG}.exe

# run
srun apptainer exec $SIF ./${PROG}.exe
