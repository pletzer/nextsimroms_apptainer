#!/bin/bash -e
#SBATCH --job-name=diffusion
#SBATCH --time=00:05:00
#SBATCH --ntasks=1
#SBATCH hetjob
#SBATCH --ntasks=1

# Load necessary modules
module purge
module load Apptainer intel

# required to avoid MPI_Init error
export I_MPI_FABRICS=ofi

SIF=/nesi/nobackup/pletzera/nextsim.sif

rm -f grids.nc namcouple
ln -s ../common_data/grids.nc .
ln -s oi_data/namcouple .
rm -f nout.?????? debug*
unset PYTHONPATH

srun --het-group=0 apptainer exec $SIF ./ocean &
srun --het-group=1 apptainer exec $SIF ./ice &
wait

