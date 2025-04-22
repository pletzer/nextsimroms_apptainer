#!/bin/bash -e

#SBATCH --job-name=diffusion
#SBATCH --time=00:05:00
#SBATCH --ntasks=2
#SBATCH --mem=10G
#SBATCH --output slog/%j.out 

# Load necessary modules
module purge
module load Apptainer intel

# Required to avoid MPI_Init error and ensure correct MPI configuration
export I_MPI_FABRICS=ofi
export I_MPI_PMI_LIBRARY=/opt/slurm/lib64/libpmi2.so

# Define container path
SIF=/nesi/nobackup/pletzera/nextsim.sif

# Clean up any existing files and create required symlinks
rm -f grids.nc namcouple nout.?????? debug*
ln -s ../common_data/grids.nc .
ln -s oi_data/namcouple .
unset PYTHONPATH

# Print the contents of the namcouple file to verify it contains the expected variables
echo "Contents of namcouple file:"
cat oi_data/namcouple

# Run mpiexec directly (not through srun) since we're already in a Slurm allocation
apptainer exec $SIF mpiexec -n 1 ./ocean : -n 1 ./ice
