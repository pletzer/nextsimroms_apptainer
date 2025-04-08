#!/bin/bash -e
#SBATCH --job-name=fake-ocean-ice 
#SBATCH --time=00:01:00      # Walltime (HH:MM:SS)
#SBATCH --ntasks=4 # ocean
#SBATCH hetjob
#SBATCH --ntasks=2 # ice

ml purge
ml Apptainer intel
SIFFILE=/nesi/nobackup/pletzera/nextsim.sif

# required to avoid an pm2 error at MPI initialization
export I_MPI_FABRICS=ofi

rm -f grids.nc namcouple
ln -s ../common_data/grids.nc .
ln -s oi_data/namcouple .
rm -f nout.?????? debug*

srun --het-group=0 apptainer exec $SIFFILE ./ocean &
srun --het-group=1 apptainer exec $SIFFILE ./ice &
wait
