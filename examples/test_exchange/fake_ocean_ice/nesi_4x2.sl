#!/bin/bash -e
#SBATCH --job-name=fake-ocean-ice 
#SBATCH --time=00:01:00      # Walltime (HH:MM:SS)
#SBATCH --ntasks=4 # ocean
#SBATCH hetjob
#SBATCH --ntasks=2 # ice

ml purge
ml Apptainer intel
SIFFILE=/nesi/nobackup/pletzera/nextsim.sif

srun --het-group=0 apptainer exec $SIFFILE ./ocean &
srun --het-group=1 apptainer exec $SIFFILE ./ice &
wait
