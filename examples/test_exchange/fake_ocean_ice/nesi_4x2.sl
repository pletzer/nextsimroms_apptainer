#!/bin/bash -e
#SBATCH --job-name=fake-ocean-ice 
#SBATCH --time=00:01:00      # Walltime (HH:MM:SS)
#SBATCH --ntasks=4 # ocean
#SBATCH hetjob
#SBATCH --ntasks=2 # ice

srun --het-group=0 ./ocean &
srun --het-group=1 ./ice &
wait