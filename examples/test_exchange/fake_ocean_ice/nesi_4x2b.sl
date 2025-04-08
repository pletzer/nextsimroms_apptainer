#!/bin/bash -e
#SBATCH --job-name=fake-ocean-ice 
#SBATCH --time=00:01:00      # Walltime (HH:MM:SS)
#SBATCH --ntasks=6 # ocean + ice

nice="2"
nocean=$(expr ${SLURM_NTASKS} - ${nice})
echo "Running ocean with $nocean and ice with $nice procs"
srun --ntasks=$nocean ./ocean : --tasks=$nice ./ice
