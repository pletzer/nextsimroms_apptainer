#!/bin/bash
#SBATCH --job-name=sendrecv_test
#SBATCH --cpus-per-task=1
#SBATCH --account=niwap99999
#SBATCH --partition=niwa_work
#SBATCH --cluster=maui_ancil
#SBATCH --gpus-per-node=nvidia_a100_1g.10gb:1
##SBATCH --gpus-per-node=A100:1
#SBATCH --time=00:02:00
echo "Date              = $(date)"
echo "Hostname          = $(hostname -s)"
echo "Working Directory = $(pwd)"
echo ""
echo "Number of Nodes Allocated      = $SLURM_JOB_NUM_NODES"
echo "Number of Tasks Allocated      = $SLURM_NTASKS"
echo "Number of Cores/Task Allocated = $SLURM_CPUS_PER_TASK"

sif=/nesi/nobackup/nesi99999/pletzera/ngarch/ngarch_apptainer/ngarch_nvhpc.sif
module load Singularity CUDA
#srun singularity exec --nv \
singularity exec --nv \
  -B/opt/niwa/um_sys/,/nesi/nobackup/nesi99999/pletzera/,/opt/nesi,/nesi/project/uoo03538/um/ \
  $sif ./hello_nvgpu 
