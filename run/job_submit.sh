#!/bin/bash -e
#SBATCH --job-name=50km_nextsim # job name (shows up in the queue)
#SBATCH --time=01:00:00      # Walltime (HH:MM:SS)
#SBATCH --nodes=1         # for the time being
#SBATCH --ntasks=8        # number of MPI ranks
#SBATCH --cpus-per-task=1 # 1 thread, openmp has race conditions
#SBATCH --mem-per-cpu=2G
#SBATCH --output=log_nextsim.log
#SBATCH -A vuw02565
#SBATCH --partition=milan
 
ml purge
ml Apptainer
ml intel # MPI

sif=/nesi/nobackup/pletzera/nextsim.sif
# where the data reside, ADJUST
dst=$PWD # /nesi/nobackup/nesi99999/pletzera/nextsim_nocpl_run/n/
 
export OMP_NUM_THREADS=1 
echo "ntasks = $SLURM_NTASKS nthreads = $OMP_NUM_THREADS"
 
export NEXTSIM_DATA_DIR=$PWD
export NEXTSIM_MESH_DIR=$PWD

mkdir -p $dst/data

# default is shm:ofi which causes an MPI init error
#export I_MPI_FABRICS=shm
export I_MPI_FABRICS=ofi
 
# use the executable in the container
#exe=/usr/local/ifort/build/nocpl/nextsim/model/bin/nextsim.exec
exe=/usr/local/ifort/build/oasis/nextsim/model/bin/nextsim.exec

#apptainer exec -B $dst/data:/data,$dst/data:/mesh $sif mpiexec -n $SLURM_NTASKS $exe --config-files=nextsim.cfg
srun apptainer exec -B $dst/data:/data,$dst/data:/mesh $sif $exe --config-files=input/nextsim.cfg : -n 1 ocean


