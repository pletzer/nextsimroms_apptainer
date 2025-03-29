#!/bin/bash -e
#SBATCH --job-name=nextsim-ocean
#SBATCH --time=00:30:00
#SBATCH --ntasks=5 #  total nextsim + ocean
#SBATCH --cpus-per-task=1
#SBATCH --mem=5g

unset PYTHONPATH
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

# make sure to use the version compiled with oasis
export NEXTSIMEXE=/usr/local/ifort/build/oasis/nextsim/model/bin/nextsim.exec

srun -n 4 apptainer exec -B ${NEXTSIM_DATA_DIR}/data:/data,${NEXTSIM_MESH_DIR}/data:/mesh $SIFFILE $NEXTSIMEXE --config-files=input/nextsim.cfg : -n 1 apptainer exec $SIFFILE ./ocean
# this does not work
#srun ./nextsim.sh : ./ocean.sh

#srun --het-group=0 ./nextsim.sh &
#srun --het-group=1 ./ocean.sh &

#srun --het-group=1 apptainer exec $SIFFILE ./ocean &
#srun --het-group=0 apptainer exec -B ${NEXTSIM_DATA_DIR}/data:/data,${NEXTSIM_MESH_DIR}/data:/mesh $SIFFILE $NEXTSIMEXE --config-files=input/nextsim.cfg &
#srun --het-group=1 apptainer exec $SIFFILE ./ocean &
#wait

