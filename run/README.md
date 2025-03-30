# Template to create a nextsim-fake ocean ru

This is a template repo and contains input files and scripts to run nextsim with a fake_ocean (ocean.F90). 

## Step 1

You will need to build the fake ocean model within the Apptainer environment. This version of the 
fake ocean model first exports data to the ice component, and then imports data from the ioce component. Accordingly, 
no `LAG` is required.

To build the fake ocean,
```bash
ml purge
ml Apptainer

# path to the apptainer image
sif=/nesi/nobackup/pletzera/nextsim.sif
apptainer shell $sif
Apptainer> make clean
Apptainer> make
```

## Step 2

Run the coupled model interactively
```bash
ml purge
ml Apptainer
# adjust NEXTSIM_DATA_DIR and NEXTSIM_MESH_DIR!
export NEXTSIM_DATA_DIR=/nesi/nobackup/nesi99999/pletzera/nextsim_oasis_run3/50km_oasis_20130102/
export NEXTSIM_MESH_DIR=$NEXTSIM_DATA_DIR
mkdir -p ${NEXTSIM_DATA_DIR}/data
mkdir -p ${NEXTSIM_MESH_DIR}/data
# path to the apptainer image
sif=/nesi/nobackup/pletzera/nextsim.sif
apptainer shell -B ${NEXTSIM_DATA_DIR}/data:/data,${NEXTSIM_MESH_DIR}/data:/mesh $sif
# nextsim is not threadsafe
# mahuika requires I_MPI_FABRICS=ofi
# ocean code only needs on MPI process
Apptainer> OMP_NUM_THREADS=1 I_MPI_FABRICS=ofi mpiexec -n 4 /usr/local/ifort/build/oasis/nextsim/model/bin/nextsim.exec --config-files=input/nextsim.cfg : -n 1 ./ocean
```
