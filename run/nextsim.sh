#!/bin/bash -e 

export OMP_NUM_THREADS=1

# use the version compiled with the oasis coupler
exe=/usr/local/ifort/build/oasis/nextsim/model/bin/nextsim.exec

echo "running $exe with:"
echo "NEXTSIM_DATA_DIR = $NEXTSIM_DATA_DIR"
echo "NEXTSIM_MESH_DIR = $NEXTSIM_MESH_DIR"
echo "with OMP_NUM_THREADS = $OMP_NUM_THREADS"
echo "in container $SIFFILE..."


srun apptainer exec -B ${NEXTSIM_DATA_DIR}/data:/data,${NEXTSIM_MESH_DIR}/data:/mesh $SIFFILE $exe --config-files=input/nextsim.cfg

