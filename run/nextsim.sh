#!/bin/bash -e 

# nextsim does not like running with more than one thread
export OMP_NUM_THREADS=1

echo "running $NEXTSIMEXE with:"
echo "NEXTSIM_DATA_DIR = $NEXTSIM_DATA_DIR"
echo "NEXTSIM_MESH_DIR = $NEXTSIM_MESH_DIR"
echo "with OMP_NUM_THREADS = $OMP_NUM_THREADS"
echo "in container $SIFFILE..."


srun apptainer exec -B ${NEXTSIM_DATA_DIR}/data:/data,${NEXTSIM_MESH_DIR}/data:/mesh $SIFFILE $NEXTSIMEXE --config-files=input/nextsim.cfg

