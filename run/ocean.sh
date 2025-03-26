#!/bin/bash -e 

exe=./ocean
echo "running $exe with:"
echo "in container $SIFFILE..."

apptainer shell $SIFFILE $exe

