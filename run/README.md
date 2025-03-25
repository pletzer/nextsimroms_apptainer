# Template to create a nextsim-fake ocean ru

This is a template repo and contains input files and scripts to run nextsim with a fake_ocean (ocean.F90). 

## Steps

    1. Edit job_submit.sh. Variable `dst` should point to the directory containing the input data, e.g. `dst=$PWD`
    2. You might to change the slurm account
    3. You might want to adjust the maximum time and the number of processes
    4. The sif file is an Apptainer image that contains the nextsim executable and the OASIS3-MCT libraries. Ajust the location of the sif file as needed
    5. When ready, submit with `sbatch job_submit.sh`

