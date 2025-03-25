# Template to create a nextsim-fake ocean ru

This is a template repo and contains input files and scripts to run nextsim with a fake_ocean (ocean.F90). 

## Step 1

You will need to build the fake ocean model within the Apptainer environment. Adjust the path to the sif file as needed.
```bash
ml purge
ml Apptainer
apptainer shell /nesi/nobackup/pletzera/nextsim.sif
Apptainer> make clean
Apptainer> make
```

## Step 2

Edit the `job_submit.sh' slurm submission file. You might to change the slurm account. You want to adjust the maximum time and the number of processes.

## Step 3

Submit with
```bash
sbatch job_submit.sh
```

Note: you can pass slurm options to override the settings in the slurm script, e.g.
```bash
sbatch -A nesi99999 job_submit.sh
```
