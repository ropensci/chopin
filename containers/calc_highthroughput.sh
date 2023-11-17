#!/bin/bash

#SBATCH --job-name=calc_
#SBATCH --error=init-rocker.error
#SBATCH --mail-user=songi2@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=highmem

## apptainer
apptainer exec --writable-tmpfs --mount='type=bind,source=/opt,destination=/ddn/gs1/home/songi2/projects/PrestoGP_Pesticides/input' /ddn/gs1/home/songi2/images/rocker_base_computation.sif Rscript /ddn/gs1/home/songi2/projects/PrestoGP_Pesticides/Calc_WBD.r
