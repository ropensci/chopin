#!/bin/bash

#SBATCH --job-name=init-rocker
#SBATCH --error=init-rocker.error
#SBATCH --mail-user=songi2@nih.gov
#SBATCH --mail-type=END,FAIL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=highmem

## apptainer
apptainer build /ddn/gs1/home/songi2/images/rocker_base_computation.sif /ddn/gs1/home/songi2/projects/Scalable_GIS/containers/apptainer_rocker

