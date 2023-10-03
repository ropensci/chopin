#!/bin/bash

#SBATCH --job-name=calc_merra_subset_scomps.r
#SBATCH --error=terra.climate.local.error
#SBATCH --mail-user=songi2@nih.gov
#SBATCH --mail-type=NONE
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=highmem

## Run R:
/ddn/gs1/biotools/R/bin/Rscript -e "sessionInfo()"
/ddn/gs1/biotools/R421/bin/Rscript -e "sessionInfo()"
