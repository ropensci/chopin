#!/bin/bash

#SBATCH --job-name=$1
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=$2
#SBATCH --mem=$3
#SBATCH --mail-user=$4
#SBATCH --mail-type=END,FAIL

# Load necessary modules

# Run your R script
swarm Rscript $5

