#!/bin/bash

# no avail
display_help() {
    # taken from https://stackoverflow.com/users/4307337/vincent-stans
    echo "Usage: $0 jobname cpus memory useremail partition imagefile bindsource rscript" >&2
    echo
    echo "  jobname      Job name"
    echo "  cpus         Number of CPUs"
    echo "  memory       Total memory to draw"
    echo "  useremail    User email address to receive status messages"
    echo "  partition    Name of partition to submit job"
    echo "  imagefile    Apptainer image file"
    echo "  bindsource   Source directory to be mapped to /mnt in container"
    echo "  rscript      R script path in host"
    # echo some stuff here for the -a or --add-options 
    exit 1
}

while [[ "$#" -gt 0 ]]; do
    case $1 in
        -h|--help) display_help; shift ;;
        # ... (same format for other required arguments)
    esac
    shift
done

#SBATCH --job-name=$1
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=$2
#SBATCH --mem=$3
#SBATCH --mail-user=$4
#SBATCH --mail-type=END,FAIL
#SBATCH --partition=$5

# Load necessary modules

# Run your R script
apptainer exec \
    --writable-tmpfs \
    --bind $7:/mnt \
    $6 Rscript $8
