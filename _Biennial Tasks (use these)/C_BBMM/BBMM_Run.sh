#!/bin/sh

#SBATCH --job-name=bbs14t14
#SBATCH --account=wyldtech1

#SBATCH --output=hpc_logs/SLURM_Test_%A.log
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=lwilde2@uwyo.edu

#SBATCH --time=3-00:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=50G

#SBATCH --array=1-25

source ~/.bashrc
{ module purge
module load gcc 
module load curl/8.4.0__libidn2_True
module load gdal 
module load proj 
module load geos 
module load udunits 
module load r 
} >/dev/null 2>&1
cd /project/wyldtech1/lwilde2/scripts

# Set timezone
export TZ="America/Denver"

# Run the R script while passing the path
Rscript CodeToRunBBMMForSpring.R "$SLURM_ARRAY_TASK_ID" "$SLURM_ARRAY_TASK_MAX" 