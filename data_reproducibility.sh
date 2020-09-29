#!/bin/bash
#SBATCH --account=sta440-f20
#SBATCH -p common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=4G

##########################################################

# data_reproducibility.sh is a shell script that reproduces all data transformations prior to modelling
# This shell script runs multiple files, each performing different manipulations and having different outputs
# 
# [TO RUN THIS FILE]
#  type: "sbatch data_reproducibility.sh" in the terminal, in the directory that contains this file

# [INPUT]
# All inputs are pkl files taken from path: /hpc/group/sta440-f20/WESAD/WESAD

# [OUTPUTS]
# All outputs will be placed in a folder 'output_data' in the same path as this reproducibility shell script

##########################################################

# removes previous outputs if code was run before
if [ -d "./output_data" ]
then
    rm -r output_data
fi
mkdir output_data

# Step 1: Converts WESAD Data pkl files to csv files
#
# Output:  - csv files by participant and by device
# Output Location: ./output_data
module load Python/3.8.1
python data.py

# Step 2: Combining Separate csv files to RDS files
# Output: 3 RDS (chest_and_wrist.Rds, chest.Rds, wrist.Rds)
# Output Location: ./output_data
module load R/3.6.3
Rscript data-processing.R

# Step 3: Feature Engineering from raw signal data RDS files
# Output: 1 RDS (final_data)
# Output Location: ./output_data

# [ERROR UPDATE]
# Running this R script requires the 'peakPick' package, which is not available on the cluster
# Thus a saved RDS file from a pre-ran script is used for the study

# module load R/4.0.0
# Rscript data-feature-engineering.R