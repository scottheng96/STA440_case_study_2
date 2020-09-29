#!/bin/bash
#SBATCH --account=sta440-f20
#SBATCH -p common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=5G
#SBATCH --mail-type=end
#SBATCH --mail-user=es321@duke.edu

module load R/3.6.0
Rscript code_reproducibility.R
