#!/bin/bash
#SBATCH --account=sta440-f20
#SBATCH -p common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=1G

python data.py