Steps to Reproduce Case Study Goals 1, 4 and 5. 

1. Log on to the cluster - ssh netID@dcc-slogin.oit.duke.edu
2. cd /hpc/group/sta440-f20/netID
3. Upload the following files into your folder: data.py, data-processing.R, data-feature-engineering.R, final_data.Rds, code_reproducibility.R, data_reproducibility.R, code_reproducibility.sh, data_reproducibility.sh

4. Start an interactive session: srun --pty bash -i

If you want to run everything from scratch, follow step 5. If you just want to reproduce the 3 case study goals, skip ahead to step 6. 

5. Reproduce the data: sbatch data_reproducibility.sh* (It is recommended that you skip to step 6 to save time.)

6. Reproduce the case study goals: sbatch code_reproducibility.sh**
Reproduce the case study goals: cat slurm-###.out, where ### is the batch job number. This  should return some text, as well as tables that display the case study goals. The output will be very long, so make sure you scroll to the top to see it all. 

*  This code should take around 1 hour to run. 
** This code should take less than 5 minutes to run, depending on how busy the cluster is. Additionally, you can go into the code_reproducibility.sh and change #SBATCH --mail-user=es321@duke.edu line to be your email. It will email you when the job is done running. 

