#!/bin/bash
#SBATCH --job-name=BeninSMC_2futuresimulations_results
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=30
#SBATCH --mem=50G
#SBATCH --output=/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_2futuresimulations/logs/results/slurm_BeninSMC_2futuresimulations_results_%A_%a.log
#SBATCH --error=/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_2futuresimulations/logs/results/slurm_BeninSMC_2futuresimulations_results_%A_%a_error.log
#SBATCH --time=06:00:00
#SBATCH --qos=6hours

module purge
module load R/4.2.1-foss-2022a

Rscript /scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_2futuresimulations/slurm_run_results.R $SLURM_CPUS_PER_TASK

