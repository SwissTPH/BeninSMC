#!/bin/bash
#SBATCH --job-name=BeninSMC_2futuresimulations_scenarios
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=50
#SBATCH --mem-per-cpu=5GB
#SBATCH --output=/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_2futuresimulations/logs/scenarios/slurm_BeninSMC_2futuresimulations_scenarios_%A_%a.log
#SBATCH --error=/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_2futuresimulations/logs/scenarios/slurm_BeninSMC_2futuresimulations_scenarios_%A_%a_error.log
#SBATCH --array=1-54
#SBATCH --time=00:30:00
#SBATCH --qos=30min
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 0)

module purge
module load R/4.2.1-foss-2022a

Rscript /scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_2futuresimulations/slurm_run_scenarios.R $ID $SLURM_CPUS_PER_TASK

