#!/bin/bash
#SBATCH --job-name=BeninSMC_1calibration_simulation
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=50
#SBATCH --mem-per-cpu=10GB
#SBATCH --output=/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_1calibration/logs/simulation/slurm_BeninSMC_1calibration_simulation_%A_%a.log
#SBATCH --error=/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_1calibration/logs/simulation/slurm_BeninSMC_1calibration_simulation_%A_%a_error.log
#SBATCH --array=1-156
#SBATCH --time=00:30:00
#SBATCH --qos=30min
ID=$(expr ${SLURM_ARRAY_TASK_ID} - 0)

export LMOD_DISABLE_SAME_NAME_AUTOSWAP="no"
module purge
module load R/4.2.1-foss-2022a
module load OpenMalaria/44.0-intel-compilers-2023.1.0
cd /scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_1calibration

Rscript /scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_1calibration/slurm_run_simulation.R $ID $SLURM_CPUS_PER_TASK

