#################################
# 1. Generate the simulation scenarios
# 2. Run OpenMalaria simulations
# 3. Postprocessing of results in one database
#
# Only if you have the omuslurm package, otherwise copy the files from 
# 2future_simulations/slurm_files  into the experiment folder,
# then navigate to the experiment folder in your terminal and run successively
# sbatch slurm_scenarios.sh
# sbatch slurm_simulation.sh
# sbatch slurm_results.sh.
# Make sure to wait until each has finished before running the next one, otherwise
# you may have missing simulations!
# You can then skip 02_runmost_past.R and go directly to 03_merge_future_with_calibration.R.
#
# FROM HERE ONLY IF YOU HAVE omuslurm PACKAGE
# This script will leave your R session hanging until everything has run
# (several hours), so you can run it in another session:
# To run this script inside a screen session in the Linux terminal,
# type screen then navigate to the script's folder
# load the R module inside the screen session:
# module purge
# module load R/4.2.1-foss-2022a (the R version where packages are stored)
# then run the script
# Rscript 02_runmost_future.R
#To detach from the screen, you need to type CTRL+A+D.
#You can reattach to the script using screen -r
#if you have only one screen session open,
#or by first listing all the screen sessions with screen -ls and
#using screen -r screen_id to attach to a specific screen.
#To delete a screen you can attach to it and then type exit
#If the screen is running, type screen -S screen_id -X quit
# 
# modified 15.11.2024 by Jeanne Lemant
#################################

# History cleanup
rm(list=ls())

# Load the necessary packages
library(openMalariaUtilities)
library(omuslurm)

# Set working directory to the one where this file is located
# This works only when running the script from Rstudio (comment out if running on screen)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# print(paste("Working directory set to ", dirname(rstudioapi::getActiveDocumentContext()$path)))

# Define root directory with all the experiments according to the user
if (Sys.getenv("USER") == "lemant0000") {
  root_dir_path = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper/"
} else {
  print("Please specify the paths to the necessary folders!")
}

expName = "BeninSMC_2futuresimulations"

source("../helper_functions/helper_functions_simulations.R")

# Load experiment
start_time = Sys.time()
print(paste(Sys.time(), "-> Loading experiment ..."))
experiment_folder = paste0(root_dir_path, expName)
loadExperiment(experiment_folder)

# Create scenarios
# print(paste(Sys.time(), "-> Creating scenarios ..."))
# slurmCreateScenarios()

# Check that all scenarios were created
check_scenarios_created(experiment_folder)

print(paste(Sys.time(), "-> Running OpenMalaria simulations ..."))
slurmRunSimulation()

# Check that all OpenMalaria simulations were created
check_simulations_created(experiment_folder)

print(paste(Sys.time(), "-> Running postprocessing ..."))
slurmRunResults()
end_time = Sys.time()

print(paste(Sys.time(), "-> Finished running"))
as.difftime(end_time - start_time, format = "%X", units = "auto", tz = "CET")
