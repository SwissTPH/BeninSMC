#################################
# 1. Generate the simulation scenarios
# 2. Run OpenMalaria simulations
# 3. Postprocessing of results in one database
#
# To run this script inside a screen session in the Linux terminal,
# type screen then navigate to the script's folder
# load the R module inside the screen session:
# module purge
# module load R/4.2.1-foss-2022a (the R version where packages are stored)
# then run the script
# Rscript 02_runmost_past.R
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
library(devtools)
library(openMalariaUtilities)
library(OMAddons)
library(omuslurm)
library(omucompat)
library(RSQLite)

# Define root directory with all the experiments according to the user
if (Sys.getenv("USER") == "lemant0000") {
  root_dir_path = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMC/"
} else {
  print("Please specify the paths to the necessary folders!")
}

expName = "BeninSMC_1calibration"

# Load experiment
start_time = Sys.time()
print(paste(Sys.time(), "-> Loading experiment ..."))
experiment_folder = paste0(root_dir_path, expName)
loadExperiment(experiment_folder)

# Create scenarios
print(paste(Sys.time(), "-> Creating scenarios ..."))
slurmCreateScenarios()

# Check that all scenarios were created
check_scenarios_created(experiment_folder)

print(paste(Sys.time(), "-> Running OpenMalaria simulations ..."))
slurmRunSimulation()

# Check that all OpenMalaria simulations were created
check_simulations_created(experiment_folder)

print(paste(Sys.time(), "-> Running postprocessing ..."))
slurmRunResults()
end_time = Sys.time()

print(paste(Sys.time(), "-> Finished running for", trial))
as.difftime(end_time - start_time, format = "%X", units = "auto", tz = "CET")
