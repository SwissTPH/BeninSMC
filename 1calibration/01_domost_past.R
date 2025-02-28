#################################
# 1. Define the base xml
# 2.: - Define the simulation settings
#     - Create the scripts for generating the simulation scenarios
#     - Create the scripts for running OpenMalaria simulations
#     - Create the scripts for postprocessing
#
# modified 15.11.2024 by Jeanne Lemant
#################################

# History cleanup
rm(list=ls())

# Install openMalariaUtilities
# devtools::install_github("SwissTPH/r-openMalariaUtilities", ref = "23.02")

# Load the necessary packages
library(openMalariaUtilities)
library(tidyverse)

#####################################
# Initialization
#####################################

# Define root directory with all the experiments according to the user
if (Sys.getenv("USER") == "lemant0000") {
  root_dir_path = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper/"
} else {
  print("Please specify the paths to the necessary folders!")
}

# Set working directory to the one where this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the base xml list setup function and auxiliary functions
source("00_create_base_past.R")

# Source helper functions
source("../helper_functions/helper_functions_setup_simulations.R")

#####################################
# Experiment setup
#####################################

expName = "BeninSMC_1calibration"

# Definition of the folder where all results will be stored
# clearCache
setupDirs(experimentName = expName, rootDir = root_dir_path, replace = TRUE)

# Initialize cache and create the base xml file
baseList = create_baseList(country_name = "BEN",
                           expName = expName,
                           sim_start = "1918-01-01",
                           versionnum = 44L)
createBaseXml(baseList, replace = TRUE)

## Copy necessary Open Malaria files. It needs to be called after
## createBaseXml, otherwise the cache is not set up.
setupOM()
# If it does not work, download the files  
# autoRegressionParameters.csv and densities.csv
# from https://github.com/SwissTPH/openmalaria/tree/schema-44.0/test
# and copy them to your experiment folder (named expName, here BeninSMC1_calibration),
# where the base XML has been saved.
# Also download the file scenario_44.xsd
# from https://github.com/SwissTPH/openmalaria/blob/schema-44.0/schema/scenario_44.xsd
# and copy it to the experiment folder

## Countrydat 
#' with information on
#'effective coverage
#'seasonality
#'historical net use
#'historical IRS
#'historical SMC
countrydat <- read.csv("../data/BEN_countrydat_oct2022_ITN_MAP_surveys.csv") %>%
  mutate(across(contains("cov"), ~replace(., is.na(.), 0))) %>%
  mutate(across(contains("month"), ~replace(., is.na(.), 1)))

dat <- countrydat %>%
  #from 2011 we use survey data
  select(-all_of(paste0("histITNcovMAP",2011:2019))) %>%
  mutate(across(contains("month"),
                ~paste0(gsub("^\\D+","",cur_column()),"-0",.,"-01")))

colnames(dat) <- gsub("MAP","",colnames(dat))
colnames(dat) <- gsub("surveys","",colnames(dat))

##-- need to convert Access to 5-day time steps
## Make sure there are no NAs in dat otherwise convert_access fails
dat = convert_access(dat,pattern="EffCov14d",katya=F,scale=1)


#####################################
# Defining the simulation scenarios
#####################################

prop_Gambiae <- .9
source("../0modelling_parameters/indoor_outdoor_exposure.R")

# Define the list with all the scenario variations per country
# (population size, seed, EIR)
full             = list()
full $ seed      = 1:10
full $ setting   = unique(dat$setting)
full $ pop       = 3000L
full$EIR = c(1:10,seq(12,250,2))
full$futITNcov=0
full$gin = prop_Gambiae*exposure_Gambiae$Exposure_Indoor_total
full$gout = prop_Gambiae*exposure_Gambiae$Exposure_Outdoor_total
full$fin = (1-prop_Gambiae)*exposure_Funestus$Exposure_Indoor_total
full$fout = (1-prop_Gambiae)*exposure_Funestus$Exposure_Outdoor_total

#### 'scens' will contain all possible combinations of these scenario variations
scens = expand.grid( full ) %>%
  left_join(dat,by="setting")

## Are all placeholders in base XML a column in scens?
getCache("placeholders")[!(getCache("placeholders") %in% colnames(scens))]

print(paste(nrow(scens), "scenarios defined for calibration"))

# Ensure that the scenario dataframe is correctly defined (adds ID and file columns)
scens = finalizeScenarios(scens)

# Store the scenarios in the cache folder
storeScenarios(scens)

#####################################
# Validate the xml
#####################################
if (validateXML(xmlfile = getCache(x = "baseXml"),
                schema = paste0(getCache(x = "experimentDir"), "/scenario_44.xsd"),
                scenarios = scens)) {
  print ("XML definition is valid.")
}

#####################################
# Prepare scripts for creating, running and postprocessing all the scenarios and simulations
# with the slurm schedulung system.
# We use a internal package (omuslurm) below to generate R scripts to create all XML files,
# run simulations and postprocess them, as well as generate bash files (.sh) to run these R scripts with slurm.

# These R scripts and bash files are also provided in the repository (1calibration/slurm_files), 
# so if you don't have omuslurm, you can copy them into the experiment folder,
# then navigate to the experiment folder in your terminal and run successively
# sbatch slurm_scenarios.sh
# sbatch slurm_simulation.sh
# sbatch slurm_results.sh.
# Make sure to wait until each has finished before running the next one, otherwise
# you may have missing simulations!
# You can then skip 02_runmost_past.R and go directly to 03_calibrate.R.
#####################################

library(omuslurm) 
print ("Generating analysis scripts ...")

## to run if lines above have not been run in this session
# experiment_folder = paste0(root_dir_path, expName)
# loadExperiment(experiment_folder)

## 1. Prepare the scripts for creating the scenarios
# Make sure to adjust the nCPU, memCPU, time and qos if you run larger experiments
slurmPrepareScenarios(expName = expName, scenarios = scens, nCPU = 50, bSize = 200,
                      memCPU = "5GB", time = "00:30:00",qos = "30min",
                      rModule = "R/4.2.1-foss-2022a"
)

## 2. Prepare the scripts for running OpenMalaria simulations
# Make sure to adjust the nCPU, memCPU, time and qos if you run larger experiments
slurmPrepareSimulations(expName = expName, scenarios = scens,
                        memCPU = "10GB", nCPU =50,bSize = 200,
                        time = "00:30:00", qos = "30min",
                        rModule = "R/4.2.1-foss-2022a",
                        omModule = "OpenMalaria/44.0-intel-compilers-2023.1.0"
)

## 3. Prepare the scripts for post-processing the OpenMalaria outputs
# Define the age groups of interest for the outputs (including aggregations)
age_groups_list = c("2-10")

# Define the OpenMalaria outputs of interest; these will correspond to the
# columns of the results table to be stored in the database
results_columns = c("scenario_id",
                    "date", "age_group", "date_aggregation",
                    "nHost", "prevalenceRate")

# Remove the results database if it already exists
# Overwriting an existing database with the same scenario IDs will not work
db_file = file.path(paste0(root_dir_path, expName, ".sqlite"))
if (file.exists(db_file)) {
  print(paste0("A database for ", expName, " exists already and will be removed."))
  file.remove(db_file)
}

# Generate the postprocessing script
# Make sure to adjust the nCPU, memCPU, time and qos if you run larger experiments
slurmPrepareResults(expDir = getCache("experimentDir"), dbName = expName,
                    resultsName = "om_results", resultsCols = results_columns,
                    aggrFun = CalcEpiOutputs,
                    aggrFunArgs = list(indicators = results_columns,
                                       aggregateByAgeGroup = age_groups_list,
                                       aggregateByDate = "year"),
                    ntasks = 1, mem = "50G", nCPU = 30,
                    strategy = "batch", indexOn = NULL,
                    rModule = "R/4.2.1-foss-2022a")

