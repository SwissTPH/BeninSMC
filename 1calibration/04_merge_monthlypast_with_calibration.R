#################################
# Extract monthly simulations and merge them with calibrated EIRs
# 
# created 26.11.2024 by Jeanne Lemant
#################################

# History cleanup
rm(list=ls())

# Load the necessary packages
library(openMalariaUtilities)
library(RSQLite)
library(tidyverse)

# Define root directory with all the experiments according to the user
if (Sys.getenv("USER") == "lemant0000") {
  root_dir_path = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper/"
} else {
  print("Please specify the paths to the necessary folders!")
}

# Set working directory to the one where this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source helper functions
source("../helper_functions/helper_functions_setup_simulations.R")

expName = "BeninSMC_1calibration"

experiment_folder = paste0(root_dir_path, expName)
loadExperiment(experiment_folder)

##### This is to set up the extraction of monthly results.
#If you don't have the omuslurm package installed, just
# run the batch file  
# in your terminal and once it has run, go to the next section
# to read the created database.

## 3. Prepare the scripts for post-processing the OpenMalaria outputs
# Define the age groups of interest for the outputs (including aggregations)
age_groups_list = c("0-5","2-10")

# Define the OpenMalaria outputs of interest; these will correspond to the
# columns of the results table to be stored in the database
results_columns = c("scenario_id",
                    "date", "age_group", "date_aggregation",
                    "nHost", "prevalenceRate")

library(omuslurm)
# Generate the postprocessing script for monthly results
# Make sure to adjust the nCPU, memCPU, time and qos if you run larger experiments
slurmPrepareResults(expDir = getCache("experimentDir"), dbName = paste0(expName,"monthly"),
                    resultsName = "om_results_monthly", resultsCols = results_columns,
                    aggrFun = CalcEpiOutputs,
                    aggrFunArgs = list(indicators = results_columns,
                                       aggregateByAgeGroup = age_groups_list,
                                       aggregateByDate = "month"),
                    ntasks = 1, mem = "50G", nCPU = 30,
                    strategy = "batch", indexOn = NULL,
                    rModule = "R/4.2.1-foss-2022a")

slurmRunResults()

##### Start again from here once batch run has been run.
# extract simulations

# open database connection
conn = DBI::dbConnect(RSQLite::SQLite(),paste0(root_dir_path,expName,"monthly.sqlite"))
# list the tables of the database
DBI::dbListTables(conn)
# extract the results table, this should have the same name as provided to slurmPrepareResults
all_simul_0 = DBI::dbReadTable(conn, "om_results_monthly")
# always disconnect from the database
DBI::dbDisconnect(conn)

# get scens object to join simulations and scenarios by ID
scens=readRDS(file.path(experiment_folder, "cache/scenarios.rds"))

# get the EIRs from the calibration
EIRs = readRDS("../1calibration/EIRs_calibration.RDS")

# months during which the surveys were conducted
dates_prevalence_surveys = data.frame(
  date = c("2011-12-15","2012-01-15","2012-02-15","2012-03-15",
           "2015-10-15","2015-11-15",
           "2017-11-15","2017-12-15","2018-01-15","2018-02-15"),
  survey_year = c(rep("DHS_2011",4),
                  rep("MIS_2015",2),
                  rep("DHS_2017",4))
)

calibration = EIRs %>%
  ungroup() %>%
  select(setting,EIR_lci,EIR_uci,EIR) %>%
  pivot_longer(cols=starts_with("EIR"),values_to = "EIR") %>%
  select(-name) %>%
  distinct()

all_simul_surveyyears = all_simul_0 %>%
  right_join(dates_prevalence_surveys) %>%
  merge(scens %>%mutate(scenario_id=ID), by="scenario_id")%>%
  mutate(EIR=as.numeric(EIR)) %>%
  rename(PR="prevalenceRate",age="age_group") %>%
  mutate(EIR_num=as.numeric(EIR))

## Calibrate future simulations
## merge with calibration/EIRs to get all communes
holy_grail = EIRs %>%
  select(setting,sub,starts_with("EIR")) %>%
  pivot_longer(cols=starts_with("EIR"),names_to="EIR_type",values_to="EIR_num") %>%
  filter(EIR_type %in% c("EIR_lci","EIR","EIR_uci")) %>%
  mutate(EIR_type=ifelse(EIR_type=="EIR","middle",
                         ifelse(EIR_type=="EIR_lci","lower","upper")))

simul_surveymonths_calib = inner_join(holy_grail,all_simul_surveyyears,by=c("setting","EIR_num"))

saveRDS(simul_surveymonths_calib, "surveymonths_calibrated_simulations.Rda")
