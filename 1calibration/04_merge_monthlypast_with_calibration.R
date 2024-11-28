#################################
# Load past simulations and merge them with calibrated EIRs
# 
# created 26.11.2024 by Jeanne Lemant
#################################

# History cleanup
rm(list=ls())

# Load the necessary packages
library(RSQLite)
library(tidyverse)

# Define root directory with all the experiments according to the user
if (Sys.getenv("USER") == "lemant0000") {
  root_dir_path = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMC/"
} else {
  print("Please specify the paths to the necessary folders!")
}

# Set working directory to the one where this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

expName = "BeninSMC_1calibration"

experiment_folder = paste0(root_dir_path, expName)

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
