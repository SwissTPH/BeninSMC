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

# Package installation for the first time:
# first you need to clone the repositories and then:
# devtools::install("~/Git_repo/r-openMalariaUtilities/")
# devtools::install("~/Git_repo/omuaddons/")
# devtools::install("~/Git_repo/omu-slurm/")
# devtools::install("~/Git_repo/omu-compat/")

# Load the necessary packages
# library(devtools)
library(openMalariaUtilities)
library(OMAddons)
library(omuslurm)
library(omucompat)
library(tidyverse)

#####################################
# Initialization
#####################################

# Define root directory with all the experiments according to the user
if (Sys.getenv("USER") == "lemant0000") {
  root_dir_path = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMC/"
} else {
  print("Please specify the paths to the necessary folders!")
}

# Set working directory to the one where this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the base xml list setup function and auxiliary functions
source("00_create_base_future.R")

#####################################
# Experiment setup
#####################################

expName = "BeninSMC_2futuresimulations"

# Definition of the folder where all results will be stored
# clearCache
setupDirs(experimentName = expName, rootDir = root_dir_path, replace = FALSE)

# Initialize cache and create the base xml file
baseList = create_baseList(country_name = "BEN",
                                   expName = expName,
                                   sim_start = "1918-01-01",
                                   versionnum = 44L)
createBaseXml(baseList, replace = FALSE)

## Copy necessary Open Malaria files. It needs to be called after
## createBaseXml, otherwise the cache is not set up.
setupOM()

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
dat= convert_access(dat,pattern="EffCov14d",katya=F,scale=1)

# adding the EIRs from the calibration
EIRs = readRDS("../1calibration/EIRs_calibration.RDS")

calibration = EIRs %>%
  ungroup() %>%
  select(setting,EIR_lci,EIR_uci,EIR) %>%
  pivot_longer(cols=starts_with("EIR"),values_to = "EIR") %>%
  select(-name) %>%
  distinct()

# types of nets distributed in 2020 and 2023 (source: NMCP)
histITNtypes <- read.csv("../data/ITNtypes_distributions_2020_2023.csv",
                         check.names=FALSE) %>%
  dplyr::select(Admin1,Admin2,ITNtype2020,ITNtype2023) %>%
  mutate(ITNtype2020=ifelse(ITNtype2020=="standard","futITNWeak","futPBO"),
         ITNtype2023=ifelse(ITNtype2023=="standard","futITNWeak","futPBO")) %>%
  mutate(Admin2=ifelse(Admin2=="adjarra","adjara",Admin2),
         Admin2=ifelse(Admin2=="cobly","kobli",Admin2),
         Admin2=ifelse(Admin2=="copargo","kopargo",Admin2),
         Admin2=ifelse(Admin2=="oussa-pehunco","pehonko",Admin2),
         Admin2=ifelse(Admin2=="seme-kpodji","seme",Admin2),
         Admin2=ifelse(Admin2=="toucountouna","toukountouna",Admin2),
         Admin2=ifelse(Admin2=="zogbodomey","zogbodome",Admin2)) %>%
  rename(sub="Admin2") %>%
  mutate(ITNtype2023=ifelse(sub=="parakou","futPBO",ITNtype2023))

ITNtypes <- histITNtypes %>%
  left_join(EIRs %>% ungroup() %>% select(sub,setting)) %>%
  select(Admin1,ITNtype2020,ITNtype2023,setting) %>%
  distinct()

dat_calib=dat %>%
  right_join(ITNtypes) %>%
  right_join(calibration)

#####################################
# Defining the simulation scenarios
#####################################


# Define the list with all the scenario variations per country
# (population size, seed, EIR)
full             = list()
full $ seed      = 1:10
full $ setting   = unique(dat_calib$setting)
#in 2024, with 3.51% growth rate from 2013 census, smallest population is Toukoutouna with 58000 inhabitants
full $ pop       = 50000L
full$fin = .08
full$fout = .02
full$gin = .81
full$gout = .09

full$futITNtype2020="P2"
full$futITNtype2023=c("P2","DN")
full$futITNtype2026=c("futITNWeakP2","futPBOP2")
full$futITNuse=c("current","opt")

full$recentIRScov2020=c(0,.9)
full$recentIRScov2021=c(0,.9)

full$futcovSMC0to5=c(0,.8)
full$futcovSMC0to10=c(0,.8)

#### 'scens' will contain all possible combinations of these scenario variations
scens = expand.grid( full )

scens = scens %>%
  # remove simultaneous SMC
  filter(!(futcovSMC0to5>0 & futcovSMC0to10 >0))

#### selecting scenarios
settings_IRS2020=EIRs %>%
  filter(sub %in% c("kopargo","djougou",
                    "gogounou","kandi","segbana",
                    "kouande")) %>%
  pull(setting)

settings_IRS2021=EIRs %>%
  filter(sub %in% c("ouake","segbana")) %>%
  pull(setting)

settings_SMC0to10 = EIRs %>%
  filter(Admin1 %in% c("Alibori","Atacora")) %>%
  pull(setting)

#these communes received OlysetPlus nets in 2023, made of polyester
sub_OlysetPlus <- c("malanville","kandi","banikoara","gogounou")

settings_OlysetPlus = EIRs %>%
  filter(sub %in% sub_OlysetPlus) %>%
  pull(setting) %>%
  unique

#SMC only in these departments
SMC_departments <- c("Alibori","Atacora","Borgou","Donga","Collines")

scens = scens %>%
  # remove IRS where not deployed
  filter(setting %in% settings_IRS2020 | recentIRScov2020==0) %>%
  filter(setting %in% settings_IRS2021 | recentIRScov2021==0) %>%
  # remove SMC in children from 0 to 10 in Borgou, Collines and Donga
  filter(setting %in% settings_SMC0to10 | futcovSMC0to10==0) %>%
  # always SMC at least in children under 5 in Alibori and Atacora
  filter(!(setting %in% settings_SMC0to10 & futcovSMC0to10==0&futcovSMC0to5==0)) %>%
  left_join(dat_calib,by="setting")

scens = scens %>%
  mutate(futcovSMC2020=ifelse(setting %in% c("atacora.kobli","atacora.materi",
                                             "atacora.tanguieta","alibori",
                                             "alibori.irs","alibori.smc"),
                              .8,0),
         futcovSMC2021=ifelse(startsWith(setting,"atacora") |
                                startsWith(setting,"alibori"),.8,0),
         futcovSMCJun2022=ifelse(startsWith(setting,"atacora"),.8,0),
         futcovSMCJul2022=ifelse(startsWith(setting,"alibori"),.8,0),
         #same coverage as DHS 2017 or 80% (optimal)
         futITNcov=ifelse(futITNuse=="opt",.8,histITNcov2017),
         futITNcov2020=futITNcov,
         futITNcov2023=futITNcov,
         futITNcov2026=futITNcov,
         futITNtype2020=paste0(ITNtype2020,futITNtype2020),
         futITNtype2023=paste0(ITNtype2023,futITNtype2023)) %>%
  #DuraNet half-life is only for Olyset+, not for standard nets
  filter(!futITNtype2023=="futITNWeakDN") %>%
  filter(!(futITNtype2023=="futPBODN"&!setting%in%settings_OlysetPlus)) %>%
  #no going back to standard nets in 2026 if PBO in 2023
  filter(!(grepl("PBO",futITNtype2023)&futITNtype2026=="futITNWeakP2")) %>%
  # no SMC in the departments not considered for it
  filter(Admin1 %in% SMC_departments | futcovSMC0to10==0) %>%
  filter(Admin1 %in% SMC_departments | futcovSMC0to5==0)

scens %>% filter(setting=="alibori.smc",EIR==66,seed==1,
                 futITNtype2023=="futPBODN",futITNtype2026=="futPBOP2",
                 futITNuse=="current"
) %>% View

scens %>% filter(futcovSMC0to5==futcovSMC0to10,seed==1,
                 setting=="borgou",EIR==70,futITNuse=="current") %>% View

scens %>% filter(seed==1,setting=="littoral",futITNuse=="current",EIR==16) %>% View


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
#####################################
print ("Generating analysis scripts ...")
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
                        time = "04:00:00", qos = "6hours",
                        rModule = "R/4.2.1-foss-2022a",
                        omModule = "OpenMalaria/44.0-intel-compilers-2023.1.0"
)

# # If not all OpenMalaria simulations have run
# 
# ## find those missing
# missing <- read.table(paste0(experiment_folder,"/missing_om_simulations.txt")) %>%
#   rename(ID = "V1") %>%
#   mutate(ID = gsub(paste0(expName,"_"),"",ID)) %>%
#   mutate(ID = as.double(gsub("_out.txt","",ID)))
# #paste0(missing$ID,collapse=",")
# scens=readRDS(file.path(experiment_folder, "cache/scenarios.rds"))
# 
# ## new bash file only for those missing
# scens_missing <- missing %>% left_join(scens)
# 
# slurmPrepareSimulations(expName = expName, scenarios = scens_missing,
#                         memCPU = "10GB", nCPU =50,bSize = 200,
#                         time = "04:00:00", qos = "6hours",
#                         rModule = "R/4.2.1-foss-2022a",
#                         omModule = "OpenMalaria/44.0-intel-compilers-2023.1.0"
# )

## 3. Prepare the scripts for post-processing the OpenMalaria outputs
# Define the age groups of interest for the outputs (including aggregations)
age_groups_list = c("0-5","2-10","0-10","5-10", "0-100","10-100")

# Define the OpenMalaria outputs of interest; these will correspond to the
# columns of the results table to be stored in the database
results_columns = c("scenario_id",
                    "date", "age_group", "date_aggregation",
                    "nTreatments1", "nTreatments2", "nTreatments3",
                    "nHost", "nUncomp", "nSevere",
                    "tUncomp", "tSevere","expectedDirectDeaths",
                    "incidenceRate", "prevalenceRate")

# Remove the results database if it already exists
# Overwriting an existing database with the same scenario IDs will not work
db_file = file.path(paste0(getCache("experimentDir"),"/", expName, ".sqlite"))
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
