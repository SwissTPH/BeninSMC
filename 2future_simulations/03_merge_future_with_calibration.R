#################################
# Load future simulations and merge them with calibrated EIRs
# 
# created 21.11.2024 by Jeanne Lemant
#################################

# History cleanup
rm(list=ls())

# Load the necessary packages
library(tidyverse)
library(RSQLite)

# Define root directory with all the experiments according to the user
if (Sys.getenv("USER") == "lemant0000") {
  root_dir_path = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper/"
} else {
  print("Please specify the paths to the necessary folders!")
}

# Set working directory to the one where this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

expName = "BeninSMC_2futuresimulations"

experiment_folder = paste0(root_dir_path, expName)

# extract simulations

# open database connection
conn = DBI::dbConnect(RSQLite::SQLite(),paste0(root_dir_path,expName,".sqlite"))
# list the tables of the database
DBI::dbListTables(conn)
# extract the results table, this should have the same name as provided to slurmPrepareResults
all_simul_0 = DBI::dbReadTable(conn, "om_results")
# always disconnect from the database
DBI::dbDisconnect(conn)

# get scens object to join simulations and scenarios by ID
scens=readRDS(file.path(experiment_folder, "cache/scenarios.rds"))

# get the EIRs from the calibration
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


all_simul=merge(all_simul_0, scens %>%mutate(scenario_id=ID), by="scenario_id")%>%
  mutate(EIR=as.numeric(EIR),
         year=as.numeric(format(as.Date(date), "%y"))+2000) %>%
  rename(PR="prevalenceRate",incidence="incidenceRate",
         age="age_group") %>%
  mutate(EIR_num=as.numeric(EIR))
rm(all_simul_0)

## Calibrate future simulations
## merge with calibration/EIRs to get all communes
holy_grail = EIRs %>%
  select(setting,sub,starts_with("EIR")) %>%
  pivot_longer(cols=starts_with("EIR"),names_to="EIR_type",values_to="EIR_num") %>%
  filter(EIR_type %in% c("EIR_lci","EIR","EIR_uci")) %>%
  mutate(EIR_type=ifelse(EIR_type=="EIR","middle",
                         ifelse(EIR_type=="EIR_lci","lower","upper")))

all_simul$nTreat=all_simul$nTreatments1+all_simul$nTreatments2+
  all_simul$nTreatments3

simul_calib=inner_join(holy_grail,all_simul,by=c("setting","EIR_num")) %>%
  select(-all_of(c("nTreatments1","nTreatments2","nTreatments3")))
rm(all_simul)

ggplot(simul_calib %>% filter(age=="0-5",sub=="parakou",
                              seed==1,EIR_type=="middle",
                              futITNtype2023=="futPBOP2",
                              futITNuse=="current"),
       aes(x=year,y=PR,color=futcovSMC0to5,group=futcovSMC0to5))+
  geom_line()+
  xlim(2020,2030)+
  facet_wrap(futITNtype2026~.)

### Filter for communes which received or should receive (or not) IRS, SMC or PMC
sub_IRS2020 = c("kopargo","djougou","gogounou","kandi","segbana","kouande")

sub_IRS2021 = c("ouake","segbana")

sub_SMC2021 = c("kobli","materi","tanguieta",
                "toukountouna","natitingou","boukoumbe",
                "kerou","kouande","pehonko",
                "banikoara","gogounou","kandi",
                "karimama","malanville","segbana")

sub_PMCSep2022 = c("bohicon","zogbodome","za-kpota")

sub_PMCMar2023 = c("lalo","toviklin","klouekanme",
                   "bembereke","sinende")

#these communes received OlysetPlus nets in 2023, made of polyester
sub_OlysetPlus <- c("malanville","kandi","banikoara","gogounou")

simul_calib <- simul_calib %>%
  filter(futITNuse=="current") %>% #to remove once rerun
  ungroup() %>%
  filter((sub %in% sub_IRS2020 & recentIRScov2020!=0)|
           (!(sub %in% sub_IRS2020) & recentIRScov2020==0)) %>%
  filter((sub %in% sub_IRS2021 & recentIRScov2021!=0)|
           (!(sub %in% sub_IRS2021) & recentIRScov2021==0)) %>%
  right_join(histITNtypes) %>%
  filter(!(futITNtype2023=="futPBODN"&!sub%in%sub_OlysetPlus)) %>%
  filter(!(futITNtype2023!="futPBODN"&sub%in%sub_OlysetPlus)) %>%
  filter(!(futPMCSep2022cov>0&!sub%in%sub_PMCSep2022)) %>%
  filter(!(futPMCMar2023cov>0&!sub%in%sub_PMCMar2023))

simul_calib %>% filter(year==2024,seed==1,EIR_type=="lower",age=="0-5",
                       futcovSMC0to5>0,futITNtype2026=="futPBOP2") %>% View

ggplot(simul_calib %>% filter(age=="0-5",sub=="parakou",
                              seed==1,EIR_type=="middle"),
       aes(x=year,y=PR,color=futcovSMC0to5,group=futcovSMC0to5))+
  geom_line()+
  xlim(2000,2027)+
  facet_wrap(futITNtype2026~.)

ggplot(simul_calib %>% filter(age=="0-5",sub=="bohicon",
                              seed==1,EIR_type=="middle"),
       aes(x=year,y=PR,color=futPMCSep2022cov,group=futPMCSep2022cov))+
  geom_line()+
  xlim(2000,2027)+
  facet_wrap(futITNtype2026~.)

saveRDS(simul_calib, "calibrated_simulations.Rda")
