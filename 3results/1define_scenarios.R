#################################
# Weight simulated cases by real population and define scenarios
#
# modified 22.11.2024 by Jeanne Lemant
#################################

library(tidyverse)
library(readxl)

rm(list=ls())

maindir <- "C:/Users/lemaje/switchdrive/Institution/AIM/7. Internal manuscripts/BEN_SMC/"

## raw calibrated futures
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
simuls = readRDS("../2future_simulations/calibrated_simulations.Rda") %>%
  filter(year<2028)

simuls$OMcases = simuls$nUncomp+simuls$nSevere

## population
unique(simuls$age)

simulated_pop = simuls %>% filter(age=="0-100") %>% distinct(nHost) %>% pull()

pop_projected <- read.csv("../data/commune_pop_from2013census_3.51growth_rate.csv", sep = ";") %>%
  rename(sub="name")

pop_projected_long <- pop_projected %>%
  pivot_longer(cols=pop2013:pop2000,names_to = "year",values_to = "pop_All",
               names_prefix = "pop") %>%
  mutate(year=as.numeric(year))

pop_w <- simuls %>%
  left_join(pop_projected_long) %>%
  mutate(pop = pop_All*nHost/simulated_pop) %>%
  mutate(across(starts_with(c("nTreat","expectedDirectDeaths",
                              "tSevere","tSimple","nSevere","OMcases")),
                ~.*pop/nHost)) %>%
  group_by(sub,setting,seed,year,age) %>%
  mutate(pop = mean(pop)) %>%
  select(-pop_All)

popU5 <- pop_w %>% filter(age == "0-5") %>% pull(nHost)
sort(unique(popU5/simulated_pop))
popU10 <- pop_w %>% filter(age == "0-10") %>% pull(nHost)
sort(unique(popU10/simulated_pop))
(mean(popU10) - mean(popU5))/simulated_pop

## define plans

futrs = pop_w %>%
  #baseline: PBO nets in 2026, SMC under 5 in Alibori and Atacora
  mutate(baseline = (futITNtype2026 == "futPBOP2"&
                  !(Admin1%in% c("Alibori","Atacora")&futcovSMC0to5 == 0)&
                  (Admin1%in% c("Alibori","Atacora")|(futcovSMC0to5 == 0)))) %>%
  #Demographic extension: same nets in 2026 as in 2023, SMC under 10 in Alibori and Atacora
  mutate(Demo = (futITNtype2026 == "futPBOP2"&
                Admin1%in% c("Alibori","Atacora")&futcovSMC0to10>0)) %>%
  #Geographic extension: same nets in 2026 as in 2023, SMC under 5 everywhere
  mutate(Geo = (futITNtype2026 == "futPBOP2"&
                futcovSMC0to5>0)) %>%
  group_by(year,age) %>%
  pivot_longer(cols = baseline:Geo,names_to = "scenario") %>%
  filter(value)

# futrs %>% filter(year==2023,seed==1,age=="0-5",scenario=="BaU",
#                  EIR_type=="middle") %>% View


### AGGREGATE 

source("helper_functions_results.R")

names_ZS <- readxl::read_xlsx("../data/sanitary_zones.xlsx",
range = "A1:D81") %>%
  distinct() %>%
  rename(Admin1 = Departements,
         nameZS = (`Zones sanitaires`)) %>%
  mutate(sub = tolower(Communes)) %>%
  filter(!(Admin1 == "Littoral"&sub != "cotonou 5")) %>%
  mutate(ZS = ifelse(Admin1 == "Littoral","COT",Abbreviations)) %>%
  mutate(nameZS = ifelse(ZS == "COT","Cotonou",nameZS),
         sub = ifelse(ZS == "COT","cotonou",sub)) %>%
  select(Admin1,sub,nameZS,ZS) %>%
  mutate(ZS1st_low = gsub("-.*","",nameZS))

futrs <- futrs %>% left_join(names_ZS)

futrs <- futrs %>%
  mutate(Extension = case_when(Admin1 %in% c("Alibori","Atacora")~"Demo",
                               Admin1 %in% c("Borgou", "Donga", "Collines")~"Geo",
                               TRUE ~ NA))

##### Data frames split by indicator or age

PfPR_df <- futrs %>% filter(age=="0-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,PR,scenario,pop)

PfPR_dfU5 <- futrs %>% filter(age=="0-5") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,PR,scenario,pop)


nSevere_df <- futrs %>% filter(age=="0-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,nSevere,scenario,pop)

OMcases_df <- futrs %>% filter(age=="0-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,OMcases,scenario,pop)

OMcases_dfU5 <- futrs %>% filter(age=="0-5") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,OMcases,scenario,pop)

OMcases_dfU10 <- futrs %>% filter(age=="0-10") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,OMcases,scenario,pop)

OMcases_df5to10 <- futrs %>% filter(age=="5-10") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,OMcases,scenario,pop)

OMcases_df10to100 <- futrs %>% filter(age=="10-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,OMcases,scenario,pop)

#nTreat
nTreat_df <- futrs %>% filter(age=="0-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,nTreat,scenario,pop)

nTreat_dfU5 <- futrs %>% filter(age=="0-5") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,nTreat,scenario,pop)

nTreat_dfU10 <- futrs %>% filter(age=="0-10") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,nTreat,scenario,pop)

nTreat_df5to10 <- futrs %>% filter(age=="5-10") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,nTreat,scenario,pop)

nTreat_df10to100 <- futrs %>% filter(age=="10-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,nTreat,scenario,pop)

#tSevere
tSevere_df <- futrs %>% filter(age=="0-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,tSevere,scenario,pop)

tSevere_dfU5 <- futrs %>% filter(age=="0-5") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,tSevere,scenario,pop)

tSevere_dfU10 <- futrs %>% filter(age=="0-10") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,tSevere,scenario,pop)

tSevere_df5to10 <- futrs %>% filter(age=="5-10") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,tSevere,scenario,pop)

tSevere_df10to100 <- futrs %>% filter(age=="10-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,tSevere,scenario,pop)

#nSevere
nSevere_df <- futrs %>% filter(age=="0-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,nSevere,scenario,pop)

nSevere_dfU5 <- futrs %>% filter(age=="0-5") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,nSevere,scenario,pop)

nSevere_dfU10 <- futrs %>% filter(age=="0-10") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,nSevere,scenario,pop)

nSevere_df5to10 <- futrs %>% filter(age=="5-10") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,nSevere,scenario,pop)

nSevere_df10to100 <- futrs %>% filter(age=="10-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,nSevere,scenario,pop)

#deaths
expectedDirectDeaths_df <- futrs %>% filter(age=="0-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,expectedDirectDeaths,scenario,pop)

expectedDirectDeaths_dfU5 <- futrs %>% filter(age=="0-5") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,expectedDirectDeaths,scenario,pop)

expectedDirectDeaths_dfU10 <- futrs %>% filter(age=="0-10") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,expectedDirectDeaths,scenario,pop)

expectedDirectDeaths_df5to10 <- futrs %>% filter(age=="5-10") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,expectedDirectDeaths,scenario,pop)

expectedDirectDeaths_df10to100 <- futrs %>% filter(age=="10-100") %>%
  select(setting,sub,Admin1,ZS,Extension,EIR_type,seed,year,age,expectedDirectDeaths,scenario,pop)

