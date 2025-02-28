############################
# Script which performs the calibration step
#
# modified 15.11.2024 by Jeanne Lemant
############################
rm(list=ls())

# Load packages
library(tidyverse)
library(RSQLite)

#####################################
# Initialization
#####################################


# Define root directory with all the experiments according to the user
if (Sys.getenv("USER") == "lemant0000") {
  root_dir_path = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper/"
  } else {
  print("Please specify the paths to the necessary folders!")
}

expName = "BeninSMC_1calibration"
experiment_folder = paste0(root_dir_path, expName)

# Set working directory to the one where this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source helper functions
source("../helper_functions/helper_functions_calibration.R")

# Load fitdat

#### correspondence between settings and commune (Admin2) names

correspondence_names = read.csv(
  file.path("../data/BEN_admin1_admin2_names.csv"))

correspondence_communes_settings = read.csv(
  file.path("../data/correspondence_communes_to_IRS_SMC_settings.csv"))

correspondence_communes_settings = correspondence_communes_settings%>%
  mutate(sub=tolower(Admin2))

good_names <- rbind(correspondence_communes_settings,
                    correspondence_names %>%
                      filter(!(sub %in% correspondence_communes_settings$sub)))

filename  = "PfPR_2-10_sd_Benin_Admin2_2000-2019_MAP_global.csv"

fitdat=read.csv(file.path("../data/PfPR_2-10_sd_Benin_Admin2_2000-2019_MAP_global.csv")) %>%
  rename("prevalenceRate_obs"=PR) %>%
  left_join(good_names) %>%
  mutate_at(vars(prevalenceRate_obs,sd,LCI,UCI,sd_low,sd_high),~./100) %>%
  mutate(age="2-10") %>%
  select(-c(Admin2,source)) %>%
  mutate(year = as.numeric(year))

#Calibration parameters
lambda = 1
select_subset = 0.5

calibration_spec = list(
  cols_observed = "prevalenceRate_obs",
  cols_simulated = "prevalenceRate"
)

# Read the results table from the database:
# open database connection:
conn = DBI::dbConnect(RSQLite::SQLite(), paste0(root_dir_path,expName,".sqlite"))
# extract the results table, this should have the same name as provided to slurmPrepareResults
all_simul = dbReadTable(conn, "om_results")
# always disconnect from the database
DBI::dbDisconnect(conn)

scens = readRDS(file.path(experiment_folder,"/cache/scenarios.rds"))

scens$scenario_id = scens$ID
all_simul_merged = merge(all_simul, scens, by = c("scenario_id")) %>%
  mutate(year = as.numeric(format(as.Date(date), format = "%Y")),
         EIR = as.numeric(EIR),
         EIR_num = as.numeric(EIR),
         prevalenceRate = as.numeric(prevalenceRate),
         age = age_group)

simul_fit = fitdat %>%
  select(setting,sub,age,year) %>%
  left_join(all_simul_merged) %>%
  filter(year>=2006, year <= 2019)


cali_output = calibrate_country(simul_fit ,
                                fitdat %>% filter(year>=2006),
                                calibration_spec = calibration_spec,
                                confidence = 0.95, lambda = lambda,
                                select_subset = select_subset,
                                llk_type = "gaussian",
                                min_EIRnb_to_keep = 10,
                                min_possible_EIR = 1,
                                max_possible_EIR =  250,
                                lower_threshold = 1,
                                upper_threshold = 250,
                                upper_threshold2 = 240)

EIRs_calibration <- cali_output$best_EIRs_ci %>%
  #put max simulated EIR as upper bound
  mutate(EIR_uci=ifelse(is.na(EIR_uci),250,EIR_uci)) %>%
  left_join(good_names)


saveRDS(EIRs_calibration,"EIRs_calibration.RDS")

## Plots of calibration by commune

holy_grail = EIRs_calibration %>%
  select(setting,sub,starts_with("EIR")) %>%
  pivot_longer(cols=starts_with("EIR"),names_to="EIR_type",values_to="EIR_num") %>%
  filter(EIR_type %in% c("EIR_lci","EIR","EIR_uci")) %>%
  mutate(EIR_type=ifelse(EIR_type=="EIR","middle",
                         ifelse(EIR_type=="EIR_lci","lower","upper")))

past_simuls_calib = all_simul_merged %>%
  select(setting, year, age, EIR_num, prevalenceRate, Admin1, seed) %>%
  inner_join(holy_grail,by=c("setting","EIR_num")) %>%
  select(-EIR_num) %>%
  pivot_wider(names_from = "EIR_type", values_from = "prevalenceRate", names_prefix = "PR_")

color_map <- rgb(78,176,155,maxColorValue = 255)

for (Department in unique(good_names$Admin1)){
  calibration_Dpt <- ggplot(past_simuls_calib %>% filter(Admin1 == Department, year <2020))+
  geom_line(aes(x = year, y = PR_lower, group = seed, color = "C"))+
  geom_line(aes(x = year, y = PR_upper, group = seed, color = "C"))+
  geom_line(aes(x = year, y = PR_middle, group = seed, color = "B"))+
  geom_pointrange(data = fitdat %>% filter(Admin1 == Department, year >= 2006),
                  aes(x = year, y = prevalenceRate_obs,
                                     ymin = LCI, ymax = UCI, color = "A"),
                  size=1,stroke=1,lwd=1,shape=21,fill="white")+
  labs(y="Malaria prevalence in children aged 2 to 10",x="Year")+
  scale_color_manual(values = c(color_map,"black","darkgrey"),
                     name = "",
                     labels = c("Estimates from MAP used for calibration",
                                "Simulations from EIR point estimate",
                                "Simulations from EIR lower and upper bound"))+
  facet_wrap(sub~.)+
  theme_minimal(base_size=20)+
  scale_y_continuous(labels=scales::percent,limits=c(0,1))+
  theme(legend.position = "bottom")
  
  ggsave(calibration_Dpt,filename=paste0(root_dir_path,"fits_calibration/",
                           "fit_",Department,".png")
         ,width = 22,height=12
  )
}
