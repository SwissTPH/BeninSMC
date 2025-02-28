#################################
# Numbers from Results section
#
# modified 26.11.2024 by Jeanne Lemant
#################################

rm(list=ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./1define_scenarios.R")

##### All necessary dataframes

### By extension (demographic versus geographic)

targeted_pop_Ext <- futrs %>% filter(year %in% 2024:2026,seed==1,EIR_type=="middle") %>%
  filter((Extension=="Demo" & scenario=="Demo" & age=="5-10" &
            sub %in% sub_Demoextension)|
           (Extension=="Geo" & scenario=="Geo" & age=="0-5" &
              sub %in% sub_Geoextension))%>%
  group_by(Extension,age,year,scenario) %>%
  summarise(pop=sum(pop)) %>%
  group_by(Extension,age,scenario) %>%
  summarise(pop=sum(pop)) %>%
  ungroup() %>%
  rename(target_pop="pop") %>% select(-age)

avertedplanned_OMcases_Ext <- casesAverted.sumAcrossYears(OMcases_df %>% select(-setting) %>%
                                                            rename(setting="Extension"),
                                                          cases_col = "OMcases",
                                                          level_aggr = "setting",
                                                          eir_col = "EIR_type",
                                                          EIR_names = c("lower","middle","upper"),
                                                          fut_cols = "scenario",
                                                          counterfactual = "planned",
                                                          year_start = 2024,
                                                          year_end = 2026) %>%
  rename(Extension="setting")

avertedplanned_nSevere_Ext <- casesAverted.sumAcrossYears(nSevere_df %>% select(-setting) %>%
                                                            rename(setting="Extension"),
                                                          cases_col = "nSevere",
                                                          level_aggr = "setting",
                                                          eir_col = "EIR_type",
                                                          EIR_names = c("lower","middle","upper"),
                                                          fut_cols = "scenario",
                                                          counterfactual = "planned",
                                                          year_start = 2024,
                                                          year_end = 2026) %>%
  rename(Extension="setting")

avertedplanned_expectedDirectDeaths_Ext <- casesAverted.sumAcrossYears(expectedDirectDeaths_df %>% select(-setting) %>%
                                                            rename(setting="Extension"),
                                                          cases_col = "expectedDirectDeaths",
                                                          level_aggr = "setting",
                                                          eir_col = "EIR_type",
                                                          EIR_names = c("lower","middle","upper"),
                                                          fut_cols = "scenario",
                                                          counterfactual = "planned",
                                                          year_start = 2024,
                                                          year_end = 2026) %>%
  rename(Extension="setting")

### Prioritisation within geographic extension

targeted_pop_geo_Admin1 <- futrs %>% 
  filter(year %in% 2024:2026,seed==1,EIR_type=="middle",
         sub %in% sub_Geoextension,scenario=="Geo",age=="0-5")%>%
  group_by(Admin1,age,year,scenario) %>%
  summarise(pop=sum(pop)) %>%
  group_by(Admin1,age,scenario) %>%
  summarise(pop=sum(pop))

avertedplanned_OMcases_Admin1 <- casesAverted.sumAcrossYears(OMcases_df %>%
                                                               select(-setting) %>%
                                                               mutate(setting=Admin1),
                                                             cases_col = "OMcases",
                                                             level_aggr = "setting",
                                                             eir_col = "EIR_type",
                                                             EIR_names = c("lower","middle","upper"),
                                                             fut_cols = "scenario",
                                                             counterfactual = "planned",
                                                             year_start = 2024,
                                                             year_end = 2026) %>%
  rename(Admin1="setting") %>%
  filter(scenario=="Geo",Admin1 %in% c("Borgou","Collines","Donga"))

avertedplanned_nSevere_Admin1 <- casesAverted.sumAcrossYears(nSevere_df %>%
                                                               select(-setting) %>%
                                                               mutate(setting=Admin1),
                                                             cases_col = "nSevere",
                                                             level_aggr = "setting",
                                                             eir_col = "EIR_type",
                                                             EIR_names = c("lower","middle","upper"),
                                                             fut_cols = "scenario",
                                                             counterfactual = "planned",
                                                             year_start = 2024,
                                                             year_end = 2026) %>%
  rename(Admin1="setting") %>%
  filter(scenario=="Geo",Admin1 %in% c("Borgou","Collines","Donga"))

avertedplanned_expectedDirectDeaths_Admin1 <- casesAverted.sumAcrossYears(expectedDirectDeaths_df %>%
                                                               select(-setting) %>%
                                                               mutate(setting=Admin1),
                                                             cases_col = "expectedDirectDeaths",
                                                             level_aggr = "setting",
                                                             eir_col = "EIR_type",
                                                             EIR_names = c("lower","middle","upper"),
                                                             fut_cols = "scenario",
                                                             counterfactual = "planned",
                                                             year_start = 2024,
                                                             year_end = 2026) %>%
  rename(Admin1="setting") %>%
  filter(scenario=="Geo",Admin1 %in% c("Borgou","Collines","Donga"))

##### Geographic versus demographic extension

#cases averted
avertedplanned_OMcases_Ext %>% filter(Extension=="Demo"&scenario=="Demo")

avertedplanned_nSevere_Ext %>% filter(Extension=="Demo"&scenario=="Demo")

avertedplanned_expectedDirectDeaths_Ext %>% filter(Extension=="Demo"&scenario=="Demo")

avertedplanned_OMcases_Ext %>% filter(Extension=="Geo"&scenario=="Geo")

avertedplanned_nSevere_Ext %>% filter(Extension=="Geo"&scenario=="Geo")

avertedplanned_expectedDirectDeaths_Ext %>% filter(Extension=="Geo"&scenario=="Geo")

#eligible population
targeted_pop_Ext %>% select(Extension,target_pop)

#division of cases averted by targeted population
right_join(avertedplanned_OMcases_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*1000)) %>%
  filter(Extension=="Demo")

right_join(avertedplanned_nSevere_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*1000)) %>%
  filter(Extension=="Demo")

right_join(avertedplanned_expectedDirectDeaths_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*10^6)) %>%
  filter(Extension=="Demo")

right_join(avertedplanned_OMcases_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*1000)) %>%
  filter(Extension=="Geo")

right_join(avertedplanned_nSevere_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*1000)) %>%
  filter(Extension=="Geo")

right_join(avertedplanned_expectedDirectDeaths_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*10^6)) %>%
  filter(Extension=="Geo")

#ratio
right_join(avertedplanned_OMcases_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*1000)) %>%
  filter(Extension=="Geo") %>% pull(ca.tot.mean) /
  right_join(avertedplanned_OMcases_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*1000)) %>%
  filter(Extension=="Demo") %>% pull(ca.tot.mean)

right_join(avertedplanned_nSevere_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*1000)) %>%
  filter(Extension=="Geo") %>% pull(ca.tot.mean) /
  right_join(avertedplanned_nSevere_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*1000)) %>%
  filter(Extension=="Demo") %>% pull(ca.tot.mean)

right_join(avertedplanned_expectedDirectDeaths_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*10^6)) %>%
  filter(Extension=="Geo") %>% pull(ca.tot.mean) /
  right_join(avertedplanned_expectedDirectDeaths_Ext,targeted_pop_Ext) %>%
  mutate(across(starts_with("ca"),~./target_pop*10^6)) %>%
  filter(Extension=="Demo") %>% pull(ca.tot.mean)

avertedplanned_nSevere_Ext %>% filter(Extension=="Geo"&scenario=="Geo") %>%
  pull(ca.tot.mean) /
  avertedplanned_nSevere_Ext %>% filter(Extension=="Demo"&scenario=="Demo") %>% 
  pull(ca.tot.mean)


##### Prioritisation among geographic extension
right_join(avertedplanned_OMcases_Admin1 %>% ungroup() %>% select(-age),
           targeted_pop_geo_Admin1) %>%
  mutate(across(starts_with("ca"),~./pop*1000)) %>% filter(Admin1=="Borgou")

right_join(avertedplanned_nSevere_Admin1 %>% ungroup() %>% select(-age),
           targeted_pop_geo_Admin1) %>%
  mutate(across(starts_with("ca"),~./pop*1000)) %>% filter(Admin1=="Borgou")

right_join(avertedplanned_expectedDirectDeaths_Admin1 %>% ungroup() %>% select(-age),
           targeted_pop_geo_Admin1) %>%
  mutate(across(starts_with("ca"),~./pop*10^6)) %>% filter(Admin1=="Borgou")

right_join(avertedplanned_OMcases_Admin1 %>% ungroup() %>% select(-age),
           targeted_pop_geo_Admin1) %>%
  mutate(across(starts_with("ca"),~./pop*1000)) %>% filter(Admin1=="Donga")

right_join(avertedplanned_nSevere_Admin1 %>% ungroup() %>% select(-age),
           targeted_pop_geo_Admin1) %>%
  mutate(across(starts_with("ca"),~./pop*1000)) %>% filter(Admin1=="Donga")

right_join(avertedplanned_expectedDirectDeaths_Admin1 %>% ungroup() %>% select(-age),
           targeted_pop_geo_Admin1) %>%
  mutate(across(starts_with("ca"),~./pop*10^6)) %>% filter(Admin1=="Donga")

