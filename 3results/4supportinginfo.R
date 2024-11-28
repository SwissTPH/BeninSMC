rm(list=ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./1define_scenarios.R")

##### All necessary dataframes

###### OMcases

OMcases_ExtU5 <- computeAggrTimeSeriesCasesInc(allSims=OMcases_dfU5 %>% select(-setting) %>%
                                                 rename(setting="Extension"),
                                               cases_col = "OMcases",
                                               level_aggr = "setting",
                                               pop_col = "pop",eir_col = "EIR_type",
                                               EIR_names = c("lower","middle","upper"),
                                               fut_cols = "scenario",convert_pop = FALSE
) %>%
  rename(Extension="setting")

OMcases_Ext5to10 <- computeAggrTimeSeriesCasesInc(allSims=OMcases_df5to10 %>% select(-setting) %>%
                                                    rename(setting="Extension"),
                                                  cases_col = "OMcases",
                                                  level_aggr = "setting",
                                                  pop_col = "pop",eir_col = "EIR_type",
                                                  EIR_names = c("lower","middle","upper"),
                                                  fut_cols = "scenario",convert_pop = FALSE
) %>%
  rename(Extension="setting")

effectbaseline_OMcases5to10_Ext3years <- percentReductionInc.acrossScenarios(OMcases_df5to10 %>% select(-setting) %>%
                                                                              rename(setting="Extension") %>%
                                                                              group_by(sub,setting,Admin1,ZS,
                                                                                       EIR_type,seed,scenario,age) %>%
                                                                              filter(year %in% 2024:2026) %>%
                                                                              summarise(OMcases=sum(OMcases),
                                                                                        pop=sum(pop)) %>%
                                                                              mutate(year=2024),
                                                                            cases_col = "OMcases",
                                                                            level_aggr = "setting",
                                                                            eir_col = "EIR_type",
                                                                            EIR_names = c("lower","middle","upper"),
                                                                            fut_cols = "scenario",
                                                                            counterfactual = "baseline",
                                                                            pop_col = "pop",
                                                                            comparison_year = 2024) %>%
  rename(Extension="setting")

effectbaseline_OMcases_Ext3years <- percentReductionInc.acrossScenarios(OMcases_df %>% select(-setting) %>%
                                                                         rename(setting="Extension") %>%
                                                                         group_by(sub,setting,Admin1,ZS,
                                                                                  EIR_type,seed,scenario,age) %>%
                                                                         filter(year %in% 2024:2026) %>%
                                                                         summarise(OMcases=sum(OMcases),
                                                                                   pop=sum(pop)) %>%
                                                                         mutate(year=2024),
                                                                       cases_col = "OMcases",
                                                                       level_aggr = "setting",
                                                                       eir_col = "EIR_type",
                                                                       EIR_names = c("lower","middle","upper"),
                                                                       fut_cols = "scenario",
                                                                       counterfactual = "baseline",
                                                                       pop_col = "pop",
                                                                       comparison_year = 2024) %>%
  rename(Extension="setting")

effectbaseline_OMcasesU5_Ext3years<- percentReductionInc.acrossScenarios(OMcases_dfU5 %>% select(-setting) %>%
                                                                          rename(setting="Extension") %>%
                                                                          group_by(sub,setting,Admin1,ZS,
                                                                                   EIR_type,seed,scenario,age) %>%
                                                                          filter(year %in% 2024:2026) %>%
                                                                          summarise(OMcases=sum(OMcases),
                                                                                    pop=sum(pop)) %>%
                                                                          mutate(year=2024),
                                                                        cases_col = "OMcases",
                                                                        level_aggr = "setting",
                                                                        eir_col = "EIR_type",
                                                                        EIR_names = c("lower","middle","upper"),
                                                                        fut_cols = "scenario",
                                                                        counterfactual = "baseline",
                                                                        pop_col = "pop",
                                                                        comparison_year = 2024) %>%
  rename(Extension="setting")

####### nSevere

effectbaseline_nSevere5to10_Ext3years <- percentReductionInc.acrossScenarios(nSevere_df5to10 %>% select(-setting) %>%
                                                                              rename(setting="Extension") %>%
                                                                              group_by(sub,setting,Admin1,ZS,
                                                                                       EIR_type,seed,scenario,age) %>%
                                                                              filter(year %in% 2024:2026) %>%
                                                                              summarise(nSevere=sum(nSevere),
                                                                                        pop=sum(pop)) %>%
                                                                              mutate(year=2024),
                                                                            cases_col = "nSevere",
                                                                            level_aggr = "setting",
                                                                            eir_col = "EIR_type",
                                                                            EIR_names = c("lower","middle","upper"),
                                                                            fut_cols = "scenario",
                                                                            counterfactual = "baseline",
                                                                            pop_col = "pop",
                                                                            comparison_year = 2024) %>%
  rename(Extension="setting")

effectbaseline_nSevere_Ext3years <- percentReductionInc.acrossScenarios(nSevere_df %>% select(-setting) %>%
                                                                         rename(setting="Extension") %>%
                                                                         group_by(sub,setting,Admin1,ZS,
                                                                                  EIR_type,seed,scenario,age) %>%
                                                                         filter(year %in% 2024:2026) %>%
                                                                         summarise(nSevere=sum(nSevere),
                                                                                   pop=sum(pop)) %>%
                                                                         mutate(year=2024),
                                                                       cases_col = "nSevere",
                                                                       level_aggr = "setting",
                                                                       eir_col = "EIR_type",
                                                                       EIR_names = c("lower","middle","upper"),
                                                                       fut_cols = "scenario",
                                                                       counterfactual = "baseline",
                                                                       pop_col = "pop",
                                                                       comparison_year = 2024) %>%
  rename(Extension="setting")

effectbaseline_nSevereU5_Ext3years <- percentReductionInc.acrossScenarios(nSevere_dfU5 %>% select(-setting) %>%
                                                                           rename(setting="Extension") %>%
                                                                           group_by(sub,setting,Admin1,ZS,
                                                                                    EIR_type,seed,scenario,age) %>%
                                                                           filter(year %in% 2024:2026) %>%
                                                                           summarise(nSevere=sum(nSevere),
                                                                                     pop=sum(pop)) %>%
                                                                           mutate(year=2024),
                                                                         cases_col = "nSevere",
                                                                         level_aggr = "setting",
                                                                         eir_col = "EIR_type",
                                                                         EIR_names = c("lower","middle","upper"),
                                                                         fut_cols = "scenario",
                                                                         counterfactual = "baseline",
                                                                         pop_col = "pop",
                                                                         comparison_year = 2024) %>%
  rename(Extension="setting")

##### Effect sizes

####### Demographic extension

#cases in children from 5 to 10
OMcases_Ext5to10 %>% filter(Extension=="Demo"&scenario%in%c("Demo","baseline")) %>%
  filter(year %in% 2024:2026) %>%
  group_by(Extension,scenario,age) %>%
  summarise(across(starts_with("totCases"),sum))

#effect size over three years compared to baseline in children from 5 to 10
effectbaseline_OMcases5to10_Ext3years %>% filter(Extension=="Demo"&scenario=="Demo")

effectbaseline_nSevere5to10_Ext3years %>% filter(Extension=="Demo"&scenario=="Demo")

####### Geographic extension

#cases in children under 5
OMcases_ExtU5 %>% filter(Extension=="Geo"&scenario%in%c("Geo","baseline")) %>%
  filter(year %in% 2024:2026) %>%
  group_by(Extension,scenario,age) %>%
  summarise(across(starts_with("totCases"),sum))

#effect size over three years compared to baseline in children under 5
effectbaseline_OMcasesU5_Ext3years %>% filter(Extension=="Geo"&scenario=="Geo")

effectbaseline_nSevereU5_Ext3years %>% filter(Extension=="Geo"&scenario=="Geo")

####### Both extensions, whole population

#effect size over three years compared to baseline in whole population
effectbaseline_OMcases_Ext3years %>% filter(Extension=="Demo"&scenario=="Demo")

effectbaseline_nSevere_Ext3years %>% filter(Extension=="Demo"&scenario=="Demo")

effectbaseline_OMcases_Ext3years %>% filter(Extension=="Geo"&scenario=="Geo")

effectbaseline_nSevere_Ext3years %>% filter(Extension=="Geo"&scenario=="Geo")

