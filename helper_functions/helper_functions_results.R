######################################################################################
## Project: MOZ NSP Support
## Script Purpose: auxiliar functions to run analysis in report
## Date: 11.01.2023
## Author: Tatiana Alonso Amor
######################################################################################


# LOAD LIBRARIES ----------------------------------------------------------
library(tidyverse)

# Dummy data to test things (as in documentation) -------------------------
# nSim = 10000
# setA.1 = data.frame(sub = rep("A.1", 18)
#                     , setting = rep("A", 18)
#                     , year = rep(seq(2023, 2025), 3)
#                     , seed = rep(c(rep(1,3), rep(2,3), rep(3,3)),2)
#                     , fut = as.factor(c(rep(1, 9), rep(2, 9)))
#                     , PR = c(0.4, 0.4, 0.4, 0.41, 0.41, 0.41,0.39,0.39,0.39, 0.4, 0.35, 0.3, 0.41, 0.36, 0.31, 0.39, 0.34, 0.29)
#                     , inc = c(400,400,400,410,410,410,390,390,390,400,350,300,410,360,310,390,340,290)
#                     , totPop = rep(100000, 18)
#                     , EIR = "middle") %>% mutate(casesSim = inc*nSim/1000)
# 
# setA.1_low = setA.1 %>% mutate(EIR = "low"
#                                , PR = 0.9*PR
#                                , inc = 0.9*inc
#                                , casesSim = round(0.9*casesSim))
# 
# setA.1_high = setA.1 %>% mutate(EIR = "high"
#                                 , PR = 1.1*PR
#                                 , inc = 1.1*inc
#                                 , casesSim = round(1.1*casesSim))
# 
# setA.2 = data.frame(sub = rep("A.2", 18)
#                     , setting = rep("A", 18)
#                     , year = rep(seq(2023, 2025), 3)
#                     , seed = rep(c(rep(1,3), rep(2,3), rep(3,3)),2)
#                     , fut = as.factor(c(rep(1, 9), rep(2, 9)))
#                     , PR = 0.5*c(0.4, 0.4, 0.4, 0.41, 0.41, 0.41,0.39,0.39,0.39, 0.4, 0.35, 0.3, 0.41, 0.36, 0.31, 0.39, 0.34, 0.29)
#                     , inc = 0.5*c(400,400,400,410,410,410,390,390,390,400,350,300,410,360,310,390,340,290)
#                     , totPop = 0.7*rep(100000, 18)
#                     , EIR = "middle") %>% mutate(casesSim = inc*nSim/1000)
# 
# 
# setA.2_low = setA.2 %>% mutate(EIR = "low"
#                                , PR = 0.9*PR
#                                , inc = 0.9*inc
#                                , casesSim = round(0.9*casesSim))
# 
# setA.2_high = setA.2 %>% mutate(EIR = "high"
#                                 , PR = 1.1*PR
#                                 , inc = 1.1*inc
#                                 , casesSim = round(1.1*casesSim))
# 
# setB.1 = data.frame(sub = rep("B.1", 18)
#                     , setting = rep("B", 18)
#                     , year = rep(seq(2023, 2025), 3)
#                     , seed = rep(c(rep(1,3), rep(2,3), rep(3,3)),2)
#                     , fut = as.factor(c(rep(1, 9), rep(2, 9)))
#                     , PR = 0.7*c(0.4, 0.4, 0.4, 0.41, 0.41, 0.41,0.39,0.39,0.39, 0.4, 0.35, 0.3, 0.41, 0.36, 0.31, 0.39, 0.34, 0.29)
#                     , inc = 0.7*c(400,400,400,410,410,410,390,390,390,400,350,300,410,360,310,390,340,290)
#                     , totPop = 0.5*rep(100000, 18)
#                     , EIR = "middle") %>% mutate(casesSim = inc*nSim/1000)
# 
# setB.1_low = setB.1 %>% mutate(EIR = "low"
#                                , PR = 0.9*PR
#                                , inc = 0.9*inc
#                                , casesSim = round(0.9*casesSim))
# 
# setB.1_high = setB.1 %>% mutate(EIR = "high"
#                                 , PR = 1.1*PR
#                                 , inc = 1.1*inc
#                                 , casesSim = round(1.1*casesSim))
# 
# allSims = rbind(setA.1, setA.1_high, setA.1_low, setA.2, setA.2_high, setA.2_low, setB.1, setB.1_low, setB.1_high)


# FUNCTIONS AGGR TIME SERIES ----------------------------------------------
## PREVALENCE
computeAggrTimeSeriesPR <- function(allSims
                                    , pr_col = "PR"
                                    , level_aggr = "setting"
                                    , pop_col = "totPop"
                                    , eir_col = "EIR"
                                    , fut_cols = "fut"
                                    , EIR_names = c("low","middle","high")
                                    , id_cols = c("sub", "setting", "seed", "year", "age", pop_col, fut_cols)){
  
  
  
  simsSub = allSims %>% pivot_wider(names_from = eir_col
                                    , values_from = pr_col
                                    , id_cols = id_cols
                                    , names_expand = TRUE) %>%
    group_by(across(id_cols[id_cols != "seed"])) %>% 
    mutate(  ind_inf = min(get(EIR_names[1]), get(EIR_names[2]), get(EIR_names[3]))
             , ind_sup = max(get(EIR_names[1]), get(EIR_names[2]), get(EIR_names[3]))
             , ind_mean = mean(middle))
  if (level_aggr == "sub"){
    aggrDF = simsSub %>% group_by(across(id_cols[!(id_cols %in% c("seed", "sub", pop_col))])) %>%
      dplyr::summarise(ind_mean = weighted.mean(ind_mean, get(pop_col))
                       , ind_inf = weighted.mean(ind_inf, get(pop_col))
                       , ind_sup = weighted.mean(ind_sup, get(pop_col)))  
  } 
  if (level_aggr == "setting"){
    aggrDF = simsSub %>% group_by(across(id_cols[!(id_cols %in% c("seed", "sub", pop_col))])) %>%
      dplyr::summarise(ind_mean = weighted.mean(ind_mean, get(pop_col))
                       , ind_inf = weighted.mean(ind_inf, get(pop_col))
                       , ind_sup = weighted.mean(ind_sup, get(pop_col)))  
  } 
  if (level_aggr == "national"){
    aggrDF = simsSub %>% group_by(across(id_cols[!(id_cols %in% c("seed", "sub", "setting",pop_col))])) %>%
      dplyr::summarise(ind_mean = weighted.mean(ind_mean, get(pop_col))
                       , ind_inf = weighted.mean(ind_inf, get(pop_col))
                       , ind_sup = weighted.mean(ind_sup, get(pop_col)))  
  }  
  return(list(simsSub, aggrDF))
}



#check
# computeAggrTimeSeriesPR(allSims)
# computeAggrTimeSeriesPR(allSims, level_aggr = "national")
# 

## CASES
computeAggrTimeSeriesCasesInc <- function(allSims
                                          , cases_col = "cases"
                                          , level_aggr = "setting"
                                          , pop_col = "totPop"
                                          , ageProp_col = "ageProp"
                                          , eir_col = "EIR"
                                          , EIR_names = c("low","middle","high")
                                          , fut_cols = "fut"
                                          , convert_pop = TRUE){
  if (convert_pop){
    allSims =  allSims %>% mutate(pop.age = get(ageProp_col)*get(pop_col))
  } else{
    allSims = allSims %>% rename(pop.age = pop_col)
  }
  
  if (level_aggr == "setting"){ 
    aggrDF = allSims %>% group_by(across(c("setting", "seed", "year", fut_cols, eir_col, "age"))) %>%
      dplyr::summarise(totCases = sum(get(cases_col))
                       , totPop.prov = sum(pop.age)
                       , inc.prov = 1000*totCases/totPop.prov) %>% 
      pivot_wider(names_from = eir_col, values_from = c(totCases, inc.prov)) %>% 
      group_by(across(c("setting", fut_cols, "year", "age"))) %>% 
      dplyr::summarise(totCases.mean = mean(get(paste0("totCases_",EIR_names[2])))
                       , totCases.sup = max(get(paste0("totCases_",EIR_names[3])),
                                            get(paste0("totCases_",EIR_names[2])),
                                            get(paste0("totCases_",EIR_names[1])))
                       , totCases.inf = min(get(paste0("totCases_",EIR_names[3])),
                                            get(paste0("totCases_",EIR_names[2])),
                                            get(paste0("totCases_",EIR_names[1])))
                       , inc.mean = mean(get(paste0("inc.prov_",EIR_names[2])))
                       , inc.sup = max(get(paste0("inc.prov_",EIR_names[3])),
                                       get(paste0("inc.prov_",EIR_names[2])),
                                       get(paste0("inc.prov_",EIR_names[1])))
                       , inc.inf = min(get(paste0("inc.prov_",EIR_names[3])),
                                       get(paste0("inc.prov_",EIR_names[2])),
                                       get(paste0("inc.prov_",EIR_names[1])))
      ) 
  } 
  if (level_aggr == "national"){ 
    aggrDF = allSims %>% group_by(across(c("seed", "year", "age", fut_cols, eir_col))) %>%
      dplyr::summarise(totCases = sum(get(cases_col))
                       , totPop.nat = sum(pop.age)
                       , inc.nat = 1000*totCases/totPop.nat) %>% 
      pivot_wider(names_from = eir_col, values_from = c(totCases, inc.nat)) %>% 
      group_by(across(c(fut_cols, "year", "age"))) %>% 
      dplyr::summarise(totCases.mean = mean(get(paste0("totCases_",EIR_names[2])))
                       , totCases.sup = max(get(paste0("totCases_",EIR_names[3])),
                                            get(paste0("totCases_",EIR_names[2])),
                                            get(paste0("totCases_",EIR_names[1])))
                       , totCases.inf = min(get(paste0("totCases_",EIR_names[3])),
                                            get(paste0("totCases_",EIR_names[2])),
                                            get(paste0("totCases_",EIR_names[1])))
                       , inc.mean = mean(get(paste0("inc.nat_",EIR_names[2])))
                       , inc.sup = max(get(paste0("inc.nat_",EIR_names[3])),
                                       get(paste0("inc.nat_",EIR_names[2])),
                                       get(paste0("inc.nat_",EIR_names[1])))
                       , inc.inf = min(get(paste0("inc.nat_",EIR_names[3])),
                                       get(paste0("inc.nat_",EIR_names[2])),
                                       get(paste0("inc.nat_",EIR_names[1])))
      ) 
  }  
  
  if (level_aggr == "sub"){
    aggrDF = allSims %>% group_by(across(c("sub", "setting", "seed", "year", fut_cols, eir_col, "age"))) %>%
      dplyr::summarise(totCases = sum(get(cases_col))
                       , totPop.sub = sum(pop.age)
                       , inc.sub = 1000*totCases/totPop.sub) %>% 
      pivot_wider(names_from = eir_col, values_from = c(totCases, inc.sub)) %>% 
      dplyr::summarise(totCases.mean = mean(get(paste0("totCases_",EIR_names[2])))
                       , totCases.sup = max(get(paste0("totCases_",EIR_names[3])),
                                            get(paste0("totCases_",EIR_names[2])),
                                            get(paste0("totCases_",EIR_names[1])))
                       , totCases.inf = min(get(paste0("totCases_",EIR_names[3])),
                                            get(paste0("totCases_",EIR_names[2])),
                                            get(paste0("totCases_",EIR_names[1])))
                       , inc.mean = mean(get(paste0("inc.sub_",EIR_names[2])))
                       , inc.sup = max(get(paste0("inc.sub_",EIR_names[3])),
                                       get(paste0("inc.sub_",EIR_names[2])),
                                       get(paste0("inc.sub_",EIR_names[1])))
                       , inc.inf = min(get(paste0("inc.sub_",EIR_names[3])),
                                       get(paste0("inc.sub_",EIR_names[2])),
                                       get(paste0("inc.sub_",EIR_names[1])))
      ) 
  }
  return(aggrDF)
}

#check
# allSims2 = allSims %>% mutate(cases = casesSim * totPop /10000, ageProp = 1, age = "All")
# computeAggrTimeSeriesCasesInc(allSims2)
# computeAggrTimeSeriesCasesInc(allSims2, convert_pop = FALSE)
# computeAggrTimeSeriesCasesInc(allSims2, level_aggr = "national")
# computeAggrTimeSeriesCasesInc(allSims2, level_aggr = "national", convert_pop = FALSE)


# FUNCTIONS AGGR, DIFF INDICATORS -----------------------------------------

## PREVALENCE 
percentReductionPR.acrossScenarios <- function(allSims
                                               , pr_col = "PR"
                                               , level_aggr = "setting"
                                               , pop_col = "totPop"
                                               , eir_col = "EIR"
                                               , EIR_names = c("low","middle","high")
                                               , fut_cols = "fut"
                                               , comparison_year = 2025
                                               , counterfactual = "BaU"
                                               , id_cols = c("sub", "setting", "seed", "year", "age", pop_col, fut_cols)){

  simsSub =  allSims %>% pivot_wider(names_from = eir_col
                                     , values_from = pr_col
                                     , id_cols = id_cols
                                     , names_expand = TRUE)

  if (level_aggr == "setting"){
    aggrDF = simsSub %>% group_by(across(c("setting", "year", fut_cols, "seed", "age"))) %>%
      dplyr::summarise(PR_mean = weighted.mean(get(EIR_names[2]), get(pop_col))
                       , PR_inf = weighted.mean(get(EIR_names[1]), get(pop_col))
                       , PR_sup = weighted.mean(get(EIR_names[3]), get(pop_col))) %>%
      filter(year %in% c(comparison_year)) %>%
      group_by(setting, seed, age) %>%
      mutate(diff.mean= 100*(1-PR_mean/PR_mean[get(fut_cols) == counterfactual]),
             diff.inf= 100*(1-PR_inf/PR_inf[get(fut_cols) == counterfactual]),
             diff.sup= 100*(1-PR_sup/PR_sup[get(fut_cols) == counterfactual])) %>%
      group_by(across(c("setting", fut_cols, 'age'))) %>%
      summarise(diff.mean.Aggr=mean(diff.mean),
                diff.inf.Aggr=min(diff.inf, diff.mean, diff.sup),
                diff.sup.Aggr=max(diff.inf, diff.mean, diff.sup))
  }

  if (level_aggr == "national"){
    aggrDF = simsSub %>% group_by(across(c("year", fut_cols, "seed", "age"))) %>%
      dplyr::summarise(PR_mean = weighted.mean(get(EIR_names[2]), get(pop_col))
                       , PR_inf = weighted.mean(get(EIR_names[1]), get(pop_col))
                       , PR_sup = weighted.mean(get(EIR_names[3]), get(pop_col))) %>%
      filter(year %in% c(comparison_year)) %>%
      group_by(seed, age) %>%
      mutate(diff.mean= 100*(1-PR_mean/PR_mean[get(fut_cols) == counterfactual]),
             diff.inf= 100*(1-PR_inf/PR_inf[get(fut_cols) == counterfactual]),
             diff.sup= 100*(1-PR_sup/PR_sup[get(fut_cols) == counterfactual])) %>%
      group_by(across(c(fut_cols, 'age'))) %>%
      summarise(diff.mean.Aggr=mean(diff.mean),
                diff.inf.Aggr=min(diff.inf, diff.mean, diff.sup),
                diff.sup.Aggr=max(diff.inf, diff.mean, diff.sup))
  }
  return(aggrDF)
}

# percentReductionPR.acrossScenarios(allSims2, scenario = 2, counterfactual = 1)
# percentReductionPR.acrossScenarios(allSims2, scenario = 2, counterfactual = 1, level_aggr = "national")

# ## CASES, INC

percentReductionInc.acrossScenarios <- function(allSims
                                               , cases_col = "cases"
                                               , level_aggr = "setting"
                                               , pop_col = "totPop"
                                               , eir_col = "EIR"
                                               , EIR_names = c("low","middle","high")
                                               , fut_cols = "fut"
                                               , comparison_year = 2025
                                               , counterfactual = "BaU"
                                               , id_cols = c("sub", "setting", "seed", "year", "age", pop_col, fut_cols)){
  

  # allSims =  allSims %>% pivot_wider(names_from = eir_col
  #                                    , values_from = cases_col
  #                                    , id_cols = id_cols
  #                                    , names_expand = TRUE)
  
  if (level_aggr == "setting"){
    aggrDF = allSims %>% group_by(across(c("setting", "seed", "year", fut_cols, eir_col, "age"))) %>%
      dplyr::summarise(totCases = sum(get(cases_col))
                       , totPop.prov = sum(get(pop_col))
                       , inc.prov = 1000*totCases/totPop.prov) %>%
      pivot_wider(names_from = eir_col, values_from = c(totCases, inc.prov)) %>%
      filter(year %in% c(comparison_year)) %>%
      group_by(across(c('setting', 'seed', "age"))) %>%
      mutate(diffCases.mean= 100*(1-get(paste0("totCases_",EIR_names[2]))/
                                    get(paste0("totCases_",EIR_names[2]))[get(fut_cols) == counterfactual])
             , diffCases.sup= 100*(1-get(paste0("totCases_",EIR_names[3]))/
                                     get(paste0("totCases_",EIR_names[3]))[get(fut_cols) == counterfactual])
             , diffCases.inf= 100*(1-get(paste0("totCases_",EIR_names[1]))/
                                     get(paste0("totCases_",EIR_names[1]))[get(fut_cols) == counterfactual])
             , diffInc.mean= 100*(1-get(paste0("inc.prov_",EIR_names[2]))/
                                    get(paste0("inc.prov_",EIR_names[2]))[get(fut_cols) == counterfactual])
             , diffInc.sup= 100*(1-get(paste0("inc.prov_",EIR_names[3]))/
                                   get(paste0("inc.prov_",EIR_names[3]))[get(fut_cols) == counterfactual])
             , diffInc.inf= 100*(1-get(paste0("inc.prov_",EIR_names[1]))/
                      get(paste0("inc.prov_",EIR_names[1]))[get(fut_cols) == counterfactual])
      ) %>%
      #ungroup() %>%
      group_by(across(c("setting", fut_cols, "age"))) %>%
      dplyr::summarise(diffCasesAggr.mean = mean(diffCases.mean)
                       , diffCasesAggr.sup = max(diffCases.inf, diffCases.mean, diffCases.sup)
                       , diffCasesAggr.inf = min(diffCases.inf, diffCases.mean, diffCases.sup)
                       , diffIncAggr.mean = mean(diffInc.mean)
                       , diffIncAggr.sup = max(diffInc.inf, diffInc.mean, diffInc.sup)
                       , diffIncAggr.inf = min(diffInc.inf, diffInc.mean, diffInc.sup)
      )
  }
  
  if (level_aggr == "national"){
    aggrDF = allSims %>% group_by(across(c("seed", "year", fut_cols, eir_col, "age"))) %>%
      dplyr::summarise(totCases = sum(get(cases_col))
                       , totPop.prov = sum(get(pop_col))
                       , inc.prov = 1000*totCases/totPop.prov) %>%
      pivot_wider(names_from = eir_col, values_from = c(totCases, inc.prov)) %>%
      filter(year %in% c(comparison_year)) %>%
      group_by(across(c('seed', "age"))) %>%
      mutate(diffCases.mean= 100*(1-get(paste0("totCases_",EIR_names[2]))/
                                    get(paste0("totCases_",EIR_names[2]))[get(fut_cols) == counterfactual])
             , diffCases.sup= 100*(1-get(paste0("totCases_",EIR_names[3]))/
                                     get(paste0("totCases_",EIR_names[3]))[get(fut_cols) == counterfactual])
             , diffCases.inf= 100*(1-get(paste0("totCases_",EIR_names[1]))/
                                     get(paste0("totCases_",EIR_names[1]))[get(fut_cols) == counterfactual])
             , diffInc.mean= 100*(1-get(paste0("inc.prov_",EIR_names[2]))/
                                    get(paste0("inc.prov_",EIR_names[2]))[get(fut_cols) == counterfactual])
             , diffInc.sup= 100*(1-get(paste0("inc.prov_",EIR_names[3]))/
                                   get(paste0("inc.prov_",EIR_names[3]))[get(fut_cols) == counterfactual])
             , diffInc.inf= 100*(1-get(paste0("inc.prov_",EIR_names[1]))/
                                   get(paste0("inc.prov_",EIR_names[1]))[get(fut_cols) == counterfactual])
      ) %>%
      #ungroup() %>%
      group_by(across(c(fut_cols, "age"))) %>%
      dplyr::summarise(diffCasesAggr.mean = mean(diffCases.mean)
                       , diffCasesAggr.sup = max(diffCases.inf, diffCases.mean, diffCases.sup)
                       , diffCasesAggr.inf = min(diffCases.inf, diffCases.mean, diffCases.sup)
                       , diffIncAggr.mean = mean(diffInc.mean)
                       , diffIncAggr.sup = max(diffInc.inf, diffInc.mean, diffInc.sup)
                       , diffIncAggr.inf = min(diffInc.inf, diffInc.mean, diffInc.sup)
      )
  }
  return(aggrDF)
}

casesAverted.sumAcrossYears <- function(allSims
                                        , cases_col = "cases"
                                        , level_aggr = "setting"
                                        , eir_col = "EIR"
                                        , EIR_names = c("low","middle","high")
                                        , fut_cols = "fut"
                                        , counterfactual = "BaU"
                                        , year_start = 2023
                                        , year_end = 2025){
  
  
  
  subSims = allSims %>% group_by(across(c("sub","ZS", "setting", "seed", "age",  fut_cols, eir_col))) %>%
    filter(year %in% seq(year_start, year_end)) %>%
    dplyr::summarise(cases.all = sum(get(cases_col))) %>%
    group_by(across(c("sub","ZS", "setting", "seed", eir_col))) %>%
    mutate(ca = cases.all[get(fut_cols) == counterfactual] - cases.all) %>%
    #keep only fut 2 (otherwise, duplicated rows)
    #filter(get(fut_cols) == scenario) %>%
    #select the extremes and take mean of middle
    pivot_wider(names_from = eir_col, values_from = c(ca), id_cols = c("sub", "ZS","setting", fut_cols, "seed", "age")) %>%
    group_by(across(c("sub", "ZS","setting", "age", fut_cols))) %>% 
    dplyr::summarise(ca.mean = mean(get(EIR_names[2]))
                     , ca.sup = max(get(EIR_names[3]), get(EIR_names[2]), get(EIR_names[1]))
                     , ca.inf = min(get(EIR_names[3]), get(EIR_names[2]), get(EIR_names[1]))) 
  
  if (level_aggr == "sub"){
    aggrDF = subSims
  }
  if (level_aggr == "setting"){ 
    #aggregate at setting level by summing all cases averted
    aggrDF = subSims %>% group_by(across(c("setting", age, fut_cols))) %>%
      dplyr::summarise(ca.tot.mean = sum(ca.mean)
                       , ca.tot.sup = sum(ca.sup)
                       , ca.tot.inf = sum(ca.inf)) 
  }
  if (level_aggr == "ZS"){ 
    #aggregate at ZS level by summing all cases averted
    aggrDF = subSims %>% group_by(across(c("ZS", age, fut_cols))) %>%
      dplyr::summarise(ca.tot.mean = sum(ca.mean)
                       , ca.tot.sup = sum(ca.sup)
                       , ca.tot.inf = sum(ca.inf)) 
  }
  if(level_aggr == "national"){
    #aggregate at setting level by summing all cases averted
    aggrDF = subSims %>% group_by(across(c("age", fut_cols))) %>%
      dplyr::summarise(ca.tot.mean = sum(ca.mean)
                       , ca.tot.sup = sum(ca.sup)
                       , ca.tot.inf = sum(ca.inf)) 
  }
  return(aggrDF)
}

# casesAverted.sumAcrossYears(allSims2
#                             , counterfactual = 1
#                             , scenario = 2
#                             , year_start = 2023
#                             , year_end = 2025
#                             , convert_pop = FALSE)
# 
# 
# casesAverted.sumAcrossYears(allSims2
#                             , counterfactual = 1
#                             , scenario = 2
#                             , year_start = 2023
#                             , year_end = 2025
#                             , level_aggr = "national"
#                             , convert_pop = FALSE)
# 
# casesAverted.sumAcrossYears(allSims2
#                             , counterfactual = 1
#                             , scenario = 2
#                             , year_start = 2024
#                             , year_end = 2024
#                             , level_aggr = "national"
#                             , convert_pop = FALSE)
# 
# casesAverted.sumAcrossYears(allSims2
#                             , counterfactual = 1
#                             , scenario = 2
#                             , year_start = 2024
#                             , year_end = 2024
#                             , convert_pop = FALSE)


firstup <- function(x) {
  substr(x, 1, 1) <- toupper( substr(x, 1, 1) )
  x
}

lastlow <- function(x) {
  substr(x, 2, nchar(x)) <- tolower(substr(x, 2, nchar(x)))
  x
}