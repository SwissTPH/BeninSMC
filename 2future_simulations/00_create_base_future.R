# Load the necessary packages
library(openMalariaUtilities)
library(tidyverse)

# Function that creates a list with all the elements which are specific for the
# base xml (placeholders, interventions, etc.)
create_baseList = function(country_name, expName, 
                           sim_start, versionnum) {

  ## Basic xml skeleton
  baseList = list(
    # Mandatory
    expName = expName,
    # Mandatory
    OMVersion = versionnum,
    # Mandatory
    demography = list(),
    monitoring = list(),
    interventions = list(),
    healthSystem = list(),
    entomology = list(),
    # These are optional for OM
    # parasiteGenetics = list(),
    # pharmacology = list(),
    # diagnostics = list(),
    model = list()
  )

  # MOSQUITO CONTRIBUTION
  # Anopheles gambiae and funestus
  contrib = c("@gin@", "@gout@", "@fin@","@fout@" )
  mosqs = c("gambiae_indoor", "gambiae_outdoor",
            "funestus_indoor", "funestus_outdoor")
  
  ## Create demography
  eval(as.symbol(country_name)) #comes from 2013 census
  baseList <- write_demography_compat(
    baseList = baseList, pop = "@pop@", country = country_name
  )
  
  
  ## Create monitoring
  baseList <- write_monitoring_compat(
    baseList = baseList, y1 = 2000, y2 = 2027, detect = 100, #RDT detection limit
    SIMSTART = sim_start,
    upperbounds = c(2, 5, 10, 100)
  )
  
  ## Begin interventions for humans
  ## Define IRS
  baseList <- define_IRS_compat(
    baseList = baseList, mosqs = mosqs, component = "Bendiocarb"
  )
  
  baseList <- define_IRS_compat(
    baseList = baseList, mosqs = mosqs, component = "Actellic50EC"
  )
  
  baseList <- define_IRS_compat(
    baseList = baseList, mosqs = mosqs, component = "Actellic300CS"
  )
  
  
  ## Define historical ITN
  baseList <- define_ITN_compat(
    baseList = baseList, component = "histITN", mosqs = mosqs, hist = TRUE,
    resist = TRUE, halflife = 1, versionnum = versionnum
  )
  
  ## Define histITNWeak (only physical barrier)
  baseList <- define_ITN_compat(
    baseList = baseList, component = "histITNWeak", mosqs = mosqs, hist = TRUE,
    resist = TRUE, halflife = 1, versionnum = versionnum
  )
  
  ## Define future standard weak nets
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  source("../0modelling_parameters/netdurability.R")
  ## P2: PermaNet 2.0; DN: DuraNet
  
  baseList <- define_ITN_compat(
    baseList = baseList, component = "futITNWeakP2", mosqs = mosqs, hist = FALSE,
    resist = TRUE, halflife = ITNdecay_P2$L/2, versionnum = versionnum
  )
  
  baseList <- define_ITN_compat(
    baseList = baseList, component = "futITNWeakDN", mosqs = mosqs, hist = FALSE,
    resist = TRUE, halflife = ITNdecay_Duranet$L/2, versionnum = versionnum
  )
  
  ## set insecticide parameters to 0 to mimic full resistance
  for (net_name in c("histITNWeak", "futITNWeakP2","futITNWeakDN")){
    indexNet = as.numeric(which(unlist(lapply(baseList$interventions$human, function(x) x[["id"]] == net_name))))
    
    lenFields = length(baseList$interventions$human[indexNet]$component$ITN$anophelesParams$postprandialKillingEffect)
    #assign 0 values for all tags inside pre and post
    #to act on the 4 mosquitoes, index 8:11 (CAREFUL WITH THIS PART IN THE FUTURE!!!)
    for (j in 8:11){
      
      for (i in 1:lenFields){
        baseList$interventions$human[indexNet]$component$ITN[j]$anophelesParams$postprandialKillingEffect[[i]] <- 0
        baseList$interventions$human[indexNet]$component$ITN[j]$anophelesParams$preprandialKillingEffect[[i]] <- 0
      }
      
    }
    
  }
  
  ## Define future nets without resistance
  
  baseList <- define_ITN_compat(
    baseList = baseList, component = "futPBOP2", mosqs = mosqs, hist = FALSE,
    resist = FALSE, halflife = ITNdecay_P2$L/2, versionnum = versionnum
  )
  
  baseList <- define_ITN_compat(
    baseList = baseList, component = "futPBODN", mosqs = mosqs, hist = FALSE,
    resist = FALSE, halflife = ITNdecay_Duranet$L/2, versionnum = versionnum
  )
  
  ## Define SMC
  baseList <- define_treatSimple(baseList, component = "SMC",
                                 durationBlood = "25d")
  
  ## Define PMC
  baseList <- define_treatSimple(baseList, component = "PMC",
                                 durationBlood = "10d")
  
  ## Deployment section
  
  ## Historical IRS
  baseList <- deploy_it_compat(
    baseList = baseList, component = "Bendiocarb",
    coverage = "@histIRScovBendiocarb@",
    ## Allowing for different historical coverage levels
    byyear = T,
    y1 = 2008, y2 = 2013, every = 1, interval = "year",
    deployvar="@histIRSmonthBendiocarb@", SIMSTART = sim_start
  )
  
  baseList <- deploy_it_compat(
    baseList = baseList, component = "Bendiocarb",
    coverage = "@histIRScovBendiocarb@",
    byyear = T,
    y1 = 2010, y2 = 2010, every = 1, interval = "year",
    deployvar="histIRSmonthBendiocarbbis", SIMSTART = sim_start
  )
  
  baseList <- deploy_it_compat(
    baseList = baseList, component = "Actellic50EC",
    coverage = "@histIRScovActellicEC@",
    ## Allowing for different hist coverage levels
    byyear = T,
    y1 = 2013, y2 = 2014, m1=5, m2= 5, every = 1, interval = "year",
    SIMSTART = sim_start
  )
  
  baseList <- deploy_it_compat(
    baseList = baseList, component = "Actellic300CS",
    coverage = "@histIRScovActellicCS@",
    ## Allowing for different hist coverage levels
    byyear = T,
    y1 = 2014, y2 = 2019, every = 1, m1=5, m2=5, interval = "year",
    SIMSTART = sim_start
  )
  
  ## Historical ITN
  baseList <- deploy_it_compat(
    baseList = baseList, component = "histITN", coverage = "@histITNcov@",
    ## Allowing for different hist coverage levels
    byyear = TRUE,
    ## Annual deployments
    y1 = 2000, y2 = 2015, every = 1, interval = "year",
    #April to match 2020 and 2023 distributions dates
    m1 = 4, m2 = 4, d1 = 5, d2 = 5,
    SIMSTART = sim_start
  )
  
  ## Resistance appears in 2016
  baseList <- deploy_it_compat(
    baseList = baseList, component = "histITNWeak", coverage = "@histITNcov@",
    ## Allowing for different hist coverage levels
    byyear = TRUE,
    ## Annual deployments
    y1 = 2016, y2 = 2019, every = 1, interval = "year",
    #April to match 2020 and 2023 distributions dates
    m1 = 4, m2 = 4, d1 = 5, d2 = 5,
    SIMSTART = sim_start
  )
  
  ## Historical SMC
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 5
                       , startDate = "2019-07-15", endDate = "2019-10-30"
                       , interval = "1 month"
                       , coverage = "@histcovSMC2019@")
  
  ## Future IRS
  baseList <- deploy_it_compat(
    baseList = baseList, component = "Actellic300CS",
    coverage = "@recentIRScov@",
    ## Allowing for different hist coverage levels
    byyear = T,
    y1 = 2020, y2 = 2021, every = 1, m1=4, m2=4, interval = "year",
    SIMSTART = sim_start
  )
  
  ## Future nets
  baseList <- deploy_it_compat(
    baseList = baseList, component = "@futITNtype2020@", coverage = "@futITNcov@",
    ## Allowing for different hist coverage levels
    byyear = TRUE,
    ## Annual deployments
    y1 = 2020, y2 = 2020, every = 1, interval = "year",
    #April to match 2020 and 2023 distributions dates
    m1 = 4, m2 = 4, d1 = 5, d2 = 5,
    SIMSTART = sim_start
  )
  
  baseList <- deploy_it_compat(
    baseList = baseList, component = "@futITNtype2023@", coverage = "@futITNcov@",
    ## Allowing for different hist coverage levels
    byyear = TRUE,
    ## Annual deployments
    y1 = 2023, y2 = 2023, every = 1, interval = "year",
    #April to match 2020 and 2023 distributions dates
    m1 = 4, m2 = 4, d1 = 5, d2 = 5,
    SIMSTART = sim_start
  )
  
  baseList <- deploy_it_compat(
    baseList = baseList, component = "@futITNtype2026@", coverage = "@futITNcov@",
    ## Allowing for different hist coverage levels
    byyear = TRUE,
    ## Annual deployments
    y1 = 2026, y2 = 2026, every = 1, interval = "year",
    #April to match 2020 and 2023 distributions dates
    m1 = 4, m2 = 4, d1 = 5, d2 = 5,
    SIMSTART = sim_start
  )
  
  ## Future SMC
  ### 2020
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 5
                       , startDate = "2020-07-15", endDate = "2020-10-30"
                       , interval = "1 month"
                       , coverage = "@futcovSMC2020@")
  ### 2021
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 5
                       , startDate = "2021-07-15", endDate = "2021-10-30"
                       , interval = "1 month"
                       , coverage = "@futcovSMC2021@")
  ### 2022
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 5
                       , startDate = "2022-07-15", endDate = "2022-10-30"
                       , interval = "1 month"
                       , coverage = "@futcovSMCJul2022@")
  
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 5
                       , startDate = "2022-06-15", endDate = "2022-09-30"
                       , interval = "1 month"
                       , coverage = "@futcovSMCJun2022@")
  
  
  ### 2023: same as in 2021
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 5
                       , startDate = "2023-07-15", endDate = "2023-10-30"
                       , interval = "1 month"
                       , coverage = "@futcovSMC2021@")
  
  ### 2024
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 5
                       , startDate = "2024-07-15", endDate = "2024-10-30"
                       , interval = "1 month"
                       , coverage = "@futcovSMC0to5@")
  
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 10
                       , startDate = "2024-07-15", endDate = "2024-10-30"
                       , interval = "1 month"
                       , coverage = "@futcovSMC0to10@")
  
  ### 2025
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 5
                       , startDate = "2025-07-15", endDate = "2025-10-30"
                       , interval = "1 month"
                       , coverage = "@futcovSMC0to5@")
  
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 10
                       , startDate = "2025-07-15", endDate = "2025-10-30"
                       , interval = "1 month"
                       , coverage = "@futcovSMC0to10@")
  
  ### 2026
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 5
                       , startDate = "2026-07-15", endDate = "2026-10-30"
                       , interval = "1 month"
                       , coverage = "@futcovSMC0to5@")
  
  baseList <- deployIT(baseList, component = "SMC"
                       , minAge = 0.25
                       , maxAge = 10
                       , startDate = "2026-07-15", endDate = "2026-10-30"
                       , interval = "1 month"
                       , coverage = "@futcovSMC0to10@")
  
  ## Future PMC: 10 and 14 weeks, 6, 9, 12, 15, 18 and 24 months
  
  ### Communes starting in September 2022
  baseList <- deploy_cont_compat(baseList, component = "PMC"
                                 , begin = "2022-09-01"
                                 , end = "2026-12-31"
                                 , targetAgeYrs = c(10/52, 14/52, 0.5, 0.75,
                                                    1 ,1.25, 1.5, 2)
                                 , coverage = rep("@futPMCSep2022cov@",8))
  
  ### Communes starting in March 2023
  baseList <- deploy_cont_compat(baseList, component = "PMC"
                                 , begin = "2023-03-01"
                                 , end = "2026-12-31"
                                 , targetAgeYrs = c(10/52, 14/52, 0.5, 0.75,
                                                    1, 1.25, 1.5, 2)
                                 , coverage = rep("@futPMCMar2023cov@",8))
  
  ## Importation: MANDATORY
  ## 10 infections per 1000 people per year
  baseList <- define_importedInfections_compat(baseList = baseList, 10, time = 0)
  
  ## Health system
  ## This code now only writes the historical values:
  ## "@EffCov14d2000@", ..., "@EffCov14d2019@" (effective coverage each year)
  baseList <- define_changeHS_compat(
    baseList = baseList, access = "EffCov14d",
    y1 = 2000, y2 = 2018,
    use_at_symbol = TRUE,
    ## Specifying future CM levels as '@futCM@'
    ## coverage = "@futCMcov@",
    ## Default values
    pSelfTreatUncomplicated = 0.01821375,
    pSeekOfficialCareSevere = .48,
    SIMSTART = sim_start
  )
  
  ## Write health system: MANDATORY
  baseList <- write_healthsys_compat(
    baseList = baseList, access = 0
  )
  
  ## Entomology section: MANDATORY
  baseList <- make_ento_compat(
    baseList = baseList, mosqs, contrib,
    EIR = "@EIR@",
    seasonality = paste0("@m", 1:12, "@"),
    propInfected = .078,
    propInfectious = .021
  )
  
  ## Specify seed: MANDATORY
  baseList <- write_end_compat(
    baseList = baseList, seed = "@seed@", modelname = "base"
  )
  
  return(baseList)
}
