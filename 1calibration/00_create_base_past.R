# Install openMalariaUtilities
# devtools::install_github("SwissTPH/r-openMalariaUtilities", ref = "23.02")

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
    resist = TRUE, versionnum = versionnum
  )
  
  ## Define histITNWeak
  baseList <- define_ITN_compat(
    baseList = baseList, component = "histITNWeak", mosqs = mosqs, hist = TRUE,
    resist = TRUE, versionnum = versionnum
  )
  
  
  ## add zeros to all parameters of post and pre killing
  #set to zero pre and post parameteres
  indexNet = as.numeric(which(unlist(lapply(
    baseList$interventions$human, function(x) x[["id"]] == "histITNWeak"))))
  
  lenFields = length(baseList$interventions$human[indexNet]$component$ITN$
                       anophelesParams$postprandialKillingEffect)
  #assign 0 values for all tags inside pre and post
  #to act on the 4 mosquitoes, index 8:11 (CAREFUL WITH THIS PART IN THE FUTURE!!!)
  for (j in 8:11){
    
    for (i in 1:lenFields){
      baseList$interventions$human[indexNet]$component$ITN[j]$anophelesParams$
        postprandialKillingEffect[[i]] <- 0
      baseList$interventions$human[indexNet]$component$ITN[j]$anophelesParams$
        preprandialKillingEffect[[i]] <- 0
    }
    
  }
  
  ## Define SMC
  baseList <- define_treatSimple(baseList, component = "SMC",
                                 durationBlood = "25d")
  
  
  ## Deployment section
  
  ## Historical IRS
  baseList <- deploy_it_compat(
    baseList = baseList, component = "Bendiocarb",
    coverage = "@histIRScovBendiocarb@",
    ## Allowing for different hist coverage levels
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
                       , startDate = "2019-06-15", endDate = "2019-09-30"
                       , interval = "1 month"
                       , coverage = "@histcovSMC2019@")
  
  
  ## Importation: MANDATORY
  baseList <- define_importedInfections_compat(baseList = baseList, 10, time = 0)
  
  ## Health system
  ## This code now only writes the historical values:
  ## "@EffCov14d2000@", ..., "@EffCov14d2019@"
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
