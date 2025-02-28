#!/usr/bin/env Rscript

## Get arguments
args <- commandArgs(trailingOnly = TRUE)
ncores <- as.numeric(args[1])

## Set correct working directory
setwd(dir = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper/")

## Load library
library(openMalariaUtilities)

## Load cached data
loadExperiment("/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_1calibration")

## Read scenarios
scens <- readScenarios()
batches <- splitSeq(1:nrow(scens), n = 10000)

funs <- getCache(x = "slurm_results")

fileCol <- function(scens, ffilter) {
  scens <- data.table::as.data.table(scens)
  ffilter <- substitute(ffilter)
  fToUse <- scens[eval(ffilter), file]
  return(fToUse)
}

for (batch in names(batches)) {
lower_scen <- min(batches[[batch]])
upper_scen <- max(batches[[batch]])

message(paste0("Working on scenario ", lower_scen, " to ", upper_scen))

collectResults(
expDir = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_1calibration",
dbName = "BeninSMC_1calibrationmonthly",
dbDir = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper/",
replace = FALSE,
fileFun = fileCol, fileFunArgs = list(scens = scens, ffilter = batches[[batch]]),
aggrFun = funs$cAggrFun, aggrFunArgs = funs$cAggrFunArgs,
ncores = ncores, ncoresDT = 1,
strategy = "batch",
resultsName = "om_results_monthly",
resultsCols = list(names = c("scenario_id", "date", "age_group", "date_aggregation", "nHost", "prevalenceRate"), types = c("INTEGER", "TEXT", "TEXT", "TEXT", "NUMERIC", "NUMERIC")),
indexOn = NULL,
verbose = FALSE
  )

gc(verbose = TRUE)
}
