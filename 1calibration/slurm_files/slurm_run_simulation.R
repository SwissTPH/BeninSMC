#!/usr/bin/env Rscript

## Get arguments
args <- commandArgs(trailingOnly = TRUE)

## Set correct working directory
setwd(dir = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_1calibration")

## Verbose output
verbose <- FALSE

## Load library
library(openMalariaUtilities)

## Load cached data
loadExperiment("/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_1calibration")

## Get scenario number to run
slurm <- getCache(x = "slurm_simulation")
ID <- args[1]
ncores <- as.numeric(args[2])
rowStart <- slurm$scen_batches[[ID]][1]
rowEnd <- slurm$scen_batches[[ID]][length(slurm$scen_batches[[ID]])]

## Read scenarios
scens <- readScenarios()

runSimulations(
  scenarios = scens, rowStart = rowStart, rowEnd = rowEnd, verbose = verbose,
  ncores = ncores
)
