#!/usr/bin/env Rscript

## Get arguments
args <- commandArgs(trailingOnly = TRUE)

## Set correct working directory
setwd(dir = "/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper/")

## Load library
library(openMalariaUtilities)

## Load cached data
loadExperiment("/scicore/home/pothin/lemant0000/OpenMalaria/Experiments/BeninSMCpaper//BeninSMC_1calibration")

## Get range of scenarios to create
slurm <- getCache(x = "slurm_scenarios")
ID <- args[1]
ncores <- as.numeric(args[2])
rowStart <- slurm$scen_batches[[ID]][1]
rowEnd <- slurm$scen_batches[[ID]][length(slurm$scen_batches[[ID]])]

## Read scenarios
scens <- readScenarios()

baseFile <- getCache(x = "baseXml")
prefix <- getCache(x = "experimentName")

setupScenarios(
  scenarios = scens, baseFile = baseFile, rowStart = rowStart, rowEnd = rowEnd,
  prefix = prefix, ncores = ncores
)
