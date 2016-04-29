

rm(list = ls())

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(paste(path, nm, sep = "\\"), ...)
    if(trace) cat("\n")
  }
}

sourceDir(path = "C:\\Users\\kcha193\\workspace\\simarioV2\\R")

library(snowfall)
library(stringr)
library(stringi)


setwd("C:\\Users\\kcha193\\workspace\\KnowLab")

source("initKnowLab.R")
source("SimmoduleMELC1_21.R")

NUM_ITERATIONS <- 21

initialSim <- initSim(NUM_ITERATIONS)

saveRDS(initialSim, "./base/initialSim.rds")

NUM_ITERATIONS <<- NUM_ITERATIONS
dict <<- initialSim$dict
limits <<- initialSim$limits
binbreaks <<- initialSim$binbreaks
catToContModels <<- initialSim$catToContModels
models <<- initialSim$models
PropensityModels <<- initialSim$PropensityModels
children <<- initialSim$children


sfInit(parallel=TRUE, cpus = 4, slaveOutfile = "test.txt" )

sfExportAll()

sfLibrary(Hmisc)
sfLibrary(snowfall)


Simenv <- createSimenv("Base", initialSim$simframe, initialSim$dict, "years1_21")


env.base <- simulateP(Simenv, 10)

sfStop()

(env.base)

saveRDS(env.base, "./base/FullBaseRun.rds")



Simenv$modules[[1]]$name


env.scenario <- createSimenv("scenario", initialSim$simframe, initialSim$dict, "years1_21")

env.scenario$cat.adjustments$z1Hearing[1,] <- c(0.8,0.2 )	


sfInit(parallel=TRUE, cpus = 4, slaveOutfile = "test.txt" )

sfExportAll()

sfLibrary(Hmisc)
sfLibrary(snowfall)

env.scenario <- simulateP(env.scenario, 10)
sfStop()


	
env.scenario$modules[[1]]$run_results_collated$means$IQ[16,]	
env.scenario$modules[[1]]$run_results_collated$freqs$z1ScoreLvl1[17,]







test <- tableBuilder(env = Simenv, statistic="means", variableName="fhrswrk")
test <- tableBuilder(Simenv, "frequencies", "welfareLvl1")
test <- tableBuilder(Simenv, statistic="frequencies", variableName="welfareLvl1", 
					grpbyName="",logisetexpr="z1singleLvl1==1")

