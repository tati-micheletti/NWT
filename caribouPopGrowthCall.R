
RUN <- "CCSM4"
# RUN <- "CanESM2"
climateModel <- "CCSM4_RCP85" # :: OK
# climateModel <- "CanESM2_RCP85" # :: OK
# climateModel <- "ACCESS1-0_RCP85" # :: OK
# climateModel <- "CSIRO-Mk3-6-0_RCP85" :: OK
# climateModel <- "INM-CM4_RCP85" # :: OK
# climateModel <- "CNRM-CM5_RCP85" # :: OK
usrEmail <- "tati.micheletti@gmail.com" # Your e.mail for GDrive authorization
hostIp <- 68 # Specify which machine this is running for
updateCRAN <- FALSE
updateGithubPackages <- FALSE
updateSubmodules <- FALSE
isTest <- FALSE # runMe
Sys.sleep(1)
runOnlySimInit <- FALSE # TRUE to run experiment, FALSE to run simulations individually
# fitTheseFireSenseModels <- "spread"
onlyLoadDEOptim <- TRUE
runName <- "NWT_NT1_BCR6" #"NWT_BCR6"
runPosthocBirds <- FALSE
originalDateAnalysis <- "21JAN21"
Sys.sleep(1)
replicateNumber <- paste0("run", RUN)
Sys.sleep(1)
vegetation <- "LandR.CS"
fire <- "fS"
runLandR <- FALSE
runBirds <- FALSE
runCaribou <- TRUE
birdModelVersion <- c(4, 6)
Sys.sleep(3)
source("1_generalSetup.R")
source("2_generatingInputs.R")
if (all(runLandR, fire != "SCFM")){
  source("3_preamble.R")
  source("4_fittingModules.R")
}
modules <- list("caribouPopGrowthModel")
#   # Caribou Population Growth Parameters!! <- Caribou popGrowth removed from the main simulation! Needs update
parameters <- list(
  caribouPopGrowthModel = list(
    ".plotInitialTime" = NULL,
    "recoveryTime" = 40,
    ".useDummyData" = FALSE,
    ".growthInterval" = 10,
    "recruitmentModelVersion" = "Johnson", # Johnson or ECCC
    "recruitmentModelNumber" = "M4",
    "femaleSurvivalModelNumber" = c("M1", "M4") # M1:M5 --> best models: M1, M4
    ) 
  # ATTENTION: recruitmentModelNumber and recruitmentModelVersion need to be paired. ie.
  # if you want to run M3 from ECCC and M1 and M4 from Johnson you should put these as
  #     "recruitmentModelVersion" = c("ECCC", "Johnson", "Johnson"),
  #     "recruitmentModelNumber" = c("M3", "M1", "M4"),
)
Times <- list(start = 2011, end = 2100)
SpaDES.core::setPaths(inputPath = "/home/tmichele/projects/NWT/outputs/climateScenarios/LandR.CS_fS/runCCSM4/",
                      outputPath = "/home/tmichele/projects/NWT/outputs/climateScenarios/LandR.CS_fS/runCCSM4/caribouPredictions")
simulationBoo <- paste0(definedRun$whichRUN, "_caribouPop")
trackSeed(replic = definedRun$whichReplicate, runName = runName)
rstCurrentBurnList <- readRDS(file.path(Paths$inputPath, 
                                            "rstCurrentBurnList_year2100.rds"))
objects <- c(objects, list("rstCurrentBurnList" = rstCurrentBurnList))
assign(x = simulationBoo,
       do.call(
         get(spadesFun),
         args = alist(
           times = Times,
           params = parameters,
           modules = modules,
           objects = objects,
           paths = Paths,
           loadOrder = unlist(modules),
           outputs = outputsLandR,
           debug = 1
         )
       ))
