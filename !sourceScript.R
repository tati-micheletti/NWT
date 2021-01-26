
############################################
############################################
#      G l o b a l     S c r i p t         #  
############################################
############################################

# sourceScript
# How to start a simulation from the R command
# 1. Define these variables. If not defined, the defaults will be ran 
# ("SCFM" and "LandR", no caribou or birds)
# Run each one of these manually in a different screen
# These are the othe parameters that can be set here:
# 1. originalDateAnalysis (should be passed in format DDMMMYY; i.e. 
# 10JAN20; used only if runLandR == FALSE and runBirds == TRUE)

# usrEmail <- "your.email@gmail.com" # Your e.mail for GDrive authorization

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
    # originalDateAnalysis <- "21JAN21"
    Sys.sleep(1)
    replicateNumber <- paste0("run", RUN)
    Sys.sleep(1)
    vegetation <- "LandR.CS"
    fire <- "fS"
    runLandR <- TRUE
    runBirds <- FALSE
    runCaribou <- FALSE
    birdModelVersion <- c(4, 6)
    Sys.sleep(3)
    source("1_generalSetup.R")
    source("2_generatingInputs.R")
    if (all(runLandR, fire != "SCFM")){
      source("3_preamble.R")
      source("4_fittingModules.R")
    }
    debugonce(LandR::loadKNNSpeciesLayers)
    source("5_runningSimulations.R")
    # source("6_posthocAnalysis.R")
# if (runOnlySimInit){
#   factorialSimulations <- SpaDES.experiment::experiment2(
#     # LandR.CS_fS = LandR.CS_fS,
#     LandR_SCFM = LandR_SCFM,
#     # LandR.CS_SCFM = LandR.CS_SCFM,
#     LandR_fS = LandR_fS,
#     clearSimEnv = TRUE,
#     replicates = 1, debug = 1,
#     drive_auth_account = "tati.micheletti@gmail.com")
# }
