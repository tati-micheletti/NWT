
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

RUN <- "1"
# Sys.sleep(600*(as.numeric(RUN)-1))
climateModel <- "CCSM4_RCP85" # :: Running...
  # climateModel <- "CanESM2_RCP85" # :: NEXT
  # climateModel <- "ACCESS1-0_RCP85" # :: 
  # climateModel <- "CSIRO-Mk3-6-0_RCP85" ::   
  # climateModel <- "INM-CM4_RCP85" # :: 
  # climateModel <- "CNRM-CM5_RCP85" # :: 
Times <- list(start = 2011, end = 2100)
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
    runName <- "NWT_NT1_BCR6_2011" #"NWT_BCR6"
    runPosthocBirds <- FALSE
    # originalDateAnalysis <- "10FEB21"
    Sys.sleep(1)
    replicateNumber <- paste(strsplit(climateModel, split = "_")[[1]][1], 
                             paste0("run", RUN), sep = "_")
    Sys.sleep(1)
    vegetation <- "LandR.CS"
    fire <- "fS"
    runLandR <- TRUE
    runBirds <- TRUE
    runCaribou <- TRUE
    birdModelVersion <- 8
    Sys.sleep(3)
    source("1_generalSetup.R")
    source("2_generatingInputs.R")
    source("3_preamble_2011layers.R")
    source("4_fittingModules.R")
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
