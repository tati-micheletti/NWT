
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
setwd("~/projects/NWT/")

# source('functions/waitingTime.R')
# allExp <- c("XIII", "XIV")

# experimentName <- I # And II, III ... XIV
# waitingTime(experimentName, mins = 10)
# Sys.sleep(60*25*(as.numeric(RUN)-1))

climateModel <- "CCSM4_RCP85" # :: DONE
  # climateModel <- "CanESM2_RCP85" # :: DONE
  # climateModel <- "ACCESS1-0_RCP85" # ::POSTPONED! (3 and 4 already did it)
  # climateModel <- "CSIRO-Mk3-6-0_RCP85" ::
  # climateModel <- "INM-CM4_RCP85" # :: DONE
  # climateModel <- "CNRM-CM5_RCP85" # :: 
Times <- list(start = 2031, end = 2071)
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
    originalDateAnalysis <- "landscapeRuns"
    Sys.sleep(1)
    replicateNumber <- paste(strsplit(climateModel, split = "_")[[1]][1], 
                             paste0("run", RUN), sep = "_")
    Sys.sleep(1)
    vegetation <- "LandR.CS"
    fire <- "fS"
    runLandR <- FALSE
    runBirds <- FALSE
    runCaribou <- FALSE
    birdModelVersion <- 8
    Sys.sleep(4)
    source("1_generalSetup.R")
    Sys.sleep(4)
    source("2_generatingInputs.R")
    # source("3_preamble_2011layers.R")
    # source("4_fittingModules.R")
    # source("5_runningSimulations.R")
    Sys.sleep(4)
    # source("7_hotspotsAnalysis.R")
    source("8_hotspotsPosthoc.R")

    # source('~/projects/NWT/functions/uploadFilesToGDrive.R')
    # uploadFilesToGDrive(resultsFolderPath = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/", 
    #                     filePatterns = "predicted", 
    #                     UnwantedPatterns = "caribou", 
    #                     Recursive = TRUE,
    #                     Gfolder = "1tNCmQEJqGJp9s9PDbNHaa2Gfy1Jql9_b")
    
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
