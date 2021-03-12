############################################
############################################
#      G l o b a l     S c r i p t         #  
############################################
############################################

# How to start reproduce the analysis from:

# Micheletti et al., in review. Teasing apart the pathways of climate change 
# effects on boreal landbird distributions in Northwestern Canada using SpaDES. 
# Frontier in Ecology and Evolution.

# 1. Define the following variables. If not defined, the defaults will be ran 
# ("SCFM" and "LandR", no bird predictions)

# Run each one of the replicates (n = 10) and factorial model combinations 
# following Table 1 in the manuscript

  RUN <- "1"
    usrEmail <- "your.name@email.com" # Your e.mail for GDrive authorization
    updateCRAN <- FALSE
    updateGithubPackages <- FALSE
    updateSubmodules <- FALSE
    Sys.sleep(1)
    onlyLoadDEOptim <- TRUE
    runName <- "NWT_BCR6"
    Sys.sleep(1)
    replicateNumber <- paste0("run", RUN)
    Sys.sleep(1)
    vegetation <- "LandR"
    fire <- "SCFM"
    runLandR <- TRUE
    runBirds <- TRUE
    runPosthocBirds <- TRUE
    birdModelVersion <- c(4, 6)
    Sys.sleep(3)
    source("1_generalSetup.R")
    source("2_generatingInputs.R")
    if (all(runLandR, fire != "SCFM")){
      source("3_preamble.R")
      source("4_fittingModules.R")
    }
    source("5_runningSimulations.R")
    source("6_posthocAnalysis.R")
