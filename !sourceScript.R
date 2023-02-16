############################################
############################################
#      G l o b a l     S c r i p t         #  
############################################
############################################

# To reproduce the analysis from 

# Micheletti, Tatiane, Samuel Haché, Frances E C Stewart, Alex M Chubaty, 
# Ceres Barros, Erin M Bayne, Steven G Cumming, et al. 2023. “Will This Umbrella 
# Leak? A Caribou Umbrella Index for Boreal Bird Conservation.” 
# Conservation Science and Practice in press.

# run each one of the replicates (n = 10) for each climate model (n = 3)
RUN <- "1" # 1 to 10
climateModel <- "CCSM4_RCP85" # "CanESM2_RCP85" or "INM-CM4_RCP85"

# And update the following
usrEmail <- "your.name@email.com" # Your e.mail for GDrive authorization
# Setup Google drive folders for results uploading
appendixFolder <- ""
individualSpFolder <- ""
LMspFolder <- ""
figuresFolder <- ""

##########################################

updateCRAN <- TRUE
updateGithubPackages <- TRUE
Sys.sleep(1)
onlyLoadDEOptim <- TRUE
runName <- "NWT_BCR6"
replicateNumber <- paste(strsplit(climateModel, split = "_")[[1]][1], 
                         paste0("run", RUN), sep = "_")
vegetation <- "LandR.CS" # Climate sensitive vegetation
fire <- "fS" # Climate sensitive fire
runLandR <- TRUE
runBirds <- TRUE
runCaribou <- TRUE
birdModelVersion <- 8
Times <- list(start = 2011, end = 2091)
originalDateAnalysis <- "landscapeRuns"

Sys.sleep(3)
source("1_generalSetup.R")
source("2_generatingInputs.R")
source("3_preamble.R")
source("4_fittingModules.R")
source("5_runningSimulations.R")
source("6_hotspotAnalysis.R")
