
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
usrEmail <- "tati.micheletti@gmail.com" # Your e.mail for GDrive authorization
updateCRAN <- FALSE
updateGithubPackages <- TRUE
updateSubmodules <- FALSE
isTest <- TRUE # runMe
runOnlySimInit <- FALSE # TRUE to run experiment, FALSE to run simulations individually

# LandR.CS + fireSense and tests # PURPLE
fitTheseFireSenseModels <- "spread"
onlyLoadDEOptim <- TRUE
runName <- "NWT_BCR6"
replicateNumber <- "testBirdsCaribou"
vegetation <- "LandR.CS"
fire <- "fS"
runLandR <- TRUE
runBirds <- TRUE
runCaribou <- TRUE
originalDateAnalysis <- "29JUN20"
birdModelVersion <- 6
source("1_generalSetup.R")
source("2_generatingInputs.R")
if (runLandR){
  source("3_preamble.R")
  source("4_fittingModules.R")
}
source("5_runningSimulations.R")

#================================

# LandR.CS + SCFM
replicateNumber <- "run6"
vegetation <- "LandR.CS"
fire <- "SCFM"
runLandR <- TRUE
runBirds <- TRUE
runCaribou <- TRUE
birdModelVersion <- c(4, 6)
source("!runMe.R") # BLUE

# LandR + SCFM
replicateNumber <- "run6"
vegetation <- "LandR"
fire <- "SCFM"
runLandR <- TRUE
runBirds <- TRUE
runCaribou <- TRUE
birdModelVersion <- c(4, 6)
source("!runMe.R") # GREEN

# LandR + fS
replicateNumber <- "run6"
vegetation <- "LandR"
fire <- "fS"
runLandR <- TRUE
runBirds <- TRUE
runCaribou <- TRUE
birdModelVersion <- c(4, 6)
source("!runMe.R") # YELLOW

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
