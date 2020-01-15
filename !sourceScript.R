# sourceScript
# How to start a simulation from the R command
# 1. Define these variables. If not defined, the defaults will be ran ("SCFM" and "LandR", no caribou or birds)
# Run each one of these manually in a different screen
# These are the othe parameters that can be set here:
# 1. originalDateAnalysis (should be passed in format DDMMMYY; i.e. 10JAN20; used only if runLandR == FALSE and runBirds == TRUE)

runOnlySimInit <- FALSE # TRUE to run experiment, FALSE to run simulations individually

# LandR.CS + fireSense
replicateNumber <- "run2"
vegetation <- "LandR.CS"
fire <- "fS"
runLandR <- TRUE
runBirds <- TRUE
runCaribou <- TRUE
birdModelVersion <- c(4, 6)
source("!runMe.R") # PURPLE

# LandR.CS + SCFM
replicateNumber <- "run3"
vegetation <- "LandR.CS"
fire <- "SCFM"
runLandR <- TRUE
runBirds <- TRUE
runCaribou <- TRUE
birdModelVersion <- c(4, 6)
source("!runMe.R") # BLUE

# LandR + SCFM
replicateNumber <- "run2"
vegetation <- "LandR"
fire <- "SCFM"
runLandR <- TRUE
runBirds <- TRUE
runCaribou <- TRUE
birdModelVersion <- c(4, 6)
source("!runMe.R") # GREEN

# LandR + fS
replicateNumber <- "run2"
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
