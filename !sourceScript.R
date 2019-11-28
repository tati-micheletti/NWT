# sourceScript
# How to start a simulation from the R command
# OBS.: 
# FINISHED RUNS:
# ALL run1 and (the ones that are available run2) in the folder of the 23OCT19!
# New experiment runs in the folder 

# 28th Oct: All results finished! 10th Nov 19: Putting to run the bird predictions with static climate and vegetation

# 10th Nov 19: I am currently running an experiment of only the climateStatic, for all scenarios

# 1. Define these variables. If not defined, the defaults will be ran ("SCFM" and "LandR", no caribou or birds)
# Run each one of these manually in a different screen

runOnlySimInit <- FALSE # TRUE to run experiment, FALSE to run simulations individually

# LandR.CS + fireSense
replicateNumber <- "run1"
vegetation <- "LandR.CS"
fire <- "fS"
runLandR <- TRUE
runBirds <- FALSE
runCaribou <- FALSE
runDynamic <- FALSE
runClimateStatic <- FALSE
runVegStatic <- FALSE
birdModelVersion <- 4 #6
source("!runMe.R") # PURPLE

# LandR.CS + SCFM
replicateNumber <- "run3"
vegetation <- "LandR.CS"
fire <- "SCFM"
runLandR <- FALSE
runBirds <- TRUE
runCaribou <- FALSE
runDynamic <- FALSE
runClimateStatic <- TRUE
runVegStatic <- FALSE
birdModelVersion <- 4 #6
source("!runMe.R") # BLUE

# LandR + SCFM
replicateNumber <- "run3"
vegetation <- "LandR"
fire <- "SCFM"
runLandR <- FALSE
runBirds <- TRUE
runCaribou <- FALSE
runDynamic <- FALSE
runClimateStatic <- TRUE
runVegStatic <- FALSE
birdModelVersion <- 4 #6
source("!runMe.R") # GREEN

# LandR + fS
replicateNumber <- "run3"
vegetation <- "LandR"
fire <- "fS"
runLandR <- FALSE
runBirds <- TRUE
runCaribou <- FALSE
runDynamic <- FALSE
runClimateStatic <- TRUE
runVegStatic <- FALSE
birdModelVersion <- 4 #6
source("!runMe.R") # YELLOW

if (runOnlySimInit){
  factorialSimulations <- SpaDES.experiment::experiment2(
    # LandR.CS_fS = LandR.CS_fS,
    LandR_SCFM = LandR_SCFM,
    # LandR.CS_SCFM = LandR.CS_SCFM,
    LandR_fS = LandR_fS,
    clearSimEnv = TRUE,
    replicates = 1, debug = 1,
    drive_auth_account = "tati.micheletti@gmail.com")
}
