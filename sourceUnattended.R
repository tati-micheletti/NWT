usrEmail <- "tati.micheletti@gmail.com" # Your e.mail for GDrive authorization
updateCRAN <- FALSE
updateGithubPackages <- FALSE
updateSubmodules <- FALSE
isTest <- FALSE # runMe
Sys.sleep(1)
runOnlySimInit <- FALSE # TRUE to run experiment, FALSE to run simulations individually
onlyLoadDEOptim <- TRUE
runName <- "NWT_BCR6"
Sys.sleep(1)
replicateNumber <- paste0("run", RUN)
Sys.sleep(1)
originalDateAnalysis <- "14AUG20" # <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
runLandR <- FALSE # <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
runBirds <- TRUE
runCaribou <- FALSE
birdModelVersion <- "6a"
Sys.sleep(3)
source("1_generalSetup.R")
source("2_generatingInputs.R")
if (all(runLandR, fire != "SCFM")){
  source("3_preamble.R")
  source("4_fittingModules.R")
}
source("5_runningSimulations.R")
