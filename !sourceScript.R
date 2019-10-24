# sourceScript
# How to start a simulation from the R command
# OBS.: 
# FINISHED RUNS:
# The run1 from LandR.CS + fS is in the folder 09OCT19
# There are 3 runs for the other combinarions (LandR + fS, LandR + SCFM, LandR.CS + SCFM) on the 23OCT19: only 2031-2071

# 1. Define these variables. If not defined, the defaults will be ran ("SCFM" and "LandR", no caribou or birds)
# Run each one of these manually in a different screen

# LandR.CS + fireSense
replicate <- "run1"
vegetation <- "LandR.CS" # "LandR.CS" 
fire <- "fS" # fS
runLandR <- FALSE 
runBirds <- TRUE
runCaribou <- FALSE
checkMemory <- TRUE
source("!runMe.R")

# LandR.CS + SCFM
replicate <- "run1"
vegetation <- "LandR.CS" # "LandR.CS" 
fire <- "SCFM" # fS
runLandR <- TRUE 
runBirds <- FALSE
runCaribou <- FALSE
checkMemory <- TRUE
source("!runMe.R")

# LandR + SCFM 
replicate <- "run1"
vegetation <- "LandR" # "LandR.CS"
fire <- "SCFM" # fS
runLandR <- TRUE 
runBirds <- FALSE
runCaribou <- FALSE
checkMemory <- TRUE
source("!runMe.R")

# LandR + fS
replicate <- "run1"
vegetation <- "LandR" # "LandR.CS"
fire <- "fS" # fS
runLandR <- TRUE 
runBirds <- FALSE
runCaribou <- FALSE
checkMemory <- TRUE
source("!runMe.R")
