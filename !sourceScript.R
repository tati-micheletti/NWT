# sourceScript
# How to start a simulation from the R command
# OBS.: 
# FINISHED RUNS:
# ALL run1 in the folder of the 23OCT19!
# ALL run2 in the folder of the 28OCT19!

# 28th Oct: All results finished!

# 1. Define these variables. If not defined, the defaults will be ran ("SCFM" and "LandR", no caribou or birds)
# Run each one of these manually in a different screen

# LandR.CS + fireSense
replicate <- "run2"
vegetation <- "LandR.CS" 
fire <- "fS"
runLandR <- FALSE 
runBirds <- TRUE
runCaribou <- FALSE
checkMemory <- TRUE
source("!runMe.R") # PURPLE

# LandR.CS + SCFM
replicate <- "run2"
vegetation <- "LandR.CS" 
fire <- "SCFM"
runLandR <- FALSE 
runBirds <- TRUE
runCaribou <- FALSE
checkMemory <- TRUE
source("!runMe.R") # BLUE

# LandR + SCFM 
replicate <- "run2"
vegetation <- "LandR"
fire <- "SCFM"
runLandR <- FALSE 
runBirds <- TRUE
runCaribou <- FALSE
checkMemory <- TRUE
source("!runMe.R") # GREEN

# LandR + fS
replicate <- "run2"
vegetation <- "LandR"
fire <- "fS"
runLandR <- FALSE 
runBirds <- TRUE
runCaribou <- FALSE
checkMemory <- TRUE
source("!runMe.R") # YELLOW
