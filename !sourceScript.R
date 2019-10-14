# sourceScript
# How to start a simulation from the R command

# 1. Define these variables. If not defined, the defaults will be ran ("SCFM" and "LandR", no caribou or birds)
replicate <- "run1"
vegetation <- "LandR.CS" # "LandR.CS"
fire <- "fS" # fS
runBirds <- TRUE
runCaribou <- FALSE
checkMemory <- TRUE


source("!runMe.R")
