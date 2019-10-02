# sourceScript
# How to start a simulation from the R command

# 1. Define these variables. If not defined, the defaults will be ran ("SCFM" and "LandR", no caribou or birds)

replicate <- "run1"
vegetation <- "LandR" # "LandR.CS"
fire <- "SCFM" # fS
runBirds <- FALSE
runCaribou <- FALSE

source("!runMe.R")