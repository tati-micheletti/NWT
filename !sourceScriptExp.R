# LandR.CS + fireSense
replicate <- "run1"
vegetation <- "LandR.CS" # "LandR.CS" 
fire <- "fS" # fS
runLandR <- TRUE 
runBirds <- FALSE
runCaribou <- FALSE
checkMemory <- TRUE
# source("!runMeExp.R") # For now will not run this one, as I already ran: 09OCT19

# LandR.CS + SCFM
replicate <- "run1"
vegetation <- "LandR.CS" # "LandR.CS" 
fire <- "SCFM" # fS
runLandR <- TRUE 
runBirds <- FALSE
runCaribou <- FALSE
checkMemory <- TRUE
source("!runMeExp.R")

# LandR + SCFM 
replicate <- "run1"
vegetation <- "LandR" # "LandR.CS"
fire <- "SCFM" # fS
runLandR <- TRUE 
runBirds <- FALSE
runCaribou <- FALSE
checkMemory <- TRUE
source("!runMeExp.R")

# LandR + fS
replicate <- "run1"
vegetation <- "LandR" # "LandR.CS"
fire <- "fS" # fS
runLandR <- TRUE 
runBirds <- FALSE
runCaribou <- FALSE
checkMemory <- TRUE
source("!runMeExp.R")

factorialSimulations <- SpaDES.core::experiment2(#LandR.CS_fS = LandR.CS_fS,
                                 sim1 = LandR.CS_SCFM, 
                                 sim2 = LandR_SCFM,
                                 sim3 = LandR_fS,
                                 clearSimEnv = TRUE,
                                 replicates = 1)

