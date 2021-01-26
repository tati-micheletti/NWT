############################################
############################################
#    I n  i t i a l     S e t u p          #  
############################################
############################################

# Authorize GDrive
if (!exists("usrEmail"))
  usrEmail <- if (pemisc::user() %in% c("tmichele", "Tati")) "tati.micheletti@gmail.com" else 
    "eliotmcintire@gmail.com"
googledrive::drive_auth(email = usrEmail)
# googledrive::drive_auth(email = usrEmail, use_oob = TRUE) # FIRST TIME!

# Update Packages and Modules
if (all(pemisc::user() %in% c("Tati", "tmichele"), 
        getwd() != "/home/tmichele/projects/NWT"))
  message(crayon::red(paste0("Your current working directory is ", getwd(), 
                 ". Please make sure it is correct!"), 
          immediate. = TRUE))

# Cleanup from previous runs
system(paste0("find ", getwd()," -empty -type d -delete"))

if (!exists("updateCRAN")) updateCRAN <- FALSE
if (!exists("updateGithubPackages")) updateGithubPackages <- FALSE
if (!exists("updateSubmodules")) updateSubmodules <- FALSE
if (!exists("isTest")) isTest <- FALSE # runMe

if (updateCRAN)
  update.packages(checkBuilt = TRUE, ask = FALSE)

if (updateGithubPackages){
  if (pemisc::user() %in% c("emcintir", "tmichele")) Sys.setenv("R_REMOTES_UPGRADE"="never")
  Pkg <- c("PredictiveEcology/Require@master",
           "PredictiveEcology/pemisc@development",
              "PredictiveEcology/LandR@master",
           ifelse(pemisc::user() %in% "tmichele", # <~~~~~~~~~~~~~~~~~~~~~~~~ HERE
                  "tati-micheletti/usefulFuns@fileMystery",
                  "PredictiveEcology/usefulFuns@development"),
              "ianmseddy/LandR.CS@master",
              "PredictiveEcology/fireSenseUtils@iterative",
              "PredictiveEcology/SpaDES@development")
  pkg <- lapply(Pkg, function(p){
    capture.output(devtools::install_github(p))
    })

  if (sum(sapply(pkg, length)) != 0){
    message(crayon::bgWhite(paste0("At least one new package was installed. ",
                               "Restarting R. Please re-run your code")))
    .rs.restartR()  
  } else {
    message(crayon::green(paste0("No new packages were installed. ",
                                   "Your setup will continue.")))
  }
}

library("Require")

if (!exists("updateSpaDES")) updateSpaDES <- FALSE
if (updateSpaDES){
  Pkg <- c("reproducible", "quickPlot", "SpaDES.tools", "SpaDES.core")
  lapply(X = Pkg, FUN = Require)
}

if (updateSubmodules){
  system(paste0("cd ", getwd(),
                " && git submodule foreach git pull"), wait = TRUE)
  system(paste0("cd ", getwd(),
                " && git pull"), wait = TRUE)
  system("git submodule", wait = TRUE) # checks if the branches and commits you are using are the correct ones
} 

library("usefulFuns")
library("data.table")
library("LandR")
library("LandR.CS")
library("SpaDES")
library("SpaDES.experiment")
library("raster")
library("plyr"); library("dplyr")
library("amc")
library("magrittr") # for piping
library("future")
library("future.apply")
library("fireSenseUtils")
library("parallel")

# Source all common functions

source("functions/defineRun.R")
source("functions/not_included/runNamesList.R")
source("functions/getFirePolygons_NFDB.R")
source("functions/getFirePoints_NFDB_V2.R")
source("functions/makeIpsForClusters.R")
source("functions/getBirdPredictedRasters.R")
source("functions/trackSeed.R")
source("functions/getFirePolys.R")
source("functions/getAnnualClimateZipURL.R")
source("functions/makeCMIandATA.R")
source('functions/runSquarenessTest.R')
source('functions/checkRasterStackIsInMemory.R')

if (!exists("vegetation")) vegetation <- "LandR" # Default if not provided
if (!exists("fire")) fire <- "SCFM" # Default if not provided
if (!exists("replicateNumber")) replicateNumber <- "run1" # Default if not provided #TOCHANGE
if (!exists("runOnlySimInit")) runOnlySimInit <- FALSE # Default if not provided

definedRun <- defineRun(replicateNumber = replicateNumber, 
                        vegetation = vegetation, 
                        fire = fire)
# vegetation = "LandR" or "LandR.CS"
# fire = "SCFM" or "fS"
# replicateNumber = i.e. "run1" or NULL for no id on the run

generalCacheFolder <- checkPath(file.path(getwd(), "cache"), create = TRUE)
user <- pemisc::user()

if (!exists("runName")) stop("You need to provide runName. This is the study area where the 
                             simulation should run for. To see all available study areas, use 
                             runNamesList(printTable = TRUE)")

paths <- list(inputPath = checkPath(file.path(getwd(), "inputs", runName), create = TRUE),
              modulePath = file.path(getwd(), "modules"),
              outputPath = checkPath(file.path(getwd(), "outputs",
                                               toupper(format(Sys.time(), "%d%b%y")),
                                               definedRun$whichRUN,
                                               replicateNumber),
                                     create = TRUE))
if (length(Paths$modulePath) == 1)
  paths$modulePath <- c(paths$modulePath, file.path(paths$modulePath, "scfm/modules"))
if (isTest)
  paths$outputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                           replacement = "Tests")

inputsCache <- checkPath(file.path(generalCacheFolder, "inputs", runName), create = TRUE)
preambleCache <- checkPath(file.path(generalCacheFolder, "preamble", runName), create = TRUE)        
fittingCache <- checkPath(file.path(generalCacheFolder, "fitting", runName), create = TRUE)
simulationsCache <- checkPath(file.path(generalCacheFolder, "simulations", runName), create = TRUE)
posthocCache <- checkPath(file.path(generalCacheFolder, "posthoc", runName), create = TRUE)

SpaDES.core::setPaths(modulePath = paths$modulePath, 
                      inputPath = paths$inputPath, 
                      outputPath = paths$outputPath, 
                      cachePath = inputsCache)

message("Your current temporary directory is ", tempdir())
maxMemory <- 5e+12
scratchDir <- file.path("~/scratch")
raster::rasterOptions(default = TRUE)
options(rasterMaxMemory = maxMemory, rasterTmpDir = scratchDir)
if(dir.create(scratchDir)) system(paste0("chmod -R 777 ", scratchDir), wait = TRUE) 
rasterOptions(default = TRUE)
options(rasterMaxMemory = maxMemory, 
        rasterTmpDir = scratchDir)

# Sessting up the cache folder: it is hosted in the project's GDrive
cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"

# Setting all SpaDES options to be used in the project
.plotInitialTime <- NA
opts <- options(
  "spades.recoveryMode" = 2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = Paths$inputPath,
  "map.overwrite" = TRUE,
  "map.useParallel" = TRUE, #!identical("windows", .Platform$OS.type),
  "reproducible.futurePlan" = FALSE,
  "future.globals.maxSize" = if (pemisc::user("tmichele")) 6000*1024^2 else 6000*1024^2,
  "reproducible.cacheSaveFormat" = "qs",
  "reproducible.qsPreset" = "fast",
  "reproducible.inputPaths" = if (pemisc::user("emcintir")) "~/data" else paths$inputPath,
  "reproducible.quick" = FALSE,
  "reproducible.overwrite" = TRUE,
  "reproducible.useMemoise" = TRUE, # Brings cached stuff to memory during the second run
  "reproducible.useNewDigestAlgorithm" = TRUE,  # use the new less strict hashing algo
  "reproducible.useCache" = TRUE,
  "reproducible.cachePath" = Paths$cachePath,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCloud" = FALSE,
  "reproducible.polygonShortcut" = FALSE, # As of 26JAN21 this is not working for Alex
  "spades.moduleCodeChecks" = FALSE, # Turn off all module's code checking
  "spades.useRequire" = FALSE, # assuming all pkgs installed correctly # CHANGED on 31AUG20 
  # --> Eliot is working on it. Its returning an error: Error: invalid version specification ‘	3.3-13’
  "pemisc.useParallel" = TRUE
)
