
# Before running this script, read !sourceScript to know the 4 
# parameters that needed to define the run 

# googledrive::drive_auth(use_oob = TRUE) # USE ONLY ONCE, the first time you are running the project 
# USING RStudio Server.

############################################
############################################
#    I n  i t i a l     S e t u p          #  
############################################
############################################

if (!exists("usrEmail"))
  usrEmail <- if (pemisc::user() %in% c("tmichele", "Tati")) "tati.micheletti@gmail.com" else 
                                                            "eliotmcintire@gmail.com"
googledrive::drive_auth(email = usrEmail)

if (pemisc::user() %in% c("Tati", "tmichele"))
  setwd("/mnt/data/Micheletti/NWT")
if (!exists("updateCRAN")) updateCRAN <- FALSE
if (!exists("updateGithubPackages")) updateGithubPackages <- FALSE
if (!exists("updateSubmodules")) updateSubmodules <- FALSE
if (!exists("isTest")) isTest <- FALSE # runMe

if (updateCRAN)
  update.packages(checkBuilt = TRUE, ask = FALSE)

if (updateGithubPackages){
  if (pemisc::user("emcintir")) Sys.setenv("R_REMOTES_UPGRADE"="never")
  devtools::install_github("PredictiveEcology/reproducible@139-spatial-updates")
  devtools::install_github("tati-micheletti/usefulFuns@development") # Updates LandR
  devtools::install_github("achubaty/amc@development")
  devtools::install_github("PredictiveEcology/pemisc@development")
  devtools::install_github("PredictiveEcology/map@development")
  devtools::install_github("PredictiveEcology/SpaDES.core@lowMemory") # Updates SpaDES.tools and SpaDES.core quickPlot
  devtools::install_github("PredictiveEcology/SpaDES.tools@allowOverlap") # Updates SpaDES.tools and SpaDES.core quickPlot
  devtools::install_github("PredictiveEcology/LandR@development") # Updates SpaDES.tools and SpaDES.core quickPlot
  devtools::install_github("tati-micheletti/LandR.CS@master") # Climate sensitivity in LandR
  devtools::install_github("PredictiveEcology/fireSenseUtils@development")
  # pedev::updateGit("fireSenseUtils", branch = "development")
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

# Source all common functions
# invisible(sapply(X = list.files(file.path(getwd(), "functions"), full.names = TRUE), FUN = source))
source("functions/not_included/pathsSetup.R")
source("functions/defineRun.R")

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

user <- pemisc::user()
whichComputer <- if (user == "tmichele") "BorealCloud" else "LocalMachine"
if (whichComputer == "BorealCloud" & basename(getwd()) != "NWT"){
  setwd(file.path(getwd(), "NWT"))
}

paths <- setPaths(inputPath = file.path(getwd(), "inputs"),
                    modulePath = file.path(getwd(), "modules"),
                    outputPath = checkPath(file.path(getwd(), "outputs", 
                                                     toupper(format(Sys.time(), "%d%b%y")),
                                                     definedRun$whichRUN, 
                                                     replicateNumber), 
                                           create = TRUE),
                    cachePath = file.path(getwd(), "cache"))
if (length(Paths$modulePath) == 1) 
  paths$modulePath <- c(paths$modulePath, file.path(paths$modulePath, "scfm/modules"))
if (isTest)
  paths$outputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                         replacement = "Tests")

SpaDES.core::setPaths(modulePath = paths$modulePath, 
                      inputPath = paths$inputPath, 
                      outputPath = paths$outputPath, 
                      cachePath = paths$cachePath)

if (pemisc::user() %in% c("Tati", "tmichele")){
  workDirectory <- getwd()
  message("Your current temporary directory is ", tempdir())
  unlink(file.path(paths$cachePath, "tmp"), recursive = TRUE, force = TRUE)
  tempFolder <- asPath(reproducible::checkPath(file.path(dirname(workDirectory), "tmp"), 
                                               create = TRUE))
  unixtools::set.tempdir(tempFolder)
}

maxMemory <- 5e+12
scratchDir <- checkPath(path = paste0("/mnt/tmp/rasterTMP/", pemisc::user()), create = TRUE)
#Here we check that the creation of the folder worked (we might have problems 
#with writting access, only tested with my own user)
if(dir.create(scratchDir)) system(paste0("sudo chmod -R 777 /mnt/tmp/rasterTMP"), wait = TRUE) 
rasterOptions(default = TRUE)
options(rasterMaxMemory = maxMemory, rasterTmpDir = scratchDir)

# Sessting up the cache folder: it is hosted in the project's GDrive
cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
tilePath <- file.path(paths$outputPath, "tiles")

# Setting all SpaDES options to be used in the project
.plotInitialTime <- NA
opts <- options(
  "spades.recoveryMode" = 2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = paths$inputPath,
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
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
  "reproducible.cachePath" = paths$cachePath,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCloud" = FALSE,
  "spades.moduleCodeChecks" = FALSE, # Turn off all module's code checking
  "spades.useRequire" = TRUE, # assuming all pkgs installed correctly
  "pemisc.useParallel" = TRUE
)

#################################################################################
################################## SIMULATION SET UP ############################
#################################################################################

if (!exists("runName")) stop("You need to provide runName. This is the study area where the 
simulation should run for. To see all available study areas, use 
runNamesList()")

stepCacheTags <- c("2_simulationSetup")

studyArea <- Cache(prepInputs,
                   url = runNamesList()[RunName == runName, studyArea],
                   destinationPath = Paths$inputPath,
                   filename2 = NULL,
                   userTags = c("studyArea", runName, stepCacheTags),
                   omitArgs = c("destinationPath", "filename2"))

rasterToMatch <- Cache(prepInputs, url = runNamesList()[RunName == runName, rasterToMatch],
                       studyArea = studyArea,
                       destinationPath = Paths$inputPath,
                       userTags = c("rasterToMatch", runName, stepCacheTags),
                       omitArgs = c("destinationPath", "filename2"))


####  Prep Layers: Exclude water, rocks and ice from flammableRTM --> NA
watersRaster <- Cache(prepInputs,
                     url = runNamesList()[RunName == runName, watersRaster],
                     destinationPath = Paths$inputPath,
                     studyArea = studyArea,
                     rasterToMatch = rasterToMatch,
                     filename2 = NULL,
                     userTags = c("rasterToMatch", runName, stepCacheTags),
                     omitArgs = c("destinationPath", "filename2"))

watersVals <- raster::getValues(watersRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA
watersValsToChange <- watersVals 
watersValsToChange[!is.na(watersValsToChange) & watersValsToChange != 1] <- NA
waterRaster <- raster::setValues(x = watersRaster, waterVals)
watersValsToChange <- watersVals 
watersValsToChange[!is.na(watersValsToChange) & watersValsToChange != 2] <- NA
wetlandsRaster <- raster::setValues(x = watersRaster, waterVals)
watersValsToChange <- watersVals 
watersValsToChange[!is.na(watersValsToChange) & watersValsToChange != 3] <- NA
uplandsRaster <- raster::setValues(x = watersRaster, waterVals)

rstLCC <- Cache(prepInputs,
                url = paste0("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/",
                             "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
                targetFile = file.path(Paths$inputPath, "LCC2005_V1_4a.tif"),
                archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
                destinationPath = Paths$inputPath,
                studyArea = studyArea,
                rasterToMatch = rasterToMatch,
                maskWithRTM = TRUE,
                method = "bilinear",
                datatype = "INT2U",
                filename2 = TRUE,
                userTags = c(runName, stepCacheTags, 
                             "rstLCC", "prepInputsrstLCC_rtm"),
                omitArgs = c("destinationPath", "filename2"))

# Ice/snow = 39
# Water (LCC05) = 37:38
# Rocks = 33
# Urban = 36

nonFlammClass <- c(33, 36:39)
flammableRTM <- rasterToMatch
# Remove LCC non flammable classes first
flammableRTM[rstLCC[] %in% nonFlammClass] <- NA
# Remove more detailed water from DUCKS layer
flammableRTM[waterRaster[] == 1] <- NA

studyAreaPSP <- Cache(prepInputs,
                      url = runNamesList()[RunName == runName, studyAreaPSP],
                      alsoExtract = "similar",
                      destinationPath = Paths$inputPath,
                      userTags = c("objectName:studyAreaPSP", "extension:BCR6",
                                   runName, stepCacheTags))

ecoRegionRAS <- Cache(prepInputs,
                      targetFile = "ecoRegionRAS.rds",
                      url = runNamesList()[RunName == runName, ecoRegionRaster],
                      alsoExtract = "similar",
                      destinationPath = Paths$inputPath,
                      studyArea = studyArea,
                      fun = "readRDS",
                      userTags = c(runName, stepCacheTags,
                                   "prepInputsEcoRegion_RAS"),
                      omitArgs = c("destinationPath", "filename2"))

if (!exists("climateModel")) climateModel <- "CCSM4_85" # Default if not provided
if (!climateModel %in% c("CCSM4_85", "CCSM4_45")) stop("Other climate scenarios are still not implemented.")

# TODO Still need to implement this for other provinces. Also need to implement other models! 
# These have been done ONLY with NWT shapefile, I believe!

cmi.url <- ifelse(climateModel == "CCSM4_85", "https://drive.google.com/open?id=1OcVsAQXKO4N4ZIESNmIZAI9IZcutctHX", 
                  "https://drive.google.com/open?id=1ERoQmCuQp3_iffQ0kXN7SCQr07M7dawv")
cmi.tf <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_CMI2011-2100.grd", "Canada3ArcMinute_CCSM4_45_CMI2011-2100.grd")
cmi.arc <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_CMI2011-2100.zip", "Canada3ArcMinute_CCSM4_45_CMI2011-2100.zip")
alsoExt <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_CMI2011-2100.gri", "Canada3ArcMinute_CCSM4_45_CMI2011-2100.gri")
ata.url <- ifelse(climateModel == "CCSM4_85", "https://drive.google.com/open?id=1jyfq-7wG4a7EoyNhirgMlq4mYnAvoOeY", 
                  "https://drive.google.com/open?id=1OA67hJDJunQbfeG0nvCnwd3iDutI_EKf")
ata.tf <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_ATA2011-2100.grd", "Canada3ArcMinute_CCSM4_45_ATA2011-2100.grd")
ata.arc <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_ATA2011-2100.zip", "Canada3ArcMinute_CCSM4_45_ATA2011-2100.zip")
alsoExt <- ifelse(climateModel == "CCSM4_85", "Canada3ArcMinute_CCSM4_85_ATA2011-2100.gri", "Canada3ArcMinute_CCSM4_45_ATA2011-2100.gri")
RCP <- ifelse(climateModel == "CCSM4_85", "85", "45")
climateModelType = ifelse(climateModel == "CCSM4_85", "CCSM4", "CanESM2")# CanESM2 is NOT implemented yet. Here just figurative
ensemble <- ifelse(climateModel == "CCSM4_85", "", "r11i1p1")
climateResolution <- "3ArcMin" # Only available for now, matches the created layers for all modules
climateFilePath <- ifelse(climateModel == "CCSM4_85", "https://drive.google.com/open?id=17idhQ_g43vGUQfT-n2gLVvlp0X9vo-R8", 
                          "https://drive.google.com/open?id=1U0TuYNMC75sQCkZs7c4EcBRLcjqeVO6N")

# Equivalency table for tree species
data("sppEquivalencies_CA", package = "LandR")
sppEquivCol <- runName

# Make NWT spp equivalencies
sppEquivalencies_CA[, paste0(runName) := c(Betu_Pap = "Betu_Pap", 
                               Lari_Lar = "Lari_Lar", 
                               Pice_Gla = "Pice_Gla",
                               Pice_Mar = "Pice_Mar", 
                               Pinu_Ban = "Pinu_Ban", 
                               Popu_Tre = "Popu_Tre")[Boreal]]

sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(get(runName))]
sppEquivalencies_CA$EN_generic_short <- sppEquivalencies_CA[[paste0(runName)]]
# Fix EN_generic_short for plotting. Needs to have all names. Don't have time now.
sppColorVect <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, 
                                 sppEquivCol = sppEquivCol,
                                 palette = "Set3")
mixed <- structure("#D0FB84", names = "Mixed")
sppColorVect[length(sppColorVect)+1] <- mixed
attributes(sppColorVect)$names[length(sppColorVect)] <- "Mixed"

if (!exists("times"))
  times <- list(start = 2011, end = 2100)

#SCFM
defaultInterval <- 1.0
defaultPlotInterval <- NA
defaultInitialSaveTime <- NA 

# FireSense SpreadFit
#  lower asymptote, upper asymptote, (inflection point), slope at inflection pt, asymmetry
lowerParams <- c(0, 0.001, 0.001, 0.001, 0.001, 0.001)
upperParams <- c(6, 3, 3, 5, 5, 3)
# Spread log function bounds
lower <- c(0.22, 0.1, 0.5, lowerParams)
upper <- c(0.3, 10, 4, upperParams)

# Setting up IP's for paralelizing
machines <- data.frame(
  ipEnd =          c(97, 216, 189, 187, 68, 174),
  availableCores = c(28, 28,  28,  28,  35, 28))
makeIps <- function(machines, ipStart = "10.20.0.", N = length(lower) * 10) {
  ipsEnd <- rep(machines$ipEnd, ceiling(machines$availableCores/ (sum(machines$availableCores)/N)) )
  ips <- paste0(ipStart, ipsEnd)
  i <- 0
  while(length(ips) > N) {
    i <- i + 1
    i <- i %% NROW(machines)
    j <- i+1
    ips <- ips[-which(endsWith(ips, suffix = as.character(machines$ipEnd[i])))[1]]  
  }
  sort(ips)
}
cores <- makeIps(machines)

parameters <- list(
  #SCFM
  # ".progress" = list(type = "text", interval = 1),
  Biomass_speciesParameters = list(
    "sppEquivCol"  = sppEquivCol,
    ".useCache" = c(".inputObjects", "init"),
         "GAMMiterations" = 2, 
         "GAMMknots" = list(
           "Betu_Pap" = 3,
           "Lari_Lar" = 4,
           "Pice_Gla" = 3,
           "Pice_Mar" = 4,
           "Pinu_Ban" = 3,
           "Popu_Tre" = 4),
         "minimumPlotsPerGamm" = 40,
         "constrainMortalityShape" = list(
           "Betu_Pap" = c(15,25),
           "Lari_Lar" = c(20,25),
           "Pice_Gla" = c(15,25),
           "Pice_Mar" = c(15,25),
           "Pinu_Ban" = c(15,25),
           "Popu_Tre" = c(15,25)
         ),
         "quantileAgeSubset" = list(
           "Betu_Pap" = 95,
           "Lari_Lar" = 95,
           "Pice_Gla" = 95,
           "Pice_Mar" = 95,
           "Pinu_Ban" = 95,
           "Popu_Tre" = 99
         )
    ),
  scfmLandcoverInit = list(
    ".plotInitialTime" = NA
  ),
  scfmSpread = list(
    "pSpread" = 0.235,
    ".useCache" = c(".inputObjects", "init"),
    "returnInterval" = defaultInterval,
    "startTime" = times$start,
    ".plotInitialTime" = NA,
    ".plotInterval" = defaultPlotInterval,
    ".saveInitialTime" = defaultInitialSaveTime,
    ".saveInterval" = defaultInterval),
  scfmRegime = list(fireCause = "L"),
  scfmDriver = list(
    targetN = 1000),
  # LandR_Biomass
  Biomass_core = list(
    "successionTimestep" = 10,
    ".plotInitialTime" = NA,
    ".saveInitialTime" = NA,
    "seedingAlgorithm" = "wardDispersal",
    ".useCache" = c(".inputObjects", "init"),
    "initialBiomassSource" = "cohortData",
    "growthAndMortalityDrivers" = definedRun$growthAndMortalityDrivers,
    ".useParallel" = 2),
  Biomass_borealDataPrep = list(
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(usefulFuns::changeTraits(speciesTable = sim$species, param = "seeddistance_max",
                                 facMult = 0.4, species = c("Betu_Pap", "Popu_Tre")))
    ),
    "useCloudCacheForStats" = TRUE,
    "sppEquivCol" = sppEquivCol,
    "successionTimestep" = 10,
    "pixelGroupAgeClass" = 20,
    ".useCache" = c(".inputObjects", "init"),
    "subsetDataBiomassModel" = 50
  ),
  Biomass_regeneration = list(
    "fireTimestep" = 1,
    "fireInitialTime" = times$start
  ),
  gmcsDataPrep = list(
    "GCM" = "CCSM4_RCP8.5"),
  ".useCache" = c(".inputObjects"),
  fireSense_IgnitionPredict = list(
    ".useCache" = c(".inputObjects"),
    "data" = c("MDC06", "LCC"),
    "modelObjName" = "fireSense_FrequencyFitted"),
  fireSense_EscapePredict = list(
    "data" = c("MDC06", "LCC")),
  # fireSense = list(
  #   "mapping" = list(
  #     "spreadProbRaster" = "fireSense_SpreadPredicted"
  #     )),
  fireSense_NWT_DataPrep = list(
    ".useCache" = c(".inputObjects", "init"),
    "train" = FALSE,
    "RCP" = RCP,
    ".useCache" = c(".inputObjects"),
    "climateModel" = climateModelType,
    "ensemble" = ensemble,
    "climateResolution" = climateResolution,
    "climateFilePath" = climateFilePath),
  # Caribou Population Growth
  caribouPopGrowthModel = list(
    ".plotInitialTime" = NULL,
    "recoveryTime" = 40,
    ".useCache" = c(".inputObjects"),
    ".useDummyData" = FALSE,
    ".growthInterval" = 5),
  # fireSense
  fireSense_dataPrep = list(
    "whichModulesToPrepare" = "fireSense_SpreadPredict",
    "RCP" = RCP,
    "climateModel" = climateModelType,
    "ensemble" = ensemble,
    "climateResolution" = climateResolution,
    "climateFilePath" = climateFilePath
  ),
  fireSense_SpreadFit = list(
    "formula" = formula(~ 0 + weather + class1 + class2 + class3 + class4 + class5), # Formula of the statistical model
    "fireAttributesFireSense_SpreadFit" = "fireAttributesFireSense_SpreadFit", # Default
    # data = c("beta", "theta"),
    "data" = c("weather", "class1", "class2", "class3", "class4", "class5"),
    # Here are the bounds for: 5 parameters for log fun + n parameters for the model (i.e. n terms of a formula)
    lower = lower,
    upper = upper,
    cores = cores, #rep("localhost", 40), #cores,
    iterDEoptim = 300,
    iterStep = 25,
    minBufferSize = 1000,
    debugMode = FALSE,#isRstudioServer(), # DEoptim may spawn many machines via PSOCK --> may be better from cmd line
    rescaleAll = TRUE,
    maxFireSpread = 0.3,
    objfunFireReps = 100,
    verbose = TRUE,
    trace = 1,
    visualizeDEoptim = TRUE,
    cacheId_DE = "56769e2b2edfe8ab",#  "c3af84b504e99a5d", # This is NWT DEoptim Cache
    cloudFolderID_DE = "1kUZczPyArGIIkbl-4_IbtJWBhVDveZFZ",
    useCloud_DE = TRUE
    
  )
)

succTS <- c(seq(times$start, times$end, 
                by = parameters$Biomass_core$successionTimestep), times$end)
outputsLandR <- data.frame(
  objectName = rep(c("burnMap",
                     "cohortData",
                     "simulationOutput",
                     "pixelGroupMap",
                     "simulatedBiomassMap",
                     "ANPPMap",
                     "mortalityMap",
                     "climateLayers",
                     "MDC06"), each = length(succTS)),
  saveTime = c(rep(succTS, times = 9))
)
lastYears <- data.frame(objectName = c("predictedCaribou", "plotCaribou", 
                                       "fireRegimeRas", "speciesEcoregion", 
                                       "species", "gcsModel", "mcsModel"),
                        saveTime = times$end)
if (length(usefulFuns::grepMulti(x = definedRun$modules, "Biomass_core")) != 0){
  clim <- data.frame(objectName = rep(c("fireSense_IgnitionPredicted", 
                                        "fireSense_EscapePredicted", "burnSummary", 
                                        "successionLayers", "activePixelIndex"), 
                                      each = 3),
                     saveTime = rep(c(times$start, round((times$start + times$end)/2, 0), times$end), 
                                    times = 1))
} else {
  clim <- NULL
}

outputsLandR <- unique(rbind(outputsLandR, lastYears, clim))

objects <- list(
  "studyAreaPSP" = studyAreaPSP,
  "rasterToMatchLarge" = rasterToMatch,
  "rasterToMatch" = rasterToMatch,
  "studyAreaLarge" = studyArea,
  "studyArea" = studyArea,
  "sppEquiv" = sppEquivalencies_CA,
  "sppEquivCol" = sppEquivCol,
  "sppColorVect" = sppColorVect,
  "omitNonVegPixels" = TRUE,
  "cloudFolderID" = cloudFolderID,
  "studyArea" = studyArea,
  "waterRaster" = waterRaster,
  "fireRegimePolys" = studyArea,
  "ecoregionRst" = ecoRegionRAS,
  "flammableRTM" = flammableRTM,
  "uplandRaster" = uplandRaster
)

data.table::setDTthreads(2) # Data.table has all threads by default, 
# which is inconveninent and unecessary. Will try setting it for only 2 cores.  

if (runOnlySimInit){
  spadesFun <- "simInit"
} else {
  spadesFun <- "simInitAndSpades"
}

#########################################################
##                   PREAMBLE                          ##
#########################################################
# if (!exists("runFireSenseFit")) runFireSenseFit <- FALSE
# if (runFireSenseFit){
  # if (!exists("prepCohortData")) prepCohortData <- FALSE
# Preamble. If already ran (i.e. objs cohortData2011 and cohortData2001 
  # exist in inputs folder) this should NOT be run i.e. FALSE)
  # if (prepCohortData){
    # tempPaths <- getPaths()
    # tempPaths$outputPath <- tempPaths$inputPath
    # outputsPreamble <- data.frame(objectName = c("cohortData", "pixelGroupMap"),
    #                               saveTime = 2001,
    #                               file = c("cohortData2001_fireSense.rds", 
    #                                        "pixelGroupMap2001_fireSense.rds"))
    # 
    # # 1. Run borealBiomassDataPrep ALONE and save: cohortData + pixelGroupMap: will be used 
    # # in fireSense_SizeFit and fireSense_SpreadFit (later on, will be also used in Ignition and Escape fits)
    # # 271 unique using ecoregion
    # # 973 unique using ecodistrict
    # 
    # biomassMaps2001 <- Cache(simInitAndSpades, times = list(start = 2001, end = 2001),
    #                          params = parameters,
    #                          modules = list("Biomass_borealDataPrep"),
    #                          objects = objects,
    #                          paths = tempPaths, 
    #                          #useCloud = TRUE, cloudFolderID = cloudFolderID,
    #                          loadOrder = "Biomass_borealDataPrep",
    #                          outputs = outputsPreamble, #clearSimEnv = TRUE,
    #                          userTags = c("objective:preambleBiomassDataPrep", 
    #                                       "time:year2001", "version:fixedZeros"))
    # # 2. Load these:
    # speciesLayers2011 <- Cache(loadkNNSpeciesLayersValidation,
    #                            dPath = Paths$inputPath,
    #                            rasterToMatch = rasterToMatch,
    #                            studyArea = studyArea,   ## Ceres: makePixel table needs same no. pixels for this, RTM rawBiomassMap, LCC.. etc
    #                            sppEquiv = sppEquivalencies_CA,
    #                            knnNamesCol = "KNN",
    #                            sppEquivCol = sppEquivCol,
    #                            thresh = 10,
    #                            url = "http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2011-attributes_attributs-2011/",
    #                            userTags = c("preamble", "speciesLayers2011"))
    # 
    # outputsPreamble <- data.frame(objectName = c("cohortData", "pixelGroupMap"),
    #                               saveTime = 2011,
    #                               file = c("cohortData2011_fireSense.rds", "pixelGroupMap2011_fireSense.rds"))
    # 
    # objectsPre <- objects
    # objectsPre$speciesLayers <- speciesLayers2011
    # 
    # # and pass as object to a second call of Biomass_borealDataPrep. Save cohortData + pixelGroupMap.
    # biomassMaps2011 <- Cache(simInitAndSpades, times = list(start = 2011, end = 2011),
    #                          params = parameters,
    #                          modules = list("Biomass_borealDataPrep"),
    #                          objects = objectsPre,
    #                          paths = tempPaths, #useCache = "overwrite",
    #                          loadOrder = "Biomass_borealDataPrep",
    #                          clearSimEnv = TRUE,
    #                          outputs = outputsPreamble,
    #                          userTags = c("objective:preambleBiomassDataPrep", "time:year2011"))
    # 
  # }
  
  # The parameters for
  # weather : need to be positive, but very low... as the sum of all needs to be < 0.245
  # class1 : needs to be negative, as it influences negatively the fire
  # class2 : needs to be negative, as it influences negatively the fire
  # class3 : needs to be positive, as fire spreads through conifers
  # class4 : needs to be positive + , as fire spreads through Jack Pine even better
  # class 5 : can be either...


  # Should let the classes take 100% of it if needs
  # MDC, Class 1:5 coefficients' bounds
# (table(cores))
# 
# parameters_fS <- list(
#   fireSense_SpreadFit = list(
#     formula = formula, # Formula of the statistical model
#     fireAttributesFireSense_SpreadFit = "fireAttributesFireSense_SpreadFit", # Default
#     data = c("weather", "class1", "class2", "class3", "class4", "class5"),
#     lower = lower,
#     upper = upper,
#     cores = cores,
#     iterDEoptim = 300,
#     iterStep = 25,
#     minBufferSize = 1000,
#     debugMode = FALSE, #isRstudioServer(), # DEoptim may spawn many machines via PSOCK --> may be better from cmd line
#     rescaleAll = TRUE,
#     maxFireSpread = 0.3,
#     objfunFireReps = 100,
#     verbose = TRUE,
#     trace = 1,
#     visualizeDEoptim = TRUE,
#     cacheId_DE = "56769e2b2edfe8ab",# This is NWT DEoptim Cache
#     cloudFolderID_DE = "1kUZczPyArGIIkbl-4_IbtJWBhVDveZFZ",# This is NWT DEoptim Cache folder
#     useCloud_DE = TRUE
#   )
# )
# 
# # Run the simulation
# sim <- simInitAndSpades(
#   modules = modules_fS,
#   objects = objects_fS,
#   params = parameters_fS,
#   times = times_fS
# )

# Iteration: 399 bestvalit: 362.469704 bestmemit:    0.117161    0.270096    7.032851    3.281557    0.529472    2.851134    1.839160    2.506558    2.243100    2.689801
# best <- c(0.117161,0.270096,7.032851,3.281557,0.529472,2.851134,1.839160,2.506558,2.243100,2.689801) # Eliot,  27th April 2020
# fireSense_SpreadFitted <- sim$fireSense_SpreadFitted # Extract the fitted model from the sim object  
# saveRDS(fireSense_SpreadFitted, file.path(Paths$inputPath, "fireSense_SpreadFitted.rds"))
# library("googledrive")
# drive_upload(file.path(Paths$inputPath, "fireSense_SpreadFitted.rds"), 
#              as_id("1Uvba7IkfTfgvStQuA3xOKRyceR56mqIB"))

# } else {
  # fireSense_SpreadFitted <- readRDS(file.path(Paths$input, 
  #                                             "fireSense_SpreadFitted.rds"))
# }

library("googledrive")

tryCatch(googledrive::drive_download(file = googledrive::as_id("1EetOiGxAq-QTZCVU9Q6Y3I26tqjZm3Oi"), 
                                     path = file.path(getPaths()$inputPath, "fireSense_FrequencyFitted.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})
tryCatch(googledrive::drive_download(file = googledrive::as_id("11CrK9PfNJzkU5cZg9-JYYzyr-VP23W0x"), 
                                     path = file.path(getPaths()$inputPath, "fireSense_EscapeFitted.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})
tryCatch(googledrive::drive_download(file = googledrive::as_id(""), 
                                     path = file.path(getPaths()$inputPath, "fireSense_SpreadFitted.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})

inputs <- data.frame(
  files = c(file.path(getPaths()$inputPath, "fireSense_FrequencyFitted.rds"),
            file.path(getPaths()$inputPath, "fireSense_EscapeFitted.rds"),
            file.path(getPaths()$inputPath, "fireSense_SpreadFitted.rds")),
  functions = "base::readRDS",
  stringsAsFactors = FALSE
)

#########################################################
##                      RUNS                           ##
#########################################################

reproducible::clearCache(userTags = '1d4dc7c4cb7869e1', ask = FALSE)
if (!exists("runLandR")) runLandR <- FALSE # Default if not provided
if (runLandR){
  message(crayon::red(paste0("Starting ", ifelse(runOnlySimInit, "simInit", "simulations"), " for ", 
                             definedRun$whichRUN, " ", definedRun$whichReplicate)))
  assign(x = definedRun$whichRUN, do.call(get(spadesFun), args = list(inputs = inputs, times = times,
                                                   params = parameters,
                                                   modules = definedRun$modules,
                                                   objects = objects,
                                                   paths = getPaths(),
                                                   loadOrder = unlist(definedRun$modules),
                                                   outputs = outputsLandR)))
  t2 <- Sys.time()
  message(crayon::green(paste0("Finished ", ifelse(runOnlySimInit, "simInit", "simulations")," for ", 
                               definedRun$whichRUN, ". Elapsed time: ", t2-t1)))
  if (!runOnlySimInit){
    saveRDS(object = get(definedRun$whichRUN),
            file = file.path(getPaths()$outputPath, paste0(definedRun$whichRUN,
                                                      toupper(format(Sys.time(), 
                                                                     "%d%b%y_%Hh%Mm%Ss")))))
    message(crayon::magenta(paste0("Saved simulations for ", definedRun$whichRUN, ". Elapsed time: "
                                   , Sys.time()-t2)))
rm(list = definedRun$whichRUN)
gc()
  } # End !runOnlySimInit
} # End runLandR

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ BIRDS MODEL 1

if (!exists("runBirds")) runBirds <- FALSE # Default if not provided
if (runBirds){
  if (!exists("birdModelVersion")) birdModelVersion <- 6 # Default if not provided
  predictionIntervals <- 20
  message(crayon::yellow(paste0("Starting simulations for BIRDS using ", definedRun$whichRUN, " ", 
                                definedRun$whichReplicate)))
  
  bMod <- ifelse(length(birdModelVersion) == 1, birdModelVersion, birdModelVersion[1])
  parameters <- list(
    birdsNWT = list(
      "lowMem" = TRUE,
      "scenario" = paste(replicateNumber, vegetation, fire, sep = "_"), # THIS IS THE CORRECT
      "useStaticPredictionsForNonForest" = TRUE,
      "useOnlyUplandsForPrediction" = TRUE,
      "baseLayer" = 2005,
      "overwritePredictions" = FALSE,
      "useTestSpeciesLayers" = FALSE, # Set it to false when you actually have results from 
      # LandR_Biomass simulations to run it with
      "useParallel" = FALSE, # Using parallel in windows is currently not working.
      "predictionInterval" = predictionIntervals,
      "quickLoad" = TRUE,
      "version" = bMod, # VERSION 6 of the modules has both climate and vegetation as covariates for the model
      "RCP" = RCP,
      "climateModel" = climateModelType,
      "ensemble" = ensemble,
      "climateResolution" = climateResolution,
      "climateFilePath" = climateFilePath
      )
  )
  objects <- c(objects, list(
    "birdsList" = c("CAWA", "OSFL"), # [ FIX ] <~~~~~~~~~~~~~~~~~ For paper, just remove this line! 
    "uplandsRaster" = uplandsRaster,
    "climateDataFolder" = getPaths()$inputPath,
    "pixelsWithDataAtInitialization" = tryCatch(readRDS(file.path(Paths$inputPath, 
                                                                  "pixelsWithDataAtInitialization.rds")), 
                                                error = function(e){
warning(paste0("The pixelsWithDataAtInitialization.rds object was not found. Returning NULL.",
"This will affect bird predictions for the NWT (i.e. density will not be predicted",
"for pixels were total biomas = 0). To fix this, save the object named 'sim$activePixelIndex' in the end of Biomass_core's init i.e. add to Biomass_core's Line 661-662 ",
"saveRDS(sim$activePixelIndex, file = file.path(outputPath(sim), 'pixelsWithDataAtInitialization.rds'))"))})))
  
  modules <- list("birdsNWT")
  
  invisible(sapply(X = list.files(file.path("/mnt/data/Micheletti/NWT/modules/birdsNWT/R/"), full.names = TRUE), FUN = source))

# [ FIX ] only because we already ran LandR previously. Needs to be commented out when doing the whole project at once
  if (all(runLandR == FALSE)){
    if (is.null(originalDateAnalysis)) stop("If runLandR == FALSE you need to pass the date for the 
                                            analysis (i.e. where LandR results are)")
    newInputPath <- gsub(x = getPaths()$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                         replacement = originalDateAnalysis)
    newOutputPath <- gsub(x = getPaths()$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                          replacement = originalDateAnalysis)
    setPaths(inputPath = newInputPath,
             outputPath = newOutputPath)
    birdOutPath <- checkPath(file.path(newOutputPath, 
                                       paste0("birdPredictionsV", 
                                              parameters[["birdsNWT"]][["version"]])), create = TRUE)
    setPaths(outputPath = birdOutPath)
  } else {
    birdOutPath <- checkPath(file.path(getPaths()$outputPath, 
                                       paste0("birdPredictionsV", 
                                              parameters[["birdsNWT"]][["version"]])), create = TRUE)
    setPaths(inputPath = getPaths()$outputPath,
             outputPath = birdOutPath)
  }
  
    assign(x = paste0(definedRun$whichRUN, "_birdsV", parameters[["birdsNWT"]][["version"]]), do.call(get(spadesFun), args = list(inputs = inputs,
                                                                                                      times = times,
                                                                                                      params = parameters,
                                                                                                      modules = modules,
                                                                                                      objects = objects,
                                                                                                      paths = getPaths(),
                                                                                                      loadOrder = unlist(modules),
                                                                                                      outputs = outputsLandR)))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ BIRDS MODEL 2
    
    if (length(birdModelVersion) > 1){ # Run a second bird model
      bMod <- birdModelVersion[2]
      parameters[["birdsNWT"]][["version"]] <- bMod
      birdOutPath <- checkPath(file.path(dirname(getPaths()$outputPath), 
                                         paste0("birdPredictionsV", 
                                                parameters[["birdsNWT"]][["version"]])), create = TRUE)
      setPaths(outputPath = birdOutPath)
      assign(x = paste0(definedRun$whichRUN, "_birdsV",
                        parameters[["birdsNWT"]][["version"]]),
             do.call(
               get(spadesFun),
               args = list(
                 inputs = inputs,
                 times = times,
                 params = parameters,
                 modules = modules,
                 objects = objects,
                 paths = getPaths(),
                 loadOrder = unlist(modules),
                 outputs = outputsLandR
               )
             ))
    } # End of second bird model
} # End of run birds

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CARIBOU

if (!exists("runCaribou")) runCaribou <- FALSE # Default if not provided
if (runCaribou){
  message(crayon::white(paste0("Starting simulations for CARIBOUS using ", definedRun$whichRUN, " ", 
                               definedRun$whichReplicate)))
  if (all(runLandR == FALSE, runBirds == FALSE)){
    if (is.null(originalDateAnalysis)) 
stop("If runLandR == FALSE you need to pass the date for the
     analysis (i.e. where LandR results are)")
    newInputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                         replacement = originalDateAnalysis)
    newOutputPath <- gsub(x = paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                          replacement = originalDateAnalysis)
    setPaths(inputPath = newInputPath,
             outputPath = newOutputPath)
  } else {
    if (runBirds == TRUE){ # input path is correct, independently if I ran LandR before birds
      caribouOutPath <- checkPath(file.path(dirname(getPaths()$outputPath), "caribouPredictions"), create = TRUE)
      setPaths(outputPath = caribouOutPath)
    } else { # only if I didn't run birds, only LandR
      caribouOutPath <- checkPath(file.path(dirname(getPaths()$outputPath), "caribouPredictions"), create = TRUE)
      setPaths(inputPath = getPaths()$outputPath,
               outputPath = birdOutPath)
    }
  }
    
  invisible(sapply(X = list.files(file.path("/mnt/data/Micheletti/NWT/modules/caribouRSF/R/"), full.names = TRUE), FUN = source))

  parameters <- list(
    caribouRSF = list(
      "decidousSp" = c("Betu_Pap", "Popu_Tre", "Popu_Bal"),
      "predictionInterval" = 20
    )
  )
  modules <- list("caribouRSF")
  assign(x = paste0(definedRun$whichRUN, "_caribou"), do.call(get(spadesFun), args = list(inputs = inputs,
                                                                                                    times = times,
                                                                                                    params = parameters,
                                                                                                    modules = modules,
                                                                                                    objects = objects,
                                                                                                    paths = getPaths(),
                                                                                                    loadOrder = unlist(modules),
                                                                                                    outputs = outputsLandR, debug = 1)))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

