setwd("/mnt/data/Micheletti/NWT")

updateCRAN <- FALSE
updateGithubPackages <- FALSE
updateSubmodules <- FALSE

if (updateGithubPackages){
  devtools::install_github("PredictiveEcology/reproducible@development")
  devtools::install_github("achubaty/amc@development")
  devtools::install_github("PredictiveEcology/pemisc@development")
  devtools::install_github("PredictiveEcology/map@development")
  devtools::install_github("PredictiveEcology/LandR@development") # Updates SpaDES.tools and SpaDES.core quickPlot
  devtools::install_github("ianmseddy/LandR.CS@master") # Climate sensitivity in LandR
}

# library("LandR")
devtools::load_all("/mnt/data/Micheletti/LandR/") # Updated from upstream@development on 18JUN19 18hs
library("LandR.CS")
library("SpaDES")
library("raster")
library("plyr"); library("dplyr")
library("magrittr") # for piping

if (updateSubmodules){
  system(paste0("cd ", getwd(),
                " && git submodule foreach git pull"), wait = TRUE)
  system(paste0("cd ", getwd(),
                " && git pull"), wait = TRUE)
  system("git submodule", wait = TRUE) # checks if the branches and commits you are using are the correct ones
}
if (updateCRAN)
  update.packages(checkBuilt = TRUE)

# Source all common functions
invisible(sapply(X = list.files(file.path(getwd(), "functions"), full.names = TRUE), FUN = source))

user <- pemisc::user()
whichComputer <- if (user == "emcintir") "LocalMachine" else if (user == "tmichele") "BorealCloud" else "LocalMachine"

if (whichComputer == "BorealCloud" & basename(getwd()) != "NWT"){
  setwd(file.path(getwd(), "NWT"))
}

isTest <- FALSE

paths <- pathsSetup(whichComputer = whichComputer, isTest = isTest)
paths$outputPath <- "/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/"
if (length(paths$modulePath) == 1) paths$modulePath <- c(paths$modulePath, file.path(paths$modulePath, "scfm/modules"))

if (pemisc::user() %in% c("Tati", "tmichele")) {
  setTempFolder(paths = paths, setTmpFolder = TRUE, usr = user)
}
maxMemory <- 5e+12
scratchDir <- checkPath(path = paste0("/mnt/tmp/rasterTMP/", user), create = TRUE)
#Here we check that the creation of the folder worked (we might have problems with writting access, only tested with my own user)
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
  "future.globals.maxSize" = 1000*1024^2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = paths$inputPath,
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
  "map.useParallel" = TRUE, #!identical("windows", .Platform$OS.type),
  "reproducible.futurePlan" = FALSE,
  "reproducible.inputPaths" = if (pemisc::user("emcintir")) "~/data" else paths$inputPath,
  "reproducible.quick" = FALSE,
  "reproducible.overwrite" = TRUE,
  "reproducible.useMemoise" = TRUE, # Brings cached stuff to memory during the second run
  "reproducible.useNewDigestAlgorithm" = TRUE,  # use the new less strict hashing algo
  "reproducible.useCache" = TRUE,
  "reproducible.cachePath" = paths$cachePath,
  "reproducible.showSimilar" = FALSE,
  "reproducible.useCloud" = if (pemisc::user("emcintir")|pemisc::user("tmichele")) FALSE else TRUE,
  "spades.moduleCodeChecks" = FALSE, # Turn off all module's code checking
  "spades.useRequire" = FALSE, # assuming all pkgs installed correctly
  "pemisc.useParallel" = TRUE
)

SpaDES.core::setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, 
                      outputPath = paths$outputPath, cachePath = paths$cachePath)

if (quickPlot::isRstudioServer()) options(httr_oob_default = TRUE)

#################################################################################
################################## SIMULATION SET UP ############################
#################################################################################

# NWT.url <- "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"
# EDE.url <- "https://drive.google.com/open?id=1fYvNPwovjNtTABoGcegrvdFGkNfCUsxf" # Official Edehzhie
EDE.url <- "https://drive.google.com/open?id=1klq0nhtFJZv47iZVG8_NwcVebbimP8yT" # Elifant Edehzhie
# smallArea.url <- "https://drive.google.com/open?id=1SUvMX4A5RJ057XYa2W_feX6P1L9As4g8"

# Small study area
studyArea <- Cache(prepInputs, url = EDE.url, 
                   destinationPath = tempdir())
originalCRS <- crs(studyArea)
dtSA <- data.frame(studyArea@data)
# studyArea <- spTransform(studyArea, CRS("+init=epsg:3347"))  # gBuffer needs planas coords
studyArea <- raster::buffer(x = studyArea, width = 30000)

#~~~~~~~~~~~~~~~~~~~~~~~
# Fix due to buffer converting SpatialPolygonDataframe into SpatialPolygon
# Create a dataframe and display default rownames
p.df <- data.frame( ID=1:length(studyArea))
# Extract polygon ID's
pid <- sapply(slot(studyArea, "polygons"), function(x) slot(x, "ID"))
# Create dataframe with correct rownames
p.df <- data.frame( ID=1:length(studyArea), row.names = pid)
# Try coersion again and check class
studyArea <- sp::SpatialPolygonsDataFrame(studyArea, p.df)
studyArea@data <- cbind(studyArea@data, dtSA)
#~~~~~~~~~~~~~~~~~~~~~~~
studyArea <- projectInputs(x = studyArea, targetCRS = "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")  # We have to give it LCC05 crs or one that defined the unit

rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df",
                       studyArea = studyArea,
                       targetFile = "RTM.tif", destinationPath = getPaths()$inputPath[[1]],
                       filename2 = NULL,
                       useSAcrs = TRUE,
                       userTags = "edeRTM",
                       omitArgs = c("destinationPath", "filename2"))
rasterToMatch2 <- rasterToMatch
res(rasterToMatch2) <- c(100, 100)
rasterToMatch <- raster::resample(rasterToMatch, rasterToMatch2)

studyAreaPSP <- prepInputs(targetFile = "BC_Alberta.shp",
                           archive = "studyAreaPSP.zip",
                           url = "https://drive.google.com/open?id=19CkdAT0oaqHenyVK84-BjG0dT3JMarV0",
                           alsoExtract = "similar",
                           destinationPath = getPaths()$inputPath, userTags = "objectName:studyAreaPSP")

waterRaster <- prepInputs(url = "https://drive.google.com/open?id=1nPd03gaVXkkaHorirR4UhYrDi95ZgyJM", 
                          destinationPath = getPaths()$inputPath, 
                          targetFile = "waterRasterNWT.tif",studyArea = studyArea,
                          rasterToMatch = rasterToMatch,
                          filename2 = NULL)

sppEquivCol <- "NWT"

# Equivalency table for tree species
data("sppEquivalencies_CA", package = "LandR")

# Make NWT spp equivalencies
# Popu_Tri == Popu_Bal in NWT
# Quer_mac in LandR needs to be Quer_Mac in NWT
sppEquivalencies_CA[, NWT := c(Abie_Bal = "Abie_Bal", 
                               Betu_Pap = "Betu_Pap", 
                               Lari_Lar = "Lari_Lar", 
                               Pice_Gla = "Pice_Gla",
                               Pice_Mar = "Pice_Mar", 
                               Pinu_Ban = "Pinu_Ban", 
                               # Pinu_Con = "Pinu_Con", 
                               # Popu_Bal = "Popu_Bal", 
                               Popu_Tre = "Popu_Tre")[Boreal]]

sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(NWT)]
sppEquivalencies_CA$EN_generic_short <- sppEquivalencies_CA$NWT
# Fix EN_generic_short for plotting. Needs to have all names. Don't have time now.

sppColorVect <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, sppEquivCol = sppEquivCol,
                                 palette = "Set3")
mixed <- structure("#D0FB84", names = "Mixed")
sppColorVect[length(sppColorVect)+1] <- mixed
attributes(sppColorVect)$names[length(sppColorVect)] <- "Mixed"

modules <- c(
  # LandR
  "Boreal_LBMRDataPrep",
  "Biomass_regeneration",
  "LBMR",
  #SCFM
  "scfmLandcoverInit",
  "scfmRegime",
  "scfmDriver",
  "scfmIgnition", 
  "scfmEscape",
  "scfmSpread",
  #LandR.CS
  "PSP_Clean",
  "gmcsDataPrep",
  #Caribou
  "caribouPopGrowthModel"
)
times <- list(start = 2001, end = 2100)

#SCFM
defaultInterval <- 1.0
defaultPlotInterval <- 10
defaultPlotInitalTime <- times$start
defaultInitialSaveTime <- NA 

parameters <- list(
  #SCFM
  # ".progress" = list(type = "text", interval = 1),
  scfmLandcoverInit = list(
    ".plotInitialTime" = NA
  ),
  scfmIgnition = list(
    "pIgnition" = 0.0001,
    "returnInterval" = defaultInterval,
    "startTime" = times$start,
    ".plotInitialTime" = NA,
    ".plotInterval" = defaultPlotInterval,
    ".saveInitialTime" = defaultInitialSaveTime,
    ".saveInterval" = defaultInterval),
  scfmEscape = list(
    "p0" = 0.05,
    "returnInterval" = defaultInterval,
    "startTime" = times$start,
    ".plotInitialTime" = NA,
    ".plotInterval" = defaultPlotInterval,
    ".saveInitialTime" = defaultInitialSaveTime,
    ".saveInterval" = defaultInterval
  ),
  scfmSpread = list(
    "pSpread" = 0.235,
    "returnInterval" = defaultInterval,
    "startTime" = times$start,
    ".plotInitialTime" = NA,
    ".plotInterval" = defaultPlotInterval,
    ".saveInitialTime" = defaultInitialSaveTime,
    ".saveInterval" = defaultInterval),
  scfmRegime = list(fireCause = "L"), #c("L", "H")
  scfmDriver = list(targetN = 1000), # 1500
  # LandR_Biomass
  LBMR = list(
    "successionTimestep" = 10,
    ".plotInitialTime" = defaultPlotInitalTime,
    ".plotInterval" = defaultPlotInterval,
    ".saveInitialTime" = NA,
    "seedingAlgorithm" = "wardDispersal",
    ".useCache" = FALSE,
    "initialBiomassSource" = "cohortData",
    "growthAndMortalityDrivers" = "LandR.CS"),
  Boreal_LBMRDataPrep = list(
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
      quote(setSeedDist(speciesTable = sim$species, 
                        param = "seeddistance_max", 
                        facMult = 0.4, 
                        species = c("Betu_Pap", "Popu_Tre")))
    ),
    "useCloudCacheForStats" = if (pemisc::user("tmichele")) FALSE else TRUE,
    "sppEquivCol" = sppEquivCol,
    "successionTimestep" = 10,
    "pixelGroupAgeClass" = 10,
    ".useCache" = c(".inputObjects", "init"),
    "subsetDataBiomassModel" = 50
  ),
  Biomass_regeneration = list(
    "fireInitialTime" = times$start,
    "fireTimestep" = 1
  ),
  climate_NWT_DataPrep = list(
    "rcp" = 45, # 45 or 85
    "gcm" = "CanESM2"), # One of CanESM2, GFDL-CM3, HadGEM2-ES, MPI-ESM-LR
  fireSense_FrequencyPredict = list(
    "rescalFactor" = (250 / 10000)^2, # fireSense_FrequencyFit was fitted using the 10km resolution, predictions are made at the 250m resolution
    "data" = c("MDC06", "LCC")),
  fireSense_EscapePredict = list(
    "data" = c("MDC06", "LCC")),
  fireSense_NWT_DataPrep = list(
    "train" = FALSE),
  # Caribou Population Growth
  caribouPopGrowthModel = list(
    ".plotInitialTime" = NULL,
    "recoveryTime" = 40,
    ".useDummyData" = FALSE,
    ".growthInterval" = 5)
)

succTS <- c(seq(times$start, times$end, 
                by = parameters$LBMR$successionTimestep), times$end)
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
caribLambda <- data.frame(objectName = "predictedCaribou",
                          saveTime = times$end)
rasBurn <- data.frame(objectName = rep("burnDT", 
                                       times = length(times$end-times$start)),
                      saveTime = times$start:times$end
)
outputsLandR <- rbind(outputsLandR, rasBurn, caribLambda)

.objects <- list(
  "studyAreaPSP" = studyAreaPSP,
  "rasterToMatch" = rasterToMatch,
  "studyAreaLarge" = studyArea,
  "sppEquiv" = sppEquivalencies_CA,
  "sppEquivCol" = sppEquivCol,
  "sppColorVect" = sppColorVect,
  "omitNonVegPixels" = TRUE,
  "cloudFolderID" = cloudFolderID,
  "studyArea" = studyArea,
  "waterRaster" = waterRaster
)
paths$outputPath <- "/mnt/data/Micheletti/NWT/outputs/04JUL19_SCFM"
data.table::setDTthreads(10) # Data.table has all threads by default, which is inconveninent and unecessary. Will try setting it for only 10 cores.  
t1 <- Sys.time()
Edehzhie_CS_SCFM <- simInitAndSpades(times = times,
                                params = parameters,
                                modules = modules,
                                objects = .objects, paths = paths,
                                loadOrder = unlist(modules),
                                outputs = outputsLandR, debug = 1)
t2 <- Sys.time()

saveRDS(object = Edehzhie_CS_SCFM,
        file = file.path(paths$outputPath, paste0("Edehzhie_CS_SCFM_", toupper(format(Sys.time(), "%d%b%y")))))
