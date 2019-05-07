# Quick script for whole NWT analysis (LandR + scfm-fireSense + Caribou)

setwd("/mnt/data/Micheletti/NWT")
library("raster")
library("SpaDES")
SpaDES.core::setPaths(modulePath = c(file.path(getwd(), "modules"),
                                     file.path(getwd(), "modules/scfm/modules")),
                      inputPath = file.path(getwd(), "inputs"), 
                      outputPath = checkPath(file.path(getwd(), "outputs", 
                                                       toupper(format(Sys.time(), "%d%b%y"))), 
                                             create = TRUE), 
                      cachePath = file.path(getwd(), "cache"))
maxMemory <- 5e+12
scratchDir <- checkPath(path = paste0("/mnt/tmp/rasterTMP/", user), create = TRUE)
if(dir.create(scratchDir)) system(paste0("sudo chmod -R 777 /mnt/tmp/rasterTMP"), wait = TRUE) 
rasterOptions(default = TRUE)
options(rasterMaxMemory = maxMemory, rasterTmpDir = scratchDir)
cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"
tilePath <- file.path(getPaths()$outputPath, "tiles")
.plotInitialTime <- NA
opts <- options(
  "future.globals.maxSize" = 1000*1024^2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = getPaths()$inputPath,
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
  "map.useParallel" = TRUE, #!identical("windows", .Platform$OS.type),
  "reproducible.futurePlan" = FALSE,
  "reproducible.inputPaths" = if (pemisc::user("emcintir")) "~/data" else NULL,
  "reproducible.quick" = FALSE,
  "reproducible.overwrite" = TRUE,
  "reproducible.useMemoise" = TRUE, # Brings cached stuff to memory during the second run
  "reproducible.useNewDigestAlgorithm" = TRUE,  # use the new less strict hashing algo
  "reproducible.useCache" = TRUE,
  "reproducible.cachePath" = getPaths()$cachePath,
  "reproducible.showSimilar" = FALSE,
  "reproducible.useCloud" = if (pemisc::user("emcintir")|pemisc::user("tmichele")) FALSE else TRUE,
  "spades.moduleCodeChecks" = FALSE, # Turn off all module's code checking
  "spades.useRequire" = FALSE, # assuming all pkgs installed correctly
  "pemisc.useParallel" = TRUE
)
studyArea <- Cache(prepInputs, # BCR6/NWT
                        url = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU",
                        destinationPath = getPaths()$inputPath[[1]],
                        omitArgs = c("destinationPath"))
modulesLandR_SCFM <- list("scfmLandcoverInit", "scfmRegime", "scfmDriver", "scfmIgnition", "scfmEscape", "scfmSpread",
                          "Boreal_LBMRDataPrep", "LandR_BiomassGMOrig","Biomass_regeneration", "LBMR")
times <- list(start = 0, end = 100)
rasterToMatch <- Cache(prepInputs, 
                            url = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df", 
                            studyArea = studyArea,
                            targetFile = "RTM.tif", destinationPath = getPaths()$inputPath, 
                            overwrite = TRUE, filename2 = NULL,
                            omitArgs = c("destinationPath", "cloudFolderID", "useCloud", "overwrite", "filename2"))

waterRaster <- prepInputs(url = "https://drive.google.com/open?id=1nPd03gaVXkkaHorirR4UhYrDi95ZgyJM",
                          targetFile = "waterRasterNW.tif", studyArea = studyArea, 
                          rasterToMatch = rasterToMatch,
                          destinationPath = getPaths()$inputPath, 
                          filename2 = "waterRasterNW.tif")
sppEquivCol <- "NWT"
defaultInterval <- NA
defaultPlotInterval <- NA
defaultInitialSaveTime <- NA 
data("sppEquivalencies_CA", package = "LandR")
sppEquivalencies_CA[, NWT := c(Abie_Bal = "Abie_Bal", 
                               Betu_Pap = "Betu_Pap", 
                               Lari_Lar = "Lari_Lar", 
                               Pice_Gla = "Pice_Gla",
                               Pice_Mar = "Pice_Mar", 
                               Pinu_Ban = "Pinu_Ban", 
                               Pinu_Con = "Pinu_Con", 
                               Popu_Bal = "Popu_Bal", 
                               Popu_Tre = "Popu_Tre")[Boreal]]

sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(NWT)]
sppEquivalencies_CA$EN_generic_short <- sppEquivalencies_CA$NWT
sppColors <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, sppEquivCol = sppEquivCol,
                              palette = "Set3")
mixed <- structure("#D0FB84", names = "Mixed")
sppColors[length(sppColors)+1] <- mixed
attributes(sppColors)$names[length(sppColors)] <- "Mixed"

library("googledrive")

tryCatch(googledrive::drive_download(file = googledrive::as_id("1EetOiGxAq-QTZCVU9Q6Y3I26tqjZm3Oi"), 
                                     path = file.path(getPaths()$inputPath, "fireSense_FrequencyFitted.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})
tryCatch(googledrive::drive_download(file = googledrive::as_id("11CrK9PfNJzkU5cZg9-JYYzyr-VP23W0x"), 
                                     path = file.path(getPaths()$inputPath, "fireSense_EscapeFitted.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})

inputs <- data.frame(
  files = c(file.path(getPaths()$inputPath, "fireSense_FrequencyFitted.rds"), 
            file.path(getPaths()$inputPath, "fireSense_EscapeFitted.rds")),
  functions = c("base::readRDS", "base::readRDS"),
  stringsAsFactors = FALSE
)

modulesLandR_fS_SCFM_Carib <- c(
  "Boreal_LBMRDataPrep",
  "LandR_BiomassGMOrig",
  "Biomass_regeneration",
  "LBMR",
  "climate_NWT_DataPrep",
  "MDC_NWT_DataPrep",
  "fireSense_NWT_DataPrep",
  "fireSense_FrequencyPredict",
  "fireSense_EscapePredict",
  "LBMR2LCC_DataPrep",
  "fireSense_NWT",
  "scfmLandcoverInit",
  "scfmRegime",
  "scfmDriver",
  "scfmSpread",
  "caribouPopGrowthModel"
)

#SCFM
defaultInterval <- 1.0
defaultPlotInterval <- NA
defaultInitialSaveTime <- NA 

parameters <- list(
  #SCFM
  ".progress" = list(type = "text", interval = 1),
  scfmLandcoverInit = list(
    ".plotInitialTime" = NULL
  ),
  scfmSpread = list(
    "pSpread" = 0.235,
    "returnInterval" = defaultInterval,
    "startTime" = times$start,
    ".plotInitialTime" = times$start,
    ".plotInterval" = defaultPlotInterval,
    ".saveInitialTime" = defaultInitialSaveTime,
    ".saveInterval" = defaultInterval),
  scfmRegime = list(fireCause = "L"), #c("L", "H")
  scfmDriver = list(targetN = 1000), # 1500
  # LandR_Biomass
  LBMR = list(
    ".useParallel" = 3,
    ".plotInitialTime" = NA,
    ".saveInitialTime" = NA,
    "seedingAlgorithm" = "wardDispersal",
    ".useCache" = FALSE,
    "successionTimestep" = 10,
    "initialBiomassSource" = "cohortData"),
  LandR_BiomassGMOrig = list( # dev branch
    "growthInitialTime" = 0,
    "successionTimestep" = 10,
    "growthAndMortalityDrivers" = "LandR"),
  Boreal_LBMRDataPrep = list(
    "useCloudCacheForStats" = if (pemisc::user("tmichele")) FALSE else TRUE,
    "sppEquivCol" = sppEquivCol,
    "successionTimestep" = 10,
    "pixelGroupAgeClass" = 10,
    ".useCache" = c(".inputObjects", "init")),
  climate_NWT_DataPrep = list(
    "rcp" = 45, # 45 or 85
    "gcm" = "CanESM2"), # One of CanESM2, GFDL-CM3, HadGEM2-ES, MPI-ESM-LR
  fireSense_FrequencyPredict = list(
    "f" = 250 / 10000, # fireSense_FrequencyFit was fitted using the 10km resolution, predictions are made at the 250m resolution?
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
    ".growthInterval" = 1)
)
.objects <- list(
  "rasterToMatch" = rasterToMatch,
  "studyAreaLarge" = studyArea,
  "sppEquiv" = sppEquivalencies_CA,
  "sppEquivCol" = sppEquivCol,
  "sppColors" = sppColors,
  "omitNonVegPixels" = TRUE,
  "cloudFolderID" = cloudFolderID,
  "studyArea" = studyArea,
  "waterRaster" = waterRaster
)
succTS <- seq(times$start, times$end, 
              by = parameters$LBMR$successionTimestep)
outputsLandR <- data.frame(
  objectName = rep(c("burnMap",
                     "cohortData",
                     "simulationTreeOutput",
                     "summaryBySpecies",
                     "summaryBySpecies1",
                     "simulationOutput",
                     "pixelGroupMap",
                     "simulatedBiomassMap",
                     "ANPPMap",
                     "mortalityMap"), each = length(succTS)),
  saveTime = c(rep(succTS, times = 10))
)

LandR_fS_SCFM_Caribou <- simInitAndSpades(times = times, params = parameters,
                                          inputs = inputs,
                                          modules = modulesLandR_fS_SCFM_Carib,
                                          objects = .objects, paths = getPaths(),
                                          loadOrder = unlist(modulesLandR_fS_SCFM_Carib),
                                          outputs = outputsLandR, debug = 2)

exist <- tryCatch(exists(a), error = function(e) return(FALSE))
if (exist)
  saveRDS(object = get(paste0("LandR_SCFM_", runVersion)),
          file = file.path(getPaths()$outputPath, paste0("LandRfSCaribou_",
                                                    toupper(format(Sys.time(), "%d%b%y")))))