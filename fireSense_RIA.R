usrEmail <- if (pemisc::user() %in% c("tmichele", "Tati")) "tati.micheletti@gmail.com" else NULL
googledrive::drive_auth(email = usrEmail)

library("usefun")
library("LandR")
library("LandR.CS")
library("SpaDES")
library("raster")
library("plyr"); library("dplyr")
library("amc")
library("magrittr") # for piping
library("future")
library("future.apply")
library("data.table")
plan("multiprocess")

user <- pemisc::user()

if (pemisc::user() %in% c("Tati", "tmichele", "emcintir")) {
  setwd("/mnt/data/Micheletti/NWT/")
}

paths <- list(cachePath = file.path(getwd(), "cache"), 
              inputPath = file.path(getwd(), "inputs"),
              modulePath = file.path(getwd(), "modules"),
              outputPath = checkPath(file.path(getwd(), "cache"), create = TRUE))
if (length(paths$modulePath) == 1) paths$modulePath <- c(paths$modulePath, file.path(paths$modulePath, "scfm/modules"))


if (pemisc::user() %in% c("Tati", "tmichele", "emcintir")) {
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
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = paths$inputPath,
  "map.overwrite" = TRUE,
  "map.tilePath" = tilePath,
  "map.useParallel" = TRUE, #!identical("windows", .Platform$OS.type),
  "reproducible.futurePlan" = FALSE,
  "future.globals.maxSize" = if (pemisc::user("tmichele")) 6000*1024^2 else 1000*1024^2,
  "reproducible.inputPaths" = if (pemisc::user("emcintir")) "~/data" else paths$inputPath,
  "reproducible.quick" = FALSE,
  "reproducible.overwrite" = TRUE,
  "reproducible.useMemoise" = FALSE, # Brings cached stuff to memory during the second run
  "reproducible.useNewDigestAlgorithm" = TRUE,  # use the new less strict hashing algo
  "reproducible.useCache" = TRUE,
  "reproducible.cachePath" = paths$cachePath,
  "reproducible.showSimilar" = FALSE,
  "reproducible.useCloud" = FALSE,
  "spades.moduleCodeChecks" = FALSE, # Turn off all module's code checking
  "spades.useRequire" = FALSE, # assuming all pkgs installed correctly
  "pemisc.useParallel" = TRUE
)

SpaDES.core::setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, 
                      outputPath = paths$outputPath, cachePath = paths$cachePath)

#################################################################################
################################## SIMULATION SET UP ############################
#################################################################################

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

studyArea <- Cache(prepInputs,
                   url = "https://drive.google.com/open?id=18XPcOKeQdty102dYHizKH3ZPE187BiYi",
                   destinationPath = getPaths()$inputPath[[1]],
                   userTags = "RIA_fiveTSA",
                   omitArgs = c("destinationPath"))

rasterToMatch <- LandR::prepInputsLCC(destinationPath = getPaths()$inputPath[[1]],
                                      studyArea = studyArea)

DEM <- Cache(prepInputs,
             url = "https://drive.google.com/open?id=18-oYLhSBQKu_m0oGSenYpYAuUuAccEBb",
             destinationPath = getPaths()$inputPath[[1]],
             userTags = "DEM_RIA",
             omitArgs = c("destinationPath"))

wetLCC <- usefun::prepInputsLayers_DUCKS(destinationPath = paths$inputPath, # Or another directory.
                     rasterToMatch = rasterToMatch,
                     studyArea = studyArea)

studyAreaPSP <- studyArea

sppEquivCol <- "RIA"

# Equivalency table for tree species
data("sppEquivalencies_CA", package = "LandR")
sppEquivalencies_CA[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                             EN_generic_full = "Pine",
                                             Leading = "Pine leading")]

# Make LandWeb spp equivalencies
sppEquivalencies_CA[, RIA := c(Pice_mar = "Pice_mar", Pice_gla = "Pice_gla",
                               Pinu_con = "Pinu_con", Popu_tre = "Popu_tre", 
                               Betu_pap = "Betu_pap", Pice_eng = "Pice_eng")[LandR]]
sppEquivalencies_CA[LANDIS_traits == "ABIE.LAS"]$RIA <- "Abie_las"

sppEquivalencies_CA <- sppEquivalencies_CA[!LANDIS_traits == "PINU.CON.CON"]

sppEquivalencies_CA[RIA == "Abie_las", EN_generic_full := "Subalpine Fir"]
sppEquivalencies_CA[RIA == "Abie_las", EN_generic_short := "Fir"]
sppEquivalencies_CA[RIA == "Abie_las", Leading := "Fir leading"]
sppEquivalencies_CA[RIA == "Popu_tre", Leading := "Pop leading"]
sppEquivalencies_CA[RIA == "Betu_pap", EN_generic_short := "Birch"]
sppEquivalencies_CA[RIA == "Betu_pap",  Leading := "Betula leading"]
sppEquivalencies_CA[RIA == "Betu_pap",  EN_generic_full := "Paper birch"]
sppEquivalencies_CA[RIA == "Pice_eng", EN_generic_full := 'Engelmann Spruce']
sppEquivalencies_CA[RIA == 'Pice_eng', EN_generic_short  := "En Spruce"]
sppEquivalencies_CA[RIA == "Popu_tre", EN_generic_short := "Aspen"]
sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(RIA)]

#Assign colour
sppColors <- RColorBrewer::brewer.pal(name = 'Paired', n = length(unique(sppEquivalencies_CA$RIA)) + 1)

#Test colours
# clearPlot()
plot(x = 1:7, y = 2:8, col = sppColors, pch = 20, cex = 3)

setkey(sppEquivalencies_CA, RIA)
sppNames <- unique(sppEquivalencies_CA$RIA)
names(sppColors) <- c(sppNames, "mixed")
sppColors

sppEquivalencies_CA
objectSynonyms <- list(c('vegMap', "LCC2005"))

times <- list(start = 2011, end = 2015)

#SCFM
defaultInterval <- 1.0
defaultPlotInterval <- NA
defaultInitialSaveTime <- NA 

parameters <- list(
  #SCFM
  # ".progress" = list(type = "text", interval = 1),
  scfmLandcoverInit = list(
    ".plotInitialTime" = NA
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
    ".plotInitialTime" = NA,
    ".saveInitialTime" = NA,
    "seedingAlgorithm" = "wardDispersal",
    ".useCache" = FALSE,
    "initialBiomassSource" = "cohortData",
    "growthAndMortalityDrivers" = "LandR.CS",
    ".useParallel" = 2),
  Boreal_LBMRDataPrep = list(
    # "speciesUpdateFunction" = list(
    #   quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, sim$sppEquiv, P(sim)$sppEquivCol)),
    #   quote(usefun::reviseSpeciesTraits(speciesTable = sim$species)),
    #   quote(usefun::changeTraits(speciesTable = sim$species, param = "seeddistance_max",
    #                              facMult = 0.4, species = c("Betu_Pap", "Popu_Tre")))
    # ),
    "useCloudCacheForStats" = TRUE,
    "sppEquivCol" = sppEquivCol,
    "successionTimestep" = 10,
    "pixelGroupAgeClass" = 10,
    ".useCache" = c(".inputObjects", "init"),
    "subsetDataBiomassModel" = 50),
  Biomass_regeneration = list(
    "fireTimestep" = 1,
    "fireInitialTime" = times$start
  ),
  climate_NWT_DataPrep = list(
    "rcp" = 45, # 45 or 85
    "gcm" = "CanESM2"), # One of CanESM2, GFDL-CM3, HadGEM2-ES, MPI-ESM-LR
  fireSense_IgnitionPredict = list(
    "data" = c("MDC06", "LCC"),
    "modelObjName" = "fireSense_FrequencyFitted"),
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
lastYears <- data.frame(objectName = c("fireRegimeRas", "speciesEcoregion", 
                                       "species", "gcsModel", "mcsModel"),
                        saveTime = times$end)
if (length(grepMulti(x = modules, "LBMR")) != 0){
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
  "rasterToMatch" = rasterToMatch,
  "studyAreaLarge" = studyArea,
  "sppEquiv" = sppEquivalencies_CA,
  "sppEquivCol" = sppEquivCol,
  "sppColorVect" = sppColors,
  "omitNonVegPixels" = TRUE,
  "cloudFolderID" = cloudFolderID,
  "studyArea" = studyArea,
  "fireRegimePolys" = studyArea,
  "DEM" = DEM,
  "wetLCC" = wetLCC
)

data.table::setDTthreads(10) # Data.table has all threads by default, which is inconveninent and unecessary. Will try setting it for only 10 cores.  

modules <- list("Boreal_LBMRDataPrep", "Biomass_regeneration", "LBMR", "PSP_Clean", 
                "gmcsDataPrep", "fireSense_NWT_DataPrep", 
                # "fireSense_IgnitionFit",
                # "fireSense_EscapeFit", 
                "fireSense_IgnitionPredict", "fireSense_EscapePredict", 
                "LBMR2LCC_DataPrep", "fireSense_NWT", "scfmLandcoverInit", 
                "scfmRegime", "scfmDriver", "scfmSpread")

mySim <- simInit(times = times, params = parameters, modules = modules, objects = objects,
                 paths = paths, loadOrder = unlist(modules))

#REVIEW SPECIES
#Edit longevity of some species according to #Burton and Cumming 1995
# mySim$speciesTable[LandisCode == "PICE.GLA"]$Longevity <- 325 #400 Changed it because of random Google answer
# mySim$speciesTable[LandisCode == "PINU.CON.LAT"]$Longevity <- 335
# mySim$speciesTable[LandisCode == "PICE.MAR"]$Longevity <- 250
# mySim$speciesTable[LandisCode == "POPU.TRE"]$Longevity <- 200
# mySim$speciesTable[LandisCode == "ABIE.LAS"]$Longevity <- 250

dev.off()
dev() 
mySimOut <- spades(mySim, debug = TRUE)
