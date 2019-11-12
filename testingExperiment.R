library("SpaDES")
library("future")
library("future.apply")
plan("multiprocess")
googledrive::drive_auth("tati.micheletti@gmail.com")
# library("usefun")
devtools::load_all("/mnt/data/Micheletti/usefun/")

if (pemisc::user() %in% c("Tati", "tmichele"))
  setwd("/mnt/data/Micheletti/NWT")

times <- list(start = 1, end = 1)
Run <- "run1"

inputs <- list()
outputs <- data.frame(
  objectName = c("deltaRasters",
                 "significantChanges",
                 "pixelsSummaries",
                 "RSFlikePlot",
                 "averageInTime",
                 "averageComparison"),
  saveTime = times$end)

doCaribou <- TRUE
doBirds <- TRUE

#  ~~~~~~~~~~~~~~~~~~~~~~~~# FOR BIRDS #~~~~~~~~~~~~~~~~~~~~~~~~

if (doBirds){
  setPaths(modulePath = file.path(getwd(), "modules"),
           inputPath = file.path(getwd(), "outputs/23OCT19/"),
           outputPath = checkPath(file.path(getwd(), "outputs/23OCT19/birdResults"), create = TRUE)
  )
  getPaths() # shows where the 4 relevant paths are
  
  species <- c("RUBL", "CAWA", "OSFL", "BBWA", "WEWP")
  parameters <- list(
    rastersPosthoc = list(patternsToRetrieveRasters = c("predicted", "Year", "tif"),
                          patternsUsedForGrouping = species,
                          years = c(seq(2011, 2100, by = 30), 2100),
                          species = species, 
                          n = 50,
                          sampleSize = 45,
                          typeOfAnalysis = "Birds",
                          uploadPlots = FALSE,
                          calculateSignificantChanges = FALSE,
                          calculateSummary = TRUE)
  )
  modules <- list("rastersPosthoc")
  objectsV6 <- list(dataFolder = list(
    LandR.CS_fS_V6 = file.path(getPaths()$inputPath, "LandR.CS_fS", Run, "birdPredictions"),
    LandR_fS_V6 = file.path(getPaths()$inputPath, "LandR_fS", Run, "birdPredictions"),
    LandR.CS_SCFM_V6 = file.path(getPaths()$inputPath, "LandR.CS_SCFM", Run, "birdPredictions"),
    LandR_SCFM_V6 = file.path(getPaths()$inputPath, "LandR_SCFM", Run, "birdPredictions")),
    googleFolders = list(
      LandR.CS_fS = "1ooFQ4IbkVtVL3topEvz_3vAMu-6h-hyB",
      LandR.CS_SCFM = "1Nuo091FmyFnrfLKVvQXjGoTN8fRUfXIJ",
      LandR_fS = "1PmQR0SkNOrfi5fq_a16PTmsYoLw8zSJJ",
      LandR_SCFM = "1A6BslpcPi4D_9bZ_2DC7sgEXz447cuRv"
    )
  )
  objectsV3 <- list(dataFolder = list(
    LandR.CS_fS_V3 = file.path(getPaths()$inputPath, "LandR.CS_fS", Run, "birdPredictionsV3"),
    LandR_fS_V3 = file.path(getPaths()$inputPath, "LandR_fS", Run, "birdPredictionsV3"),
    LandR.CS_SCFM_V3 = file.path(getPaths()$inputPath, "LandR.CS_SCFM", Run, "birdPredictionsV3"),
    LandR_SCFM_V3 = file.path(getPaths()$inputPath, "LandR_SCFM", Run, "birdPredictionsV3")),
    googleFolders = list(
      LandR.CS_fS = "1ooFQ4IbkVtVL3topEvz_3vAMu-6h-hyB",
      LandR.CS_SCFM = "1Nuo091FmyFnrfLKVvQXjGoTN8fRUfXIJ",
      LandR_fS = "1PmQR0SkNOrfi5fq_a16PTmsYoLw8zSJJ",
      LandR_SCFM = "1A6BslpcPi4D_9bZ_2DC7sgEXz447cuRv"
    )
  )
  
  birdV3 <- simInit(times = times, params = parameters, modules = modules,
                    objects = objectsV3, outputs = outputs)
  
  # birdV6 <- simInit(times = times, params = parameters, modules = modules,
  #                   objects = objectsV6, outputs = outputs)
  
  # birdResults <- spades(birdV6, debug = 1)
}

#  ~~~~~~~~~~~~~~~~~~~~~~~~# FOR CARIBOUS #~~~~~~~~~~~~~~~~~~~~~~~~
if (doCaribou){
  setPaths(modulePath = file.path(getwd(), "modules"),
           inputPath = file.path(getwd(), "outputs/23OCT19/"),
           outputPath = checkPath(file.path(getwd(), "outputs/23OCT19/caribouResults"), create = TRUE)
  )
  getPaths() # shows where the 4 relevant paths are
  
  typeOfRas <- c("SelectionTaiga", "SelectionUncertainTaiga")
  parameters <- list(
    rastersPosthoc = list(patternsToRetrieveRasters = c("relativeSelection", "Year", "tif"),
                          patternsUsedForGrouping = typeOfRas,
                          years = c(2011, 2041, 2071, 2100),
                          species = typeOfRas, 
                          n = 50,
                          sampleSize = 45,
                          typeOfAnalysis = "Caribou",
                          uploadPlots = FALSE,
                          calculateSignificantChanges = FALSE,
                          calculateSummary = TRUE,
                          makeRSFLikePlot = TRUE)
  )
  modules <- list("rastersPosthoc")
  objectsCaribou <- list(dataFolder = list(
    LandR.CS_fS = file.path(getPaths()$inputPath, "LandR.CS_fS", Run, "caribouPredictions"),
    LandR_fS = file.path(getPaths()$inputPath, "LandR_fS", Run, "caribouPredictions"),
    LandR.CS_SCFM = file.path(getPaths()$inputPath, "LandR.CS_SCFM", Run, "caribouPredictions"),
    LandR_SCFM = file.path(getPaths()$inputPath, "LandR_SCFM", Run, "caribouPredictions")),
    googleFolders = list(
      LandR.CS_fS = "1ooFQ4IbkVtVL3topEvz_3vAMu-6h-hyB",
      LandR.CS_SCFM = "1Nuo091FmyFnrfLKVvQXjGoTN8fRUfXIJ",
      LandR_fS = "1PmQR0SkNOrfi5fq_a16PTmsYoLw8zSJJ",
      LandR_SCFM = "1A6BslpcPi4D_9bZ_2DC7sgEXz447cuRv"
    )
  )
  
  boo <- simInit(times = times, params = parameters, modules = modules,
                          objects = objectsCaribou, outputs = outputs)
}

library("SpaDES.experiment")

  factorialSimulations <- experiment2(birdsV3 = birdV3,
                                      # birdsV6 = birdV6, # NOT WORKING. NO IDEA WHY.
                                      cariboo = boo,
                                      clearSimEnv = TRUE, 
                                      replicates = 2)
