typeOfSimulation <- "dynamic" # this is the only one now, as bird models are different
modelVersion <- "V6"# "V4" #For birds only
googleFoldersList <- list(
  # dynamic simulation
  dynamic = list(
    LandR.CS_fS = "1My_5RIOMBVyuQ7RhyiZP2Q1sHp4dlLN3",
    LandR.CS_SCFM = "1aJ_jjeaWJx6rhbw3C6z7xknqXuA9emSe",
    LandR_fS = "14yu782g8uR0fJl2sfbAhkUsORliAxgKl",
    LandR_SCFM = "1DlooFy1hYu9yOHiOOiS4uA2RrbnfxRZV")
)
library("SpaDES")
library("future")
library("future.apply")
plan("sequential")
googledrive::drive_auth("tati.micheletti@gmail.com")
library(usefun)
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
           inputPath = file.path(getwd(), "outputs/06DEC19/"),
           outputPath = checkPath(file.path(getwd(), "outputs/06DEC19/birdResults", typeOfSimulation, Run), create = TRUE)
  )
  getPaths() # shows where the 4 relevant paths are
  species <- c("RUBL", "CAWA", "OSFL")
  parameters <- list(
    rastersPosthoc = list(patternsToRetrieveRasters = c("predicted", "Year", "tif"),
                          patternsUsedForGrouping = species,
                          years = c(seq(2011, 2100, by = 30), 2100),
                          species = species, 
                          n = 50,
                          sampleSize = 45,
                          typeOfAnalysis = paste0("birds_", typeOfSimulation),
                          uploadPlots = TRUE,
                          calculateSignificantChanges = TRUE,
                          calculateSummary = TRUE)
  )
  modules <- list("rastersPosthoc")
  objects <- list(dataFolder = list(
    LandR.CS_fS_V6 = file.path(getPaths()$inputPath, "LandR.CS_fS", 
                               Run, paste0("birdPredictions", modelVersion, typeOfSimulation)),
    LandR_fS_V6 = file.path(getPaths()$inputPath, "LandR_fS", 
                            Run, paste0("birdPredictions", modelVersion, typeOfSimulation)),
    LandR.CS_SCFM_V6 = file.path(getPaths()$inputPath, "LandR.CS_SCFM", 
                                 Run, paste0("birdPredictions", modelVersion, typeOfSimulation)),
    LandR_SCFM_V6 = file.path(getPaths()$inputPath, "LandR_SCFM", 
                              Run, paste0("birdPredictions", modelVersion, typeOfSimulation))),
    googleFolders = googleFoldersList[[typeOfSimulation]])
  
  birdResultsV6 <- simInitAndSpades(times = times, params = parameters, modules = modules,
                                    objects = objects, debug = 1)
}
if (doBirds){
  modelVersion <- "V4"# "V6" #For birds only
  setPaths(modulePath = file.path(getwd(), "modules"),
           inputPath = file.path(getwd(), "outputs/06DEC19/"),
           outputPath = checkPath(file.path(getwd(), "outputs/06DEC19/birdResults", typeOfSimulation, Run), create = TRUE)
  )
  getPaths() # shows where the 4 relevant paths are
  species <- c("RUBL", "CAWA", "OSFL")
  parameters <- list(
    rastersPosthoc = list(patternsToRetrieveRasters = c("predicted", "Year", "tif"),
                          patternsUsedForGrouping = species,
                          years = c(seq(2011, 2100, by = 30), 2100),
                          species = species, 
                          n = 50,
                          sampleSize = 45,
                          typeOfAnalysis = paste0("birds_", typeOfSimulation),
                          uploadPlots = TRUE,
                          calculateSignificantChanges = TRUE,
                          calculateSummary = TRUE)
  )
  modules <- list("rastersPosthoc")
  objects <- list(dataFolder = list(
    LandR.CS_fS_V4 = file.path(getPaths()$inputPath, "LandR.CS_fS", 
                               Run, paste0("birdPredictions", modelVersion, typeOfSimulation)),
    LandR_fS_V4 = file.path(getPaths()$inputPath, "LandR_fS", 
                            Run, paste0("birdPredictions", modelVersion, typeOfSimulation)),
    LandR.CS_SCFM_V4 = file.path(getPaths()$inputPath, "LandR.CS_SCFM", 
                                 Run, paste0("birdPredictions", modelVersion, typeOfSimulation)),
    LandR_SCFM_V4 = file.path(getPaths()$inputPath, "LandR_SCFM", 
                              Run, paste0("birdPredictions", modelVersion, typeOfSimulation))),
    googleFolders = googleFoldersList[[typeOfSimulation]])
  
  birdResultsV4 <- simInitAndSpades(times = times, params = parameters, modules = modules,
                                    objects = objects, debug = 1)
}

#  ~~~~~~~~~~~~~~~~~~~~~~~~# FOR CARIBOUS #~~~~~~~~~~~~~~~~~~~~~~~~
if (doCaribou){
  setPaths(modulePath = file.path(getwd(), "modules"),
           inputPath = file.path(getwd(), "outputs/06DEC19/"),
           outputPath = checkPath(file.path(getwd(), "outputs/06DEC19/caribouResults", typeOfSimulation, Run), create = TRUE)
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
                          uploadPlots = TRUE,
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
  boo <- simInitAndSpades(times = times, params = parameters, modules = modules,
                          objects = objectsCaribou, outputs = outputs)
}