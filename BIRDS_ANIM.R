
setwd("/mnt/data/Micheletti/NWT/")
# Load SpaDES
library("SpaDES")
library("raster")

options("spades.recoveryMode" = 1)

# Source functions in R folder
invisible(sapply(X = list.files(file.path(getwd(), "R"), full.names = TRUE), FUN = source))
invisible(sapply(X = list.files(file.path("/mnt/data/Micheletti/NWT/modules/birdsNWT/R/"), full.names = TRUE), FUN = source))
invisible(sapply(X = list.files(dirname(getwd()), "functions", full.names = TRUE), FUN = source)) 

setPaths(modulePath = file.path(getwd(), "modules"), cachePath = file.path(getwd(), "cache"),
         inputPath = file.path(getwd(), "outputs/18JUN19_CS_SCFM/"), 
         outputPath = file.path(getwd(), "outputs/18JUN19_CS_SCFM/birdPredictions"))
getPaths() # shows where the 4 relevant paths are

parameters <- list(
  # .progress = list(type = "text", interval = 1), # for a progress bar
  birdsNWT = list(
    "scenario" = "CS",
    "useStaticPredictionsForNonForest" = TRUE,
    "useOnlyUplandsForPrediction" = TRUE,
    "baseLayer" = 2005,
    "overwritePredictions" = FALSE,
    "useTestSpeciesLayers" = FALSE, # Set it to false when you actually have results from LandR_Biomass simulations to run it with
    "useParallel" = TRUE, # Using parallel in windows is currently not working.
    "predictionInterval" = 10,
    "quickLoad" = TRUE
  )
)

isEDHZHIE <- FALSE
if (isEDHZHIE){
  url.sA <- "https://drive.google.com/open?id=15n9BOtswKCJ81-us1u8Dbs0WT9f8bagq"
  # EDE.url <- "https://drive.google.com/open?id=1fYvNPwovjNtTABoGcegrvdFGkNfCUsxf" ???? Which Edezhie should I use?
} else {
  url.sA <- "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"
}

studyArea <- Cache(prepInputs,
                   url = url.sA,
                   destinationPath = getPaths()$inputPath[[1]],
                   userTags = "studyArea", filename2 = NULL,
                   omitArgs = c("destinationPath"))

rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df",
                       studyArea = studyArea,
                       targetFile = "RTM.tif", destinationPath = getPaths()$inputPath[[1]],
                       filename2 = NULL,
                       userTags = "RTM",
                       omitArgs = c("destinationPath", "filename2"))

# Passing the uplandsRaster here makes sure that all computers can use it as the operations take up a lot of memory
uplandsRaster <- prepInputs(targetFile = "uplandsNWT250m.tif", studyArea = studyArea, rasterToMatch = rasterToMatch,
                            url = "https://drive.google.com/open?id=1EF67NCH7HqN6QZ0KGlpntB_Zcquu6NJe", 
                            destinationPath = getPaths()$inputPath, filename2 = NULL)

# Check the list of species available:
showAvailableBirdSpecies()

# This is the original, do based on the models that are already in the folder. not ideal.
# birdSpecies <- list.files(path = file.path(getwd(), "modules/birdsNWT/data/models"), pattern = "brt2.R")
# birdSpecies <- unlist(strsplit(birdSpecies, split = "brt2.R"))

# Each species takes around 20Gb of RAM to run, and about 12hs. Keep that in mind when running on smaller computers.

    times <- list(start = 2011, end = 2100)
    birdSpecies <- c("CAWA")

    sppEquivCol <- "NWT"
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
    
    .objects <- list(
      "birdsList" = birdSpecies,
      "uplandsRaster" = uplandsRaster,
      "rasterToMatch" = rasterToMatch,
      "studyArea" = studyArea,
      "sppEquiv" = sppEquivalencies_CA,
      "sppEquivCol" = sppEquivCol)
    
    modules <- list("birdsNWT")
    inputs <- list()
    outputs <- list()
    runName <- "CAWA_CS_SCFM"
    
    assign(x = runName, value = simInitAndSpades(times = times,
                                                 params = parameters, 
                                                 modules = modules, 
                                                 objects = .objects,
                                                 debug = 2), envir = env)
    saveRDS(object = get(runName),
            file = file.path(SpaDES.core::getPaths()$outputPath,
                             paste0(runName,
                                    toupper(format(Sys.time(), "%d%b%y")))))
