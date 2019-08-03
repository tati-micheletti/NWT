
setwd("/mnt/data/Micheletti/NWT/")
# Load SpaDES
library("SpaDES")
library("raster")
library("usefun")
library("LandR")

options("spades.recoveryMode" = 3)

# Source functions in R folder
invisible(sapply(X = list.files(file.path(getwd(), "R"), full.names = TRUE), FUN = source))
invisible(sapply(X = list.files(file.path("/mnt/data/Micheletti/NWT/modules/birdsNWT/R/"), full.names = TRUE), FUN = source))
invisible(sapply(X = list.files(dirname(getwd()), "functions", full.names = TRUE), FUN = source)) 

setPaths(modulePath = file.path(getwd(), "modules"), cachePath = file.path(getwd(), "cache"),
         inputPath = file.path(getwd(), "outputs/18JUL19/run1/"), 
         outputPath = file.path(getwd(), "outputs/18JUL19/birdPredictionsV3_Fixed"))
getPaths() # shows where the 4 relevant paths are

parameters <- list(
  # .progress = list(type = "text", interval = 1), # for a progress bar
  birdsNWT = list(
    "version" = 3,
    "scenario" = "CS",
    "useStaticPredictionsForNonForest" = TRUE,
    "useOnlyUplandsForPrediction" = TRUE,
    "baseLayer" = 2005,
    "overwritePredictions" = FALSE,
    "useTestSpeciesLayers" = FALSE, # Set it to false when you actually have results from LandR_Biomass simulations to run it with
    "useParallel" = TRUE, # Using parallel in windows is currently not working.
    "predictionInterval" = 20,
    "quickLoad" = FALSE
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

# Each species takes around 20Gb of RAM to run, and about 12hs. Keep that in mind when running on smaller computers.
spG0 <- c("AMRE", "BLPW", "CAWA", "FOSP", "BBWA", "BOCH",  
            "OSFL", "OVEN", "PAWA", "RCKI", "RUBL", "WCSP", "CMWA")

spG1 <- c("ALFL", "AMCR", "AMGO", "AMRO", "ATSP", "BAOR", "BAWW",  
          "BBWO", "BCCH", "BHCO", "BHVI", "BLBW", "BLJA", "BRBL", 
          "BRCR", "BRTH", "BTNW", "CCSP", "CEDW", "CHSP", "COGR",
          "YEWA")

spG2 <- c("HOLA", "HOWR", "LEFL", "LISP", "MAWA", "MOWA", "NOFL", "NOWA", 
          "OCWA", "PHVI", "PIGR", "PISI", "PIWO", "PUFI", "RBGR", "RBNU", 
          "REVI", "RWBL", "SEWR", "SOSP", "TOWA", "TRES", "VATH", "VEER",
          "YRWA")

spG3 <- c("CORA", "CORE", "COYE", "CSWA", "DEJU", "EAKI", "EAPH", "EVGR", 
          "GCKI", "GCTH", "GRAJ", "GRCA", "HAFL", "VESP", "WAVI", "WBNU", 
          "WETA", "WEWP", "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA")

birdSpeciesList <- list(G0 = spG0, G1 = spG1, G2 = spG2, G3 = spG3) #  # Already completed

env <- environment()
# ys <- c()
# lapply(X = ys, function(YEAR) {
  lapply(names(birdSpeciesList), function(groupIndex) {
  times <- list(start = 2001, end = 2100)
  birdSpecies <- birdSpeciesList[[groupIndex]]

  sppEquivCol <- "NWT"
  data("sppEquivalencies_CA", package = "LandR")
  sppEquivalencies_CA[, NWT := c(Betu_Pap = "Betu_Pap", 
                                 Lari_Lar = "Lari_Lar", 
                                 Pice_Gla = "Pice_Gla",
                                 Pice_Mar = "Pice_Mar", 
                                 Pinu_Ban = "Pinu_Ban", 
                                 Popu_Tre = "Popu_Tre")[Boreal]]
  
  sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(NWT)]
  sppEquivalencies_CA$EN_generic_short <- sppEquivalencies_CA$NWT
  # Fix EN_generic_short for plotting. Needs to have all names. Don't have time now.
  
  sppColorVect <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, sppEquivCol = sppEquivCol,
                                   palette = "Set3")
  mixed <- structure("#D0FB84", names = "Mixed")
  sppColorVect[length(sppColorVect)+1] <- mixed
  attributes(sppColorVect)$names[length(sppColorVect)] <- "Mixed"
  
  
  .objects <- list(
    "birdsList" = birdSpecies,
    "uplandsRaster" = uplandsRaster,
    "rasterToMatch" = rasterToMatch,
    "studyArea" = studyArea,
    "sppEquiv" = sppEquivalencies_CA,
    "sppEquivCol" = sppEquivCol,
    "urlModels" = "https://drive.google.com/open?id=19Ys5vHj6L_jyfrZdbUb6qpKyEfDfosQ9", # Folder where models are
    "urlStaticLayers" = "https://drive.google.com/open?id=1DsuIAt1eEkd1jXqSNCmlhs-DGoCa3KEY") # Link to static layers used
  
  modules <- list("birdsNWT")
  inputs <- list()
  outputs <- list()
  runName <- paste0(groupIndex, "_BIRDS_CS_fS")

  assign(x = runName, value = simInitAndSpades(times = times,
                                               params = parameters, 
                                               modules = modules, 
                                               objects = .objects,
                                               debug = 1), envir = env)
  saveRDS(object = get(runName),
          file = file.path(SpaDES.core::getPaths()$outputPath, 
                           paste0(runName,
                                  toupper(format(Sys.time(), "%d%b%y")))))
  rm(runName, envir = env)
  })
# })