library("SpaDES")
library("raster")
library ("rgdal")
library ("sp") 

setwd("/mnt/data/Micheletti/NWT/")
# Source functions in R folder
invisible(sapply(X = list.files(file.path(getwd(), "R"), full.names = TRUE), FUN = source))
invisible(sapply(X = list.files(file.path(getwd(), "functions"), full.names = TRUE), FUN = source))

# Set a storage project folder
setPaths(modulePath = file.path(getwd(), "modules"), cachePath = file.path(getwd(), "cache"),
         inputPath = file.path(getwd(), "outputs/18JUL19/birdPredictionsV3_Fixed"), 
         outputPath = file.path(getwd(), "outputs/30JUL19/comMetrics"))
getPaths() # shows where the 4 relevant paths are

times <- list(start = 2100, end = 2100)

parameters <- list(
  commu_metricsNWT = list(
    "frequency" = 20
  )
  # .progress = list(type = "text", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  # birdsNWT = list(
  #     "baseLayer" = 2005,
  #     "overwritePredictions" = TRUE,
  #     "useTestSpeciesLayers" = TRUE, # Set it to false when you actually have results from LandR_Biomass simulations to run it with
  #     "useParallel" = FALSE, # Using parallel in windows is currently not working.
  #     "predictionInterval" = 1
  #   )
)

# birdPrediction is a named list
lastY <- if (times$end != times$start) times$end else NULL
succTS <- c(seq(times$start, times$end, 
                by = parameters$commu_metricsNWT$frequency), lastY)

birdPrediction <- lapply(succTS, FUN = function(y){
  birdPrediction <- bringObjectTS(path = getPaths()$inputPath, rastersNamePattern = c("predicted", y))
  nms <- unlist(lapply(birdPrediction, function(ras){
    nm <- ras@data@names
    nm <- usefun::substrBoth(strng = strsplit(nm, split = "Year")[[1]][1], howManyCharacters = 4, fromEnd = TRUE)
    return(nm)
  })
  )
  names(birdPrediction) <- nms
  return(birdPrediction)
})
names(birdPrediction) <- paste0("Year", seq(times$start, times$end, by = 20))

outputsCommMet <- data.frame(objectName = rep(c("currentDiversityRasters","diversityByPolygon", 
                                                "diversityStatistics"), 
                                              each = length(succTS)),
                             saveTime = rep(succTS, 
                                            times = 3))
.objects <- list(
  "birdsList" = names(birdPrediction),
  "birdPrediction" = birdPrediction)
modules <- list("comm_metricsNWT")
inputs <- list()
outputs <- list()
t1 <- Sys.time()
comm_metricsNWT <- simInitAndSpades(times = times, params = parameters, modules = modules,
                                    objects = .objects, loadOrder = c("birdsNWT","comm_metricsNWT"), 
                                    outputs = outputsCommMet, debug = 2)
t2 <- Sys.time()