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
         inputPath = file.path(getwd(), "outputs/18JUN19_CS_SCFM/birdPredictions"), 
         outputPath = file.path(getwd(), "outputs/18JUN19_CS_SCFM/comMetrics"))
getPaths() # shows where the 4 relevant paths are

times <- list(start = 2100, end = 2100)

parameters <- list(
  commu_metricsNWT = list(
    "frequency" = 10
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
birdPrediction <- bringObjectTS(path = getPaths()$inputPath, rastersNamePattern = c("predicted", times$start))
nms <- unlist(lapply(birdPrediction, function(ras){
  nm <- ras@data@names
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  nm <- substrRight(x = unlist(strsplit(x = nm, split = paste0("Year", times$start))), n = 4)
})
)
tstep <- if (!is.null(parameters$commu_metricsNWT$predictionInterval)) parameters$commu_metricsNWT$predictionInterval else 1

lastY <- if (times$end != times$start) times$end else NULL
names(birdPrediction) <- nms
succTS <- c(seq(times$start, times$end, 
                by = tstep), lastY)
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