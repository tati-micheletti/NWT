
# Load SpaDES
library("SpaDES")
library("raster")
setwd("/mnt/data/Micheletti/NWT/modules/caribouRSF/")
# Source functions in R folder
invisible(sapply(X = list.files(file.path(getwd(), "R"), full.names = TRUE), FUN = source))
invisible(sapply(X = list.files(file.path(dirname(dirname(getwd())), "functions"), full.names = TRUE), FUN = source))

# Set a storage project folder
workDirectory <- getwd()
message("Your current temporary directory is ", tempdir())

setPaths(modulePath = file.path(dirname(getwd())), 
         inputPath = file.path("/mnt/data/Micheletti/NWT/outputs/30JUL19/run7/"), 
         outputPath = file.path("/mnt/data/Micheletti/NWT/outputs/30JUL19/RSF/"), 
         cachePath = file.path("/mnt/data/Micheletti/NWT/cache/"))
getPaths() # shows where the 4 relevant paths are

times <- list(start = 2091, end = 2100) # 2081

parameters <- list(
  "decidousSp" = c("Betu_Pap", "Popu_Tre", "Popu_Bal"),
  "predictionInterval" = 10
)
modules <- list("caribouRSF")
.objects <- list()
inputs <- list()
outputs <- list()

if (quickPlot::isRstudioServer()) options(httr_oob_default = TRUE)

RSF_CS <- simInitAndSpades(times = times, params = parameters, modules = modules,
                           objects = .objects, debug = 2)
saveRDS(RSF_CS, file.path(getPaths()$outputPath, "RSF_CS.rds"))