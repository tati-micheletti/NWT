library(SpaDES.core)
options(reproducible.useCache = TRUE)
setPaths(modulePath = file.path(getwd(), "modules"),
         inputPath = checkPath(file.path(getwd(), "testingCaribouIK"), create = TRUE),
         cachePath = file.path(getwd(), "cache"),
         outputPath = checkPath(file.path(getwd(), "testingCaribouIK"), create = TRUE))

getPaths() # shows where the 4 relevant paths are

times <- list(start = 2011, end = 2042)

parameters <- list(
  "caribouCIP" = list(
    "predictionInterval" = 30,
    "includeLastYear" = TRUE
  ),
  "caribouIK" = list(
    "decidousSp" = c("Betu_Pap", "Popu_Tre", "Popu_Bal"),
    "predictionInterval" = 30,
    "subsetForModel" = 50
  )
)
modules <- list("caribouIK", "caribouCIP")
objects <- list()
inputs <- list()
outputs <- list()

mySim <- simInitAndSpades(times = times, params = parameters, modules = modules,
                          objects = objects, debug = 1)