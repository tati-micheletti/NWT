library(magrittr)
library(raster)
library(SpaDES)

modulePath <- normalizePath("..")

start <- end <- 1

# Define simulation parameters
times <- list(start = start, end = end, timeunit = "year")
modules <- list("fireSense_EscapePredict")
paths <- list(
  modulePath = modulePath
)

# Create random weather and fire escape data
  # data.frame
dataFireSense_EscapePredict <- data.frame(
  weather = rnorm(1000, 150, 30),
  escapeProb = rbinom(1000, 1, .5)
)  

  # raster
  nx <- ny <- 100L
  dataFireSense_EscapePredict <- raster(nrows = ny, ncols = nx, xmn = -nx/2, xmx = nx/2, ymn = -ny/2, ymx = ny/2) %>%
        gaussMap(scale = 20, var = 100, speedup = nx/5e2, inMemory = TRUE) %>%
        stack %>% setNames("weather")

  # Create a typical output of fireSense_EscapeFit
  fireSense_EscapeFitted <- glm(
    formula = escapeProb ~ weather,
    family = binomial(),
    data = data.frame(
      weather = rnorm(1000, 150, 100),
      escapeProb = rbinom(1000, 1, .5)
    )
  )
  class(fireSense_EscapeFitted) <- c("fireSense_EscapeFit", class(fireSense_EscapeFitted))

# Define module parameters
parameters <- list(
  fireSense_EscapePredict = list(
    modelName = "fireSense_EscapeFitted",
    data = "dataFireSense_EscapePredict",
    .runInterval = 1
  )
)

# Objects to pass from the global environment to the simList environment
objects <- c("dataFireSense_EscapePredict", "fireSense_EscapeFitted")

# Create the simList
sim <- simInit(
  times = times, 
  params = parameters, 
  modules = modules, 
  objects = objects, 
  paths = paths
)

sim <- spades(sim)
x11(); plot(sim$fireSense_EscapePredicted)

