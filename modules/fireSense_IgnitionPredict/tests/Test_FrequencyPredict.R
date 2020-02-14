library(magrittr)
library(raster)
library(SpaDES)

set.seed(1)

modulePath <- normalizePath("..")

start <- end <- 2

# Define simulation parameters
times <- list(start = start, end = end, timeunit = "year")
modules <- list("fireSense_IgnitionPredict")
paths <- list(
  modulePath = modulePath
)

# Create random weather and fire frequency data
  # data.frame
  dataFireSense_IgnitionPredict <- data.frame(
    weather = rnorm(1000, 150, 30),
    fireFrequency = rpois(1000, .5)
  )
    
  # raster
  nx <- ny <- 100L
  dummyRaster <- raster(nrows = ny, ncols = nx, xmn = -nx/2, xmx = nx/2, ymn = -ny/2, ymx = ny/2)
  dataFireSense_IgnitionPredict <- dummyRaster %>%
    gaussMap(scale = 300, var = 0.03, speedup = nx/5e2, inMemory = TRUE) %>%
    stack %>% setNames("weather")

# Create a typical output of fireSense_IgnitionFit
fireSense_IgnitionFitted <- list(
  formula = fireFrequency ~ weather2,
  family = poisson(),
  coef = setNames(c(0.1, 0.01), c("intercept", "weather2"))
)
class(fireSense_IgnitionFitted) <- "fireSense_IgnitionFit"

# Define module parameters
parameters <- list(
  fireSense_IgnitionPredict = list(
    modelName = "fireSense_IgnitionFitted",
    data = "dataFireSense_IgnitionPredict",
    mapping = list(weather2 = "weather"), # One can use mapping to map variables
                                          # in the formula of the fitted object
                                          # to those in data. Here weather2
                                          # (formula) is mapped to weather (data).
    f = 10
  )
)

# Objects to pass from the global environment to the simList environment
objects <- c("dataFireSense_IgnitionPredict", "fireSense_IgnitionFitted")

# Create the simList
sim <- simInit(
  times = times, 
  params = parameters, 
  modules = modules, 
  objects = objects, 
  paths = paths
)

sim <- spades(sim)
X11(); plot(sim$fireSense_IgnitionPredicted)
