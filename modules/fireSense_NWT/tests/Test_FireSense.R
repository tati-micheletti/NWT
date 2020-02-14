library(raster)
library(SpaDES)

# options(spades.moduleCodeChecks = TRUE)

modulePath <- normalizePath("..")
paths <- list(
  modulePath = modulePath,
  outputPath = file.path(modulePath, "outputs")
)

# rasterOptions(maxmemory = 1e8)

# We start by fitting the spread model using the fireSense_SpreadFit module. In
# order to fit this model we need data about the shape of the fire size
# distribution. We can predict it using the fireSense_SizePredict module and fit
# the model to our study region using the fireSense_SizeFit module.

start <- 2000
end <- 2010

inputs <- rbind(
  data.frame(
    objectName = "dataFireSense_SizeFit",
    files = normalizePath("tests/data/dataFireSense_SizeFit.rds"),
    functions = "base::readRDS",
    loadTime = start
  ),
  do.call(
    "rbind",
    lapply(
      start:end,
      function(year)
        data.frame(
          objectName = "dataFireSense_SizePredict", 
          files = normalizePath(paste0("tests/data/dataFireSense_SizePredict_RASTER_", year, ".rds")),
          functions = "base::readRDS",
          loadTime = year
        )
    )
  )
)

# Define module parameters
parameters <- list(
  fireSense_SizeFit = list(
    formula = list(beta = fireSize ~ cn + ot + dt + wt + MDC_07,
                   theta = fireSize ~ cn + ot + dt + wt + MDC_07),
    a = 1,
    itermax = 5000,
    trace = 100
  ),
  fireSense_SizePredict = list(
    .runInterval = 1,
    .saveInitialTime = start,
    .saveInterval = 1
  )
)

sim <- simInit(
  times = list(start = start, end = end, timeunit = "year"),
  modules = list("fireSense_SizeFit",
                 "fireSense_SizePredict"),
  paths = paths,
  inputs = inputs,
  params = parameters
)

sim <- spades(sim) # Outputs tapered Pareto's Beta and Theta maps for each year

# Test passing inputs as objects
TP_Beta <- stack(
  lapply(
    start:end,
    function(year)
    {
      readRDS(paste0("../outputs/fireSense_SizePredicted_Beta_year", year, ".rds"))
    }
  )
)

TP_Theta <- stack(
  lapply(
    start:end,
    function(year)
    {
      readRDS(paste0("../outputs/fireSense_SizePredicted_Theta_year", year, ".rds"))
    }
  )
)

# Then we can fit the spread model using the fireSense_SpreadFit module
inputs <- data.frame(
  objectName = "fireLoc_FireSense_SpreadFit",
  files = normalizePath("tests/data/fireLoc_FireSense_SpreadFit.shp"),
  functions = "raster::shapefile",
  loadTime = 1
)

objects <- c("TP_Beta", "TP_Theta")

# Define module parameters
parameters <- list(
  fireSense_SpreadFit = list(
    formula = ~ TP_Beta + TP_Theta,
    data = c("TP_Beta", "TP_Theta"),
    lower = c(.01, .2, .1,  .3, .001, .001, .001),
    upper = c(.20, .5, 10,  4., .300, .300, .300),
    nCores = 1,
    trace = 1,
    itermax = 1
  )
)

sim <- simInit(
  times = list(start = 1, end = 1, timeunit = "year"),
  modules = list("fireSense_SpreadFit"),
  paths = paths,
  inputs = inputs,
  objects = objects,
  params = parameters
)

sim <- spades(sim)
fireSense_SpreadFitted <- sim$fireSense_SpreadFitted


# Then we can fit frequency and escape models using respectively the 
# fireSense_FrequencyFit and fireSense_EscapeFit modules and predict using their
# companion modules (fireSense_*Predict). Then predict spread probabilities 
# using fireSense_SpreadPredict and finally run fireSense.

list2env(
  setNames(
    unstack(TP_Beta),
    nm = paste0("TP_Beta", start:end)
  ),
  envir = globalenv()
)
list2env(
  setNames(
    unstack(TP_Theta),
    nm = paste0("TP_Theta", start:end)
  ),
  envir = globalenv()
)

# Define from where and how data will be loaded in the simList environment
inputs <- rbind(
  data.frame(
    objectName = "dataFireSense_FrequencyFit",
    file = normalizePath("tests/data/dataFireSense_FrequencyFit.rds"),
    functions = "base::readRDS",
    arguments = NA,
    loadTime = start
  ),
  do.call(
    "rbind",
    lapply(
      start:end,
      function(year)
        data.frame(
          objectName = "tests/dataFireSense_FrequencyPredict", 
          file = normalizePath(paste0("tests/data/dataFireSense_SizePredict_RASTER_", year, ".rds")),
          functions = "base::readRDS",
          arguments = NA,
          loadTime = year
        )
    )
  ),
  data.frame(
    objectName = "dataFireSense_EscapeFit",
    file = normalizePath("tests/data/dataFireSense_EscapeFit.rds"),
    functions = "base::readRDS",
    arguments = NA,
    loadTime = start
  ),
  {
    # Note 'arguments' must be a list of NROW(inputs), with each element itself being a list,
    #  which is passed to do.call(fun[x], arguments[[x]]), where x is row number, one at a time
    args <- c(
      lapply(
        start:end,
        function(x)
        {
          list(x = paste0("TP_Beta", x), envir = globalenv())
        }
      ),
      lapply(
        start:end,
        function(x)
        {
          list(x = paste0("TP_Theta", x), envir = globalenv())
        }
      )
    )
    data.frame(
      objectName = c("TP_Beta", "TP_Theta"),
      file = NA,
      functions = "base::get",
      arguments = I(args),
      loadTime = start:end
    )
  }
)

objects <- c("fireSense_SpreadFitted")

# Define module parameters
parameters <- list(
  fireSense_FrequencyFit = list(
    formula = nFires ~ hw:MDC_07 + cn:MDC_07 + ot:MDC_07 + dt:MDC_07 - 1,
    family = MASS::negative.binomial(theta = 1, link = "identity"),
    ub = list(coef = c(1, 1, 1, 1)),
    trace = 100,
    itermax = 100,
    nTrials = 100
  ),
  fireSense_FrequencyPredict = list(
    f = 100
  ),
  fireSense_EscapeFit = list(
    formula = cbind(escaped, nFires) ~ MDC_07 + hw + dt + ot + wt
  ),
  fireSense_EscapePredict = list(
    data = "dataFireSense_FrequencyPredict"
  ),
  fireSense_SpreadPredict = list(
    data = c("dataFireSense_FrequencyPredict", "TP_Beta", "TP_Theta")
  ),
  fireSense = list(
    mapping = list(
      ignitionProb = "fireSense_FrequencyPredicted",
      escapeProb = "fireSense_EscapePredicted",
      spreadProb = "fireSense_SpreadPredicted"
    )
  )
)

modules <- list(
  "fireSense_FrequencyFit",
  "fireSense_FrequencyPredict",
  "fireSense_EscapeFit",
  "fireSense_EscapePredict",
  "fireSense_SpreadPredict",
  "fireSense"
)

sim <- simInit(
  times = list(start = start, end = end, timeunit = "year"),
  modules = modules,
  paths = paths,
  inputs = inputs,
  objects = objects,
  params = parameters,
  loadOrder = unlist(modules)
)

sim <- spades(sim)

