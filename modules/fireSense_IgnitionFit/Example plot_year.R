library(dplyr)
library(SpaDES)

set.seed(1)

modulePath <- "~/git/SpaDES_Modules"
start <- end <- 1

# Define simulation parameters
times <- list(start = start, end = end, timeunit = "year")
modules <- list("fireSense_FrequencyFit", "fireSense_FrequencyPredict")
paths <- list(
  modulePath = modulePath
)

# Create random weather and fire frequency dataset
dummyData <- data.frame(
  weather = rep(1:100, each = 10),
  nFires = rpois(1000, lambda=rep(10:1, each = 100)),
  year = rep(1:10, each = 100)
)


# Define module parameters
parameters <- list(
  fireSense_FrequencyFit = list(
    formula = nFires ~ weather,
    family = poisson(),
    data = "dummyData"
  ),
  fireSense_FrequencyPredict = list(
    modelName = "fireSense_FrequencyFitted",
    data = "dummyData"
  )
)

# Objects to pass from the global environment to the simList environment
objects <- "dummyData"

# Create the simList
sim <- simInit(
  times = times, 
  params = parameters, 
  modules = modules, 
  objects = objects, 
  paths = paths
)

sim <- spades(sim)

# Prepare data
data <- bind_cols(dummyData, list(predict = sim$fireSense_FrequencyPredicted)) 

# Plot predictions versus observations
data %>%
  group_by(year) %>%
  summarise(observed = sum(nFires), predicted = sum(predict)) %>%
  with(., plot(predicted ~ .$observed, xlim = c(150, 1200), ylim = c(150, 1200), pch = 16))
abline(0,1, col = "red", lwd = 2)

# Predicted number of fires as a function of a covariate, here weather
x11()
with(data, plot(sim$fireSense_FrequencyPredicted ~ weather, 
                ylab = expression(Predicted~number~of~fires~occurrences), type = "l", lwd = 2))




