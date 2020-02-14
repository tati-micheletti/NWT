# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_NWT",
  description = "A landscape fire model, sensitive to environmental changes (e.g.
                 weather and land-cover).",
  keywords = c("fire", "percolation", "environmental control", "feedback", 
                "weather", "vegetation", "land-cover"),
  authors = c(person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_NWT.Rmd"),
  reqdPkgs = list("data.table", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", default, min, max, "parameter description")),
    defineParameter(name = "mapping", class = "character, list", default = NULL,
                    desc = "optional named vector or list of character strings 
                            mapping one or more inputs required by the module to
                            objects loaded in the simList environment."),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start
                            time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = 1, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in units of simulation time."),
    defineParameter(name = ".saveInitialTime", class = "numeric", default = NA, 
                    desc = "optional. When to start saving output to a file."),
    defineParameter(name = ".saveInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between save events."),
    defineParameter(name = ".plotInitialTime", class = "numeric", default = NA, 
                    desc = "optional. When to start plotting."),
    defineParameter(name = ".plotInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between plot events.")
  ),
  inputObjects = rbind(
    expectsInput(
      objectName = "ignitionProb",
      objectClass = "RasterLayer",
      sourceURL = NA_character_,
      desc = "A RasterLayer or RasterStack (time series) describing spatial
              variations in ignition probabilities. The name of the expected 
              input might also be fireSense_FrequencyPredicted"
    ),
    expectsInput(
      objectName = "escapeProb",
      objectClass = "RasterLayer",
      sourceURL = NA_character_,
      desc = "A RasterLayer or RasterStack (time series) describing spatial
              variations in escape probabilities. The name of the expected 
              input might also be fireSense_EscapePredicted"
    )
  ),
  outputObjects = rbind(
    createsOutput(
      objectName = "spreadState",
      objectClass = "numeric",
      desc = "data.table describing the current state of burning pixels."
    )
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense_NWT = function(sim, eventTime, eventType, debug = FALSE) 
{
  switch(
    eventType,
    init = { sim <- init(sim) },
    predict = { sim <- predict(sim) },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  invisible(sim)
}


init <- function(sim) 
{
  moduleName <- current(sim)$moduleName
  
  sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "predict")
  
  sim
}

predict <- function(sim) 
{
  moduleName <- current(sim)$moduleName
  currentTime <- time(sim, timeunit(sim))
  ## Mapping
  mod[["ignitionProb"]] <- 
    if (!is.null(P(sim)[["mapping"]][["ignitionProb"]])) 
      sim[[P(sim)[["mapping"]][["ignitionProb"]]]] 
    else
      if (!is.null(sim[["ignitionProb"]])){
        sim[["ignitionProb"]]
      } else {
        if (!is.null(sim[["fireSense_FrequencyPredicted"]])){
          sim[["fireSense_FrequencyPredicted"]]
        } else {
          if (!is.null(sim[["fireSense_IgnitionPredicted"]])){
            sim[["fireSense_IgnitionPredicted"]]
          } else {
            stop("Neither `fireSense_FrequencyPredicted` (i.e. being deprecated), `fireSense_IgnitionPredicted` nor 
                 `ignitionProb` were found. Please provide one of these")
          }
        }        
      }
  
  mod[["escapeProb"]] <-
    if (!is.null(P(sim)[["mapping"]][["escapeProb"]]))
      sim[[P(sim)[["mapping"]][["escapeProb"]]]]
    else
      if (!is.null(sim[["escapeProb"]])){
        sim[["escapeProb"]]
      } else {
        if (!is.null(sim[["fireSense_EscapePredicted"]])){
          sim[["fireSense_EscapePredicted"]]
        } else {
          stop("Neither `fireSense_EscapePredicted` nor `escapeProb` were found. Please provide one of these")
        }        
      }
      
  ## Ignite
  notNA <- which(!is.na(mod[["ignitionProb"]][]))
  ignitionProb <- mod[["ignitionProb"]][notNA]
  
  ignited <- notNA[which(
    rbinom(n = length(ignitionProb),
           size = 1,
           prob = pmin(ignitionProb, 1)
    ) > 0
  )]
  
  rm(ignitionProb)
  
  if (length(ignited) > 0L)
  {
    ## Escape
    adjacent <- SpaDES.tools::adj(
      x = mod[["escapeProb"]],
      cells = ignited,
      directions = 8,
      returnDT = TRUE
    )
    
    if (is.matrix(adjacent))
      adjacent <- as.data.table(adjacent)
    
    from <- unique(adjacent, by = "from")
    from[, `:=` (probEscape = mod[["escapeProb"]][from], to = NULL)]
    
    # Update probEscape to get p0
    p0 <- with(
      from[adjacent, on = "from"][!is.na(probEscape)][
        , 
        probEscape := (1 - (1 - probEscape)^(1 / .N)),
        by = "from"
        ],
      {
        p0 <- mod[["escapeProb"]]
        p0[to] <- probEscape
        p0
      }
    )
    sim$spreadState <- SpaDES.tools::spread2(
      landscape = mod[["escapeProb"]],
      start = ignited,
      iterations = 1,
      spreadProb = p0,
      asRaster = FALSE
    )
  }
  
  if (!is.na(P(sim)$.runInterval))
    sim <- scheduleEvent(sim, currentTime + P(sim)$.runInterval, moduleName, "predict")
  
  invisible(sim)
}
