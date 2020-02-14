# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_EscapePredict",
  description = "Predicts fire escape probabilities using a model fitted with
                 the fireSense_EscapeFit module. These can be used to feed the 
                 fire escape component of a landscape fire model (e.g fireSense).",
  keywords = c("escape probability", "fire frequency", "logistic", "fireSense"),
  authors = person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0", fireSense_EscapePredict = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_EscapePredict.Rmd"),
  reqdPkgs = list("magrittr", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(name = "modelObjName", class = "character", 
                    default = "fireSense_EscapeFitted",
                    desc = "name of the object of class fireSense_EscapeFit
                            describing the statistical model used for
                            predictions."),
    defineParameter(name = "data", class = "character",
                    default = "dataFireSense_EscapePredict",
                    desc = "a character vector indicating the names of objects 
                            in the `simList` environment in which to look for 
                            variables present in the model formula. `data`
                            objects can be data.frames, RasterLayers, RasterStacks or
                            RasterBricks. However, data.frames cannot be mixed
                            with objects of other classes."),
    defineParameter(name = "mapping", class = "character, list", default = NULL,
                    desc = "optional named vector or list of character strings
                            mapping one or more variables in the model formula
                            to those in `data` objects."),
    defineParameter(name = ".runInitialTime", class = "numeric",
                    default = start(sim),
                    desc = "when to start this module? By default, the start 
                            time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = 1, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in units of simulation time. By default, 1 year."),
    defineParameter(name = ".saveInitialTime", class = "numeric", default = NA, 
                    desc = "optional. When to start saving output to a file."),
    defineParameter(name = ".saveInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = rbind(
    expectsInput(
      objectName = "fireSense_EscapeFitted",
      objectClass = "fireSense_EscapeFit",
      sourceURL = NA_character_,
      desc = "An object of class fireSense_EscapeFit created with the fireSense_EscapeFit module."
    ),
    expectsInput(
      objectName = "dataFireSense_EscapePredict",
      objectClass = "data.frame, RasterLayer, RasterStack",
      sourceURL = NA_character_,
      desc = "One or more objects of class data.frame, RasterLayer or RasterStack in which to look for variables with which to predict."
    )
  ),
  outputObjects = createsOutput(
    objectName = "fireSense_EscapePredicted",
    objectClass = NA_character_,
    desc = "An object whose class depends on that of the inputs, could be a RasterLayer or a vector of type numeric."
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense_EscapePredict = function(sim, eventTime, eventType, debug = FALSE)
{
  moduleName <- current(sim)$moduleName
  
    switch(
    eventType,
    init = {
      sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, current(sim)$moduleName, "run")
      
      if (!is.na(P(sim)$.saveInitialTime))
        sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", .last())
    },
    run = { 
      sim <- escapePredictRun(sim) 
      
      if (!is.na(P(sim)$.runInterval))
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, moduleName, "run", eventPriority = 5.5) # THIS WILL HAVE TO CHANGE AND BECOME QUICK FOR EVERY YEAR
    },
    save = { 
      sim <- escapePredictSave(sim) 
      
      if (!is.na(P(sim)$.saveInterval))
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, moduleName, "save", .last())
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  
  invisible(sim)
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

escapePredictRun <- function(sim)
{
  moduleName <- current(sim)$moduleName
  
  if (!is(sim[[P(sim)$modelObjName]], "fireSense_EscapeFit"))
    stop(moduleName, "> '", P(sim)$modelObjName, "' should be of class 'fireSense_EscapeFit.")
  
  ## Toolbox: set of functions used internally by escapePredictRun
  escapePredictRaster <- function(model, data, sim)
  {
    model %>%
      model.matrix(data) %>%
      `%*%` (coef(sim[[P(sim)$modelObjName]])) %>%
      drop %>% sim[[P(sim)$modelObjName]]$family$linkinv(.)
  }
  
  # Load inputs in the data container
  # list2env(as.list(envir(sim)), envir = mod)
  
  mod_env <- new.env()
  
  for (x in P(sim)$data) 
  {
    if (!is.null(sim[[x]]))
    {
      if (is.data.frame(sim[[x]]))
      {
        list2env(sim[[x]], envir = mod_env)
      } 
      else if (is(sim[[x]], "RasterStack") || is(sim[[x]], "RasterBrick")) 
      {
        list2env(setNames(unstack(sim[[x]]), names(sim[[x]])), envir = mod_env)
      }
      else if (is(sim[[x]], "RasterLayer"))
      {
        mod_env[[x]] <- sim[[x]]
      } 
      else stop(moduleName, "> '", x, "' is not a data.frame, a RasterLayer, a RasterStack or a RasterBrick.")
    }
  }
  
  terms <- delete.response(terms.formula(sim[[P(sim)$modelObjName]]$formula))
  
  ## Mapping variables names to data
  if (!is.null(P(sim)$mapping))
  {
    for (i in 1:length(P(sim)$mapping)) 
    {
      attr(terms, "term.labels") %<>% gsub(
        pattern = names(P(sim)$mapping[i]),
        replacement = P(sim)$mapping[[i]],
        x = .
      )
    }
  }
  
  formula <- reformulate(attr(terms, "term.labels"), intercept = attr(terms, "intercept"))
  allxy <- all.vars(formula)
  
  if (all(unlist(lapply(allxy, function(x) is.vector(mod_env[[x]]))))) 
  {
    mod_env[["predictEscapeFun"]] <- function()
    {
      formula %>%
        model.matrix(mod_env) %>%
        `%*%` (coef(sim[[P(sim)$modelObjName]])) %>%
        drop %>% sim[[P(sim)$modelObjName]]$family$linkinv(.)
    }
  } 
  else if (all(unlist(lapply(allxy, function(x) is(mod_env[[x]], "RasterLayer"))))) 
  {
    mod_env[["predictEscapeFun"]] <- function()
    {
      mget(allxy, envir = mod_env, inherits = FALSE) %>%
        stack %>% raster::predict(model = formula, fun = escapePredictRaster, na.rm = TRUE, sim = sim)  
    }
  } 
  else 
  {
    missing <- !allxy %in% ls(mod_env, all.names = TRUE)
    
    if (s <- sum(missing))
      stop(moduleName, "> '", allxy[missing][1L], "'",
           if (s > 1) paste0(" (and ", s-1L, " other", if (s>2) "s", ")"),
           " not found in data objects.")
    
    badClass <- unlist(lapply(allxy, function(x) is.vector(mod_env[[x]]) || is(mod_env[[x]], "RasterLayer")))
    
    if (any(badClass)) 
    {
      stop(moduleName, "> Data objects of class 'data.frame' cannot be mixed with objects of other classes.")
    } 
    else 
    {
      stop(moduleName, "> '", paste(allxy[which(!badClass)], collapse = "', '"),
           "' does not match a data.frame's column, a RasterLayer or a layer from a RasterStack or RasterBrick.")
    }
  }
  
  sim[["fireSense_EscapePredicted"]] <- mod_env[["predictEscapeFun"]]()
  
  invisible(sim)
}

escapePredictSave <- function(sim)
{
  moduleName <- current(sim)$moduleName
  timeUnit <- timeunit(sim)
  currentTime <- time(sim, timeUnit)
  
  saveRDS(
    sim$fireSense_EscapePredicted, 
    file = file.path(paths(sim)$out, paste0("fireSense_EscapePredicted_", timeUnit, currentTime, ".rds"))
  )
  
  invisible(sim)
}

