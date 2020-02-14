# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "climate_NWT_DataPrep",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = c(person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5", climate_NWT_DataPrep = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "climate_NWT_DataPrep.Rmd"),
  reqdPkgs = list("raster", "stringr"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".runInitialTime", "numeric", default = start(sim), desc = "when to start this module? By default, the start time of the simulation."),
    defineParameter(".runInterval", "numeric", default = 30, desc = "optional. Interval between two runs of this module,expressed in units of simulation time. Defaults to 30 years"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(
      objectName = "rasterToMatch",
      objectClass = "raster",
      sourceURL = "https://drive.google.com/open?id=1NIjFbkckG3sewkTqPGaBGQDQLboPQ0wc",
      desc = "Raster template for GIS operations."
    )
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(
      objectName = "climateLayers",
      objectClass = "RasterStack",
      desc = "RasterStack with climate layers for the current time step."
    )
  )
))

## event types
#   - type `init` is required for initialization

doEvent.climate_NWT_DataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      # do stuff for this event
      sim <- Init(sim)
      sim <- updateClimateLayers(sim) # Hack of the Friday afternoon --' # Doesn't load if it is Cached! ~TM
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "climate_NWT_DataPrep", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "climate_NWT_DataPrep", "save")
      
      sim <- scheduleEvent(sim, P(sim)$.runInitialTime + 1, "climate_NWT_DataPrep", "updateClimateLayers")
    },
    updateClimateLayers = {
      sim <- updateClimateLayers(sim)
      sim <- scheduleEvent(sim, 2040, "climate_NWT_DataPrep", "updateClimateLayers")
      sim <- scheduleEvent(sim, 2070, "climate_NWT_DataPrep", "updateClimateLayers")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "climate_NWT_DataPrep", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "climate_NWT_DataPrep", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "climate_NWT_DataPrep", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "climate_NWT_DataPrep", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

updateClimateLayers <- function(sim){

    currentTime <- time(sim, "year")
  
  # 2020s: average for years 2011-2040, 2050s: 2041-2070, 2080s: 2071-2100. 
  
  if (currentTime < 2041)
  {
    period <- 2025
  }
  else if (currentTime < 2071)
  {
    period <- 2055
  }
  else
  {
    period <- 2085
  }
  
  prepInputs7z <- function(gcm, rcp, period, pathData){
    zipfile <- paste0(gcm, "_rcp", rcp, "_", period, "_Monthly_ASCII.7z")

    # downloadData(
    #   module = attr(mod, "name"),
    #   urls = paste0("http://www.cacpd.org/selected_AOGCMs/", zipfile)
    # ) # Doesn't work if you don't have a Checksums file... ~TM
    if (!file.exists(file.path(pathData, zipfile), recursive = TRUE)){
      message("Downloading climate data...")
      download.file(url = paste0("http://www.cacpd.org/selected_AOGCMs/", zipfile),
                    destfile = file.path(pathData, zipfile))
    }

    # Check for 7zip
    if (Sys.which("7z") == "") {
      msg <- if (.Platform$OS.type != "windows") {
        "If you have sudo powers, you can run \nsudo apt-get install p7zip p7zip-full"
      } else {
        ""
      }
      stop("7zip should be installed on your machine and '7z' should be in your path. ", msg)
    }
      
    # wd <- setwd(dataPath(sim))
    # on.exit(setwd(wd)) # This breaks everything else for all the other modules... ~TM

    # Extract zip contents
    # system(paste("7z e", zipfile, "-aos")) # Use -aos switch to skip files already extracted # Doesn't work if not passing the destination path ~TM
    message(paste0("Unzipping data for: gcm = ", P(sim)$gcm, " rcp = ", P(sim)$rcp, " period = ", period))
    system(paste0("7z x ", paste0(pathData, "/", zipfile), " -o", dataPath(sim))) # Use -aos switch to skip files already extracted # doesn't work ~TM
    
    message("Writting Checksums for climate data")
    reproducible::Checksums(path = pathData, write = TRUE)
  }
  
  Cache(prepInputs7z, gcm = P(sim)$gcm, rcp = P(sim)$rcp, 
        period = period, pathData = dataPath(sim))
   
  sim[["climateLayers"]] <- Cache(buildClimateLayers, rasterResolution = raster::res(sim[["rasterToMatch"]]),
                                               rtm = sim[["rasterToMatch"]],
                                               dataPathSim = dataPath(sim),
                                               gcm = P(sim)$gcm,
                                               rcp = P(sim)$rcp,
                                               period = period,
                                               omitArgs = c("dataPathSim"),
                                  userTags = c(paste0("climateLayersRes", raster::res(sim[["rasterToMatch"]])[1])))
  # cacheId = 0e96099cee3e1a5e # Res 250 NWT
  invisible(sim)            
}


### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  # This module can't work with relative time, as it is time explicit.
if (nchar(start(sim)) < 4)
  stop("fireSense is a time explicit module. Please provide start and end time as YYYY")
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
