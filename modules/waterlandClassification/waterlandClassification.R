defineModule(sim, list(
  name = "waterlandClassification",
  description = paste0("This module can be used to return a raster identifying", 
                       " which pixels are lowlands and which pixels are uplands by using LCC05 or LCC10",
                       "and the Hybrid Wetland Layer from Ducks Unlimited Canada v. 2.1"),
  keywords = "lowlands",
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.4", waterlandClassification = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "waterlandClassification.Rmd"),
  reqdPkgs = list("magrittr"),
  parameters = rbind(
    defineParameter(name = ".useCache", class = "logical", default = FALSE, min = NA, max = NA, 
                    desc = "Should this entire module be run with caching activated?"),
    defineParameter(name = "baseLayer", class = "character", default = "both", min = NA, max = NA, 
                    desc = "Which layer should be used? LCC05, LCC10 or both?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "rasterDUCKS", objectClass = "RasterLayer", 
                 desc = "Hybrid Wetland Layer from Ducks Unlimited Canada v. 2.1", 
                 sourceURL = "https://drive.google.com/open?id=1sQBdeCyWvVH-aYcWNGyo6w8x6RNYMJgk"),
    expectsInput(objectName = "studyArea", objectClass = "shapefile", 
                 desc = "Shapefile of the studyArea to be used",
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "wetLCC05", objectClass = "RasterLayer", 
                  desc = paste0("RasterLayer containing identifying", 
                                " uplands and lowlands in a given study area based on LCC05")),
    createsOutput(objectName = "wetLCC10", objectClass = "RasterLayer", 
                  desc = paste0("RasterLayer containing identifying", 
                                " uplands and lowlands in a given study area based on LCC10")),
    createsOutput(objectName = "wetDiagnostics", objectClass = "list", 
                  desc = paste0(" It returns the diagnostics on which approach should be used:", 
                                "pixel based, or class using XXXXXX"))
  )
))

doEvent.waterlandClassification = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # 2. Load the layer 
      # 1. Test that the study area is inside the DUCKS layer. If not, return warning that only the area (%?) is inside and will be assessed
      # 3. Run the events (create wetZone based on LCC05 pixel, create wetZone based on LCC10, run diagnostics comparing to pure layer)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "waterlandClassification", "loadLayers")
      sim <- scheduleEvent(sim, start(sim), "waterlandClassification", "testStudyArea")
      sim <- scheduleEvent(sim, start(sim), "waterlandClassification", "createWetZone") # 2 outputs : wetLCC05 and wetLCC10 based
      sim <- scheduleEvent(sim, start(sim), "waterlandClassification", "createWetZone")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "waterlandClassification", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "waterlandClassification", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "waterlandClassification", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "waterlandClassification", "templateEvent")

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

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
