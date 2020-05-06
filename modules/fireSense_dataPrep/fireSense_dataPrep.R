## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "fireSense_dataPrep",
  description = paste0("Data preparation module for fireSense. ",
                       "Currently only the dataPrep for the spread prediction is working. ",
                       "This module needs", 
                       " to happen before any fireSense modules"),
  keywords = c("fireSense", "spread", "data munging"),
  authors = structure(list(list(given = "Tati", family = "Micheletti", role = c("aut", "cre"), 
                                email = "tati.micheletti@gmail.com", comment = NULL)), 
                      class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.0.9004", fireSense_dataPrep = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "fireSense_dataPrep.Rmd")),
  reqdPkgs = list("raster"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter("whichModulesToPrepare", "character", 
                    default = c("fireSense_IgnitionFit", "fireSense_IgnitionPredict",
                                "fireSense_EscapeFit", "fireSense_EscapePredict",
                                "fireSense_SpreadFit", "fireSense_SpreadPredict"), NA, NA,
                    paste("For which fireSense modules this data preparation process should",
                          "be ran? Defaults for all 6 modules.")),
    defineParameter("predictionInterval", "numeric", 1, NA, NA,
                    "This describes the simulation time interval between prediction events."),
    defineParameter(name = "RCP", class = "character", default = "85", 
                    min = NA, max = NA, 
                    desc = "Which RCP should be used? Default to 85"),
    defineParameter(name = "climateModel", class = "character", default = "CCSM4", 
                    min = NA, max = NA, 
                    desc = "Which climate model should be used? Default to CCSM4"),
    defineParameter(name = "ensemble", class = "character", default = "CCSM4", 
                    min = NA, max = NA, 
                    desc = paste("Which ensemble model should be used? Default to ''. ",
                                 "CCSM4 doesn't have ensemble, just CanESM2 (r11i1p1)")),
    defineParameter(name = "climateResolution", class = "character", default = "3ArcMin", 
                    min = NA, max = NA, 
                    desc = paste("Which DEM resolution was used for generating the climate layers?",
                                 "Default to '3ArcMin'.")),
    defineParameter(name = "climateFilePath", class = "character", 
                    default = "https://drive.google.com/open?id=17idhQ_g43vGUQfT-n2gLVvlp0X9vo-R8", 
                    min = NA, max = NA, 
                    desc = paste("URL to zipped climate file coming from ClimateNA, containing all",
                                 "climate variables for all years of simulation"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", 
                 desc = paste0("Table that defines the cohorts by pixelGroup"), 
                 sourceURL = NA), 
    expectsInput(objectName = "pixelGroupMap", objectClass = "RasterLayer", 
                 desc = paste0("RasterLayer that defines the pixelGroups for cohortData table"), 
                 sourceURL = NA), 
    expectsInput(objectName = "rasterToMatch",
                 objectClass = "RasterLayer",
                 sourceURL = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df",
                 desc = "a template raster describing the studyArea"),
    expectsInput(objectName = "studyArea",
                 objectClass = "SpatialPolygonsDataFrame",
                 sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU",
                 desc = "a template polygon describing the studyArea"),
    expectsInput(objectName = "usrEmail",
                 objectClass = "character",
                 sourceURL = NA,
                 desc = "User e.mail for GDrive authorization"),
    expectsInput(objectName = "flammableRTM",
                 objectClass = "RasterLayer",
                 sourceURL = NA,
                 desc = "RTM without ice/rocks/urban/water. Flammable map with 0 and 1.")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "dataFireSense_IgnitionFit", 
                  objectClass = "RasterStack", 
                  desc = paste0("One or more RasterLayers or RasterStacks in which to look for ",
                                "variables present in the model formula.")),
    createsOutput(objectName = "dataFireSense_EscapeFit", 
                  objectClass = "RasterStack", 
                  desc = paste0("One or more RasterLayers or RasterStacks in which to look for ",
                                "variables present in the model formula.")),
    createsOutput(objectName = "dataFireSense_SpreadFit", 
                  objectClass = "RasterStack", 
                  desc = paste0("One or more RasterLayers or RasterStacks in which to look for ",
                                "variables present in the model formula.")),
    createsOutput(objectName = "dataFireSense_IgnitionPredict", 
                  objectClass = "RasterStack", 
                  desc = paste0("One or more RasterLayers or RasterStacks in which to look for ",
                                "variables present in the model formula.")),
    createsOutput(objectName = "dataFireSense_EscapePredict", 
                  objectClass = "RasterStack", 
                  desc = paste0("One or more RasterLayers or RasterStacks in which to look for ",
                                "variables present in the model formula.")),
    createsOutput(objectName = "dataFireSense_SpreadPredict", 
                  objectClass = "RasterStack", 
                  desc = paste0("One or more RasterLayers or RasterStacks in which to look for ",
                                "variables present in the model formula.")),
    createsOutput(objectName = "annualStacks", 
                  objectClass = "RasterStack", 
                  desc = paste0("Annual Stack of covariates ")),
    createsOutput(objectName = "nonAnnualStacks", 
                  objectClass = "RasterStack", 
                  desc = paste0("Annual Stack of covariates "))
  )
))



## event types
#   - type `init` is required for initialization

doEvent.fireSense_dataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # schedule future event(s)
      if ("fireSense_IgnitionFit" %in% P(sim)$whichModulesToPrepare)
        sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrep", "prepIgnitionFitData")
      if ("fireSense_EscapeFit" %in% P(sim)$whichModulesToPrepare)
        sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrep", "prepEscapeFitData")
      if ("fireSense_SpreadFit" %in% P(sim)$whichModulesToPrepare)
        sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrep", "prepSpreadFitData")
      if ("fireSense_IgnitionPredict" %in% P(sim)$whichModulesToPrepare)
        sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrep", "prepIgnitionPredictData")
      if ("fireSense_EscapePredict" %in% P(sim)$whichModulesToPrepare)
        sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrep", "prepEscapePredictData")
      if ("fireSense_SpreadPredict" %in% P(sim)$whichModulesToPrepare)
        sim <- scheduleEvent(sim, start(sim), "fireSense_dataPrep", "prepSpreadPredictData")
    },
    prepIgnitionFitData = {
      # TODO when we work with IgnitionFit
    },
    prepEscapeFitData = {
      # TODO when we work with EscapeFit
    },
    prepSpreadFitData = {
      # TODO when we work with SpreadFit --> Coming from the global !runMe.R
      # Go over the code down, pull out what needs to go back to Global and what needs to stay here 
      
      ######################## Create dataset for SpreadFit ########################
      
      # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ fireAttributesFireSense_SpreadFit
      # 
      # # Load these so I can use as rasterToMatch 
      # #TODO Wrap this up in a tryCatch + a redo statement
      # 
      # # THESE I NEED TO PASS FROM THE PREAMBLE OUTSIDE!! Need to add to inputs
      # # 2001
      cohortData2001 <- readRDS(file.path(Paths$inputPath, "cohortData2001_fireSense_year2001.rds"))
      pixelGroupMap2001 <- readRDS(file.path(Paths$inputPath, "pixelGroupMap2001_fireSense_year2001.rds"))
      # 
      # # 2011
      cohortData2011 <- readRDS(file.path(Paths$inputPath, "cohortData2011_fireSense_year2011.rds"))
      pixelGroupMap2011 <- readRDS(file.path(Paths$inputPath, "pixelGroupMap2011_fireSense_year2011.rds"))
      # 
      # # After getting the fire, I should get the weather (MDC)
      # # I downloaded the data manually using climateNA and placed in the /inputs folder
      source("functions/calculateMDC.R")
      # 
      fireYears <- 1991:2017
      names(fireYears) <- as.character(fireYears)
      # plan("multiprocess", workers = length(fireYears))
      if (!file.exists(file.path(Paths$inputPath, "MDC_1991_2017.rds"))){
        # This file NWT_3ArcMinuteM comes from downloading the specific data from ClimateNA.
        # While there isn't an API for it, this is a manual step. You will need the DEM for the area
        # and specify which variables you want (in our case, monthlt variables)
        MDC <- Cache(calculateMDC, pathInputs = file.path(Paths$inputPath[[1]], "NWT_3ArcMinuteM"),
                     years = c(fireYears), doughtMonths = 4:9, rasterToMatch = pixelGroupMap2001,
                     userTags = c("MDC_1991_2017", "normals_MDC"))

        saveRDS(MDC, file.path(Paths$inputPath[[1]], "MDC_1991_2017.rds"))
      } else {
        MDC <- readRDS(file.path(Paths$inputPath, "MDC_1991_2017.rds"))
      }
      # 
      # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ dataFireSense_SpreadFit
      # 
      # # RasterStack of the model covariates. 
      # # The number of layers in the RasterStack should equal the number of distinct dates in column 'date'.
      # # Each COVARIATE of the model is ONE different raster stack containing that variable for all the different years.
      # # TODO: To include a bit more stochasticity, simulate vegetation changes from 2001 until 2011, and from 2011 until 2017 using 2001 and 2011 layers, respectively and the fire dataset (I know exactly where the fires occurred and how big they were! I just need to call them in a module each year). At each each I save cohortData and pixelGroupMap, and using rasterizeReduce, I create the proportional biomass layers for each year for each class (would have to reclassify the species).
      # 
      # # For now, however, we will assume the landscape and proportions of species don't change for @ 15 years, and we use all the dataset 1991-2017 to fit the spread model, using 2001 and 2011 layers
      # # 1991:2004 --> 2001: cohortData2001
      # # 2005:2017 --> 2011: cohortData2011
      # 
      # # Create the classification. Repeat with 2011
      # source(file.path(getwd(), 'functions/classifyCohortsFireSenseSpread.R')) # 04MAY20, TM: This is still the same version in fS_spreadDataPrep. 
      # #But needs to be: 1) made flexible; 2) put in fireSenseUtils
      # 
      classList2001 <- classifyCohortsFireSenseSpread(cohortData2001,
                                                      year = 2001,
                                                      pixelGroupMap = pixelGroupMap2001,
                                                      flammable = flammableRTM)
      classList2011 <- classifyCohortsFireSenseSpread(cohortData2011,
                                                      year = 2011,
                                                      pixelGroupMap = pixelGroupMap2011,
                                                      flammable = flammableRTM)
      # 
      # # Assign values from 2001 and 2011 veg input layers to annual data
      # yearToDivide <- 2005
      # 
      classList <- list(classList2001, classList2011)
      names(classList) <- c(paste0(fireYears[fireYears < yearToDivide], collapse = "_"),
                            paste0(fireYears[fireYears >= yearToDivide], collapse = "_"))

      # pull to memory
      stackToMemory <- function (x, ...){
        r <- stack(x, ...)
        r <- setValues(r, getValues(r))
        return(r)
      }

      weather <- Cache(stackToMemory, MDC)
      weather <- raster::unstack(weather)
      names(weather) <- as.character(fireYears)

      # weave all covariates together
      annualRasters <- mapply(c, weather = weather, SIMPLIFY=FALSE)
      sim$annualStacks <- lapply(annualRasters, raster::stack)
      rm(annualRasters)

      nonAnnualRasters <- mapply(c, classList, SIMPLIFY=FALSE)
      sim$nonAnnualStacks <- lapply(nonAnnualRasters, raster::stack)
      rm(nonAnnualRasters)

      ######################################
    },
    prepIgnitionPredictData = {
      # TODO when we work with IgnitionPredict --> coming from fireSense_NWT_dataPrep, which will be extinguished
    },
    prepEscapePredictData = {
      # TODO when we work with EscapePredict --> coming from fireSense_NWT_dataPrep, which will be extinguished
    },
    prepSpreadPredictData = {
    sim$dataFireSense_SpreadPredict <- raster::stack(Cache(classifyCohortsFireSenseSpread, 
                                                           cohortData = sim$cohortData,
                                                           year = time(sim),
                                                           pixelGroupMap = sim$pixelGroupMap,
                                                           flammable = sim$flammableRTM))
    # We need to: 
    # 1) Add zeros to where we don't have proportions (currently NA)
    sim$dataFireSense_SpreadPredict <- stack(lapply(names(sim$dataFireSense_SpreadPredict), 
                                                    function(rasName){
      ras <- sim$dataFireSense_SpreadPredict[[rasName]]
      ras[rasterToMatch[] == 1 & is.na(ras[])] <- 0 
      return(ras)
    })
    )
    # 2) Assert that all proportions sum to 1
    summedRas <- sum(sim$dataFireSense_SpreadPredict)
    tb <- table(summedRas[])
    testthat::expect_equal(sort(as.numeric(names(tb))), c(0, 1))

      if (is.null(sim$usrEmail))
        warning(paste0("If in a non-interactive session, please make sure you supply the object",
                       " `usrEmail` for google authentication"))
      
      MDC06 <- Cache(usefun::prepareClimateLayers, authEmail = sim$usrEmail,
                                                pathInputs = inputPath(sim), 
                                                studyArea = sim$studyArea,
                                                rasterToMatch = sim$rasterToMatch, 
                                                years = time(sim),
                                                variables = "fireSense", model = "fireSense",
                                                returnCalculatedLayersForFireSense = TRUE,
                                                RCP = P(sim)$RCP,
                                                climateModel = P(sim)$climateModel,
                                                ensemble = P(sim)$ensemble, 
                                                climateFilePath = P(sim)$climateFilePath,
                                                fileResolution = P(sim)$climateResolution)
      MDC06 <- MDC06[[paste0("year", time(sim))]]
      # MDC06 is weather!
      names(MDC06) <- "weather"
      sim$dataFireSense_SpreadPredict <- stack(sim$dataFireSense_SpreadPredict, MDC06)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere("usrEmail", sim)){
    sim$usrEmail <- if (pemisc::user() %in% c("tmichele", "Tati")) "tati.micheletti@gmail.com" else NULL
  }
  
  if (!suppliedElsewhere(object = "studyArea", sim = sim)){
    sim$studyArea <- Cache(prepInputs,
                           url = extractURL("studyArea"),
                           destinationPath = dataPath(sim),
                           cloudFolderID = sim$cloudFolderID,
                           omitArgs = c("destinationPath", "cloudFolderID"))
  }
  
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)){
    sim$rasterToMatch <- Cache(prepInputs, url = extractURL("rasterToMatch"), 
                               studyArea = sim$studyArea,
                               targetFile = "RTM.tif", destinationPath = dataPath(sim), 
                               filename2 = NULL,
                               omitArgs = c("destinationPath", "cloudFolderID", 
                                            "useCloud", "overwrite", "filename2"))
  }

  return(invisible(sim))
}
