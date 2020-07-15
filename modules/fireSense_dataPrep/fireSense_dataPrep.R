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
  reqdPkgs = list("raster", 
                  "PredictiveEcology/LandR@development"),
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
                                 "climate variables for all years of simulation")),
    defineParameter(name = "train", class = "logical", default = TRUE,
                    desc = "train or predict mode. Defaults is TRUE, or train mode.")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "cohortData2001", objectClass = "data.table", 
                 desc = paste0("Table that defines the cohorts by pixelGroup in 2001"), 
                 sourceURL = NA), 
    expectsInput(objectName = "pixelGroupMap2001", objectClass = "RasterLayer", 
                 desc = paste0("RasterLayer that defines the pixelGroups for cohortData table ",
                               "in 2001"), 
                 sourceURL = NA), 
    expectsInput(objectName = "cohortData2011", objectClass = "data.table", 
                 desc = paste0("Table that defines the cohorts by pixelGroup in 2011"), 
                 sourceURL = NA), 
    expectsInput(objectName = "pixelGroupMap2011", objectClass = "RasterLayer", 
                 desc = paste0("RasterLayer that defines the pixelGroups for cohortData table ",
                               "in 2011"), 
                 sourceURL = NA), 
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
                 desc = "RTM without ice/rocks/urban/water. Flammable map with 0 and 1."),
    expectsInput(objectName = "rstLCC",
                 objectClass = "RasterLayer",
                 sourceURL = NA,
                 desc = "Raster of land cover. Defaults to LCC05.")
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
    createsOutput(objectName = "MDC06", 
                  objectClass = "RasterStack", 
                  desc = paste0("Calculated MDC for June ",
                                "To be used by ignition, escape(?) and spread predictions.")),
    expectsInput(objectName = "rstLCC",
                 objectClass = "RasterLayer",
                 sourceURL = NA,
                 desc = "Raster of land cover. Defaults to LCC05.")
    )
))

doEvent.fireSense_dataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # 
      # REVISE THE CODE IN THE INIT. CAME FROM fireSense_NWT_dataPrep and
      # needs to be revised!
      # wetLCC code for Water 1
      # wetLCC code for Wetlands 2
      # wetLCC code for Uplands 3
      
      message("Reclassifying water in LCC05...")
      testthat::expect_false(is.null(sim$rstLCC), 
                             label = "rstLCC is still NULL. Please debug .inputObjects") # Assertion
      
      # We are modifying these layers in the simList so we get this propagated accross all modules
      # Improving the water and wetlands locations
      sim$rstLCC[sim$wetLCC == 1] <- 37 # LCC05 code for Water bodies
      sim$rstLCC[sim$wetLCC == 2] <- 19 # LCC05 code for Wetlands
      
      if (P(sim)$train){
        message("train is TRUE, preparing RTM. This should happen only if dataFireSense_EscapeFit 
            \nand dataFireSense_FrequencyFit are not being passed.")
        mod[["RTM"]] <- Cache(
          aggregate,
          sim[["rstLCC"]],
          fact = P(sim)$res / xres(sim[["rstLCC"]]),
          fun = function(x, ...) if (anyNA(x)) NA else 1
        )
        
        mod[["PX_ID"]] <- tibble(PX_ID = which(!is.na(mod[["RTM"]][])))
        
        mod[["RTM_VT"]] <- bind_cols(
          st_as_sf(rasterToPolygons(mod[["RTM"]])),
          mod[["PX_ID"]]
        )
      }
      # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
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
      
      if (P(sim)$train){ # From fireSense_NWT_DataPrep. NEEDS REVISION!!!
        
        sim <- PrepThisYearMDC(sim) 
        sim <- PrepThisYearFire(sim)
        
      } # Fire and MDC only get prepped when train == TRUE, while LCC gets prepped every time the module `fireSense_NWT_DataPrep runs`
      
    },
    prepEscapeFitData = {
      # TODO when we work with EscapeFit
    },
    prepSpreadFitData = {
      # TODO when we work with SpreadFit --> Coming from the global !runMe.R
      # Go over the code down, pull out what needs to go back to Global and what needs to stay here 
      
      # source(file.path(getwd(), "modules/fireSense_dataPrep/R/calculateMDC.R")) 
      # # Needs to go to fireSenseUtils
      fireYears <- 1991:2017
      names(fireYears) <- as.character(fireYears)
      # plan("multiprocess", workers = length(fireYears))

      if (!file.exists(file.path(Paths$inputPath, "MDC_1991_2017.rds"))){
        # This file NWT_3ArcMinuteM comes from downloading the specific data from ClimateNA.
        # While there isn't an API for it, this is a manual step. You will need the DEM for the area
        # and specify which variables you want (in our case, monthly variables)
        MDC <- Cache(calculateMDC, pathInputs = file.path(Paths$inputPath),
                     years = c(fireYears), doughtMonths = 4:9, rasterToMatch = pixelGroupMap2001,
                     userTags = c("MDC_1991_2017", "normals_MDC"))

        saveRDS(MDC, file.path(Paths$inputPath, "MDC_1991_2017.rds"))
      } else {
        MDC <- readRDS(file.path(Paths$inputPath, "MDC_1991_2017.rds"))
        # Fixing Caching and moving rasters problem
        if (!file.exists(filename(MDC$Year1991$MDC_1991$MDC_1991))) 
          MDC <- lapply(MDC, function(yr) {
            r <- yr[[1]]
            r@file@name <- gsub("^.*(MDC_.*)$", file.path(Paths$inputPath, "\\1"), 
                                yr[[1]][[1]]@file@name)
            return(r)
          })
      }
      
      # For now, we will assume the landscape and proportions of species don't change for @ 15 years
      # and we use all the dataset 1991-2017 to fit the spread model, using 2001 and 2011 layers
      # 1991:2004 --> 2001: cohortData2001
      # 2005:2017 --> 2011: cohortData2011
      # Eventually, these can come from LandR
      # 
      # Create the classification. Repeat with 2011
      # source(file.path(getwd(), "modules/fireSense_dataPrep/R/classifyCohortsFireSenseSpread.R')) 
      # Needs to be: 1) made flexible; 2) put in fireSenseUtils
      classList2001 <- classifyCohortsFireSenseSpread(cohortData2001,
                                                      year = 2001,
                                                      pixelGroupMap = pixelGroupMap2001,
                                                      flammable = flammableRTM)
      classList2011 <- classifyCohortsFireSenseSpread(cohortData2011,
                                                      year = 2011,
                                                      pixelGroupMap = pixelGroupMap2011,
                                                      flammable = flammableRTM)
      
      # Assign values from 2001 and 2011 veg input layers to annual data
      yearToDivide <- 2005
       
      classList <- list(classList2001, classList2011)
      names(classList) <- c(paste0(fireYears[fireYears < yearToDivide], collapse = "_"),
                            paste0(fireYears[fireYears >= yearToDivide], collapse = "_"))

      weather <- stackToMemory(MDC)
      weather <- raster::unstack(weather)
      names(weather) <- as.character(fireYears)

      # weave all covariates together
      annualRasters <- mapply(c, weather = weather, SIMPLIFY = FALSE)
      annualStacks <- lapply(annualRasters, raster::stack)
      rm(annualRasters)
      nonAnnualRasters <- mapply(c, classList, SIMPLIFY = FALSE)
      nonAnnualStacks <- lapply(nonAnnualRasters, raster::stack)
      rm(nonAnnualRasters)
      sim$dataFireSense_SpreadFit <-list(annualStacks = annualStacks, 
                                         nonAnnualStacks = nonAnnualStacks) 
      
      ######################################
    },
    prepIgnitionPredictData = {
      # TODO when we work with IgnitionPredict --> coming from fireSense_NWT_dataPrep, which will be extinguished

      if (is.null(sim$usrEmail))
        warning(paste0("If in a non-interactive session, please make sure you supply the object",
                       " `usrEmail` for google authentication"))
      
      prepClimLays <- FALSE
      if (is.null(sim$MDC06)){
        prepClimLays <- TRUE
      } else {
        if (attr(sim$MDC06, "YEAR") != time(sim)){
          prepClimLays <- TRUE
        }
      }
      if (prepClimLays){
        sim$MDC06 <- usefulFuns::prepareClimateLayers(authEmail = sim$usrEmail,
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
        sim$MDC06 <- sim$MDC06[[paste0("year", time(sim))]]
        attributes(sim$MDC06)$YEAR <- time(sim)
      }
        preparedLCC <- PrepThisYearLCC(currentTime = time(sim, "year"),
                                       firePolys = sim$firePolys,
                                       rstLCC = sim$rstLCC,
                                       train = P(sim)$train,
                                       res = P(sim)$res) # training only)
        # Happens for training only
        sim$pp_lcc <- preparedLCC$pp_lcc
        # Updates the rstLCC with real burned data
        sim$rstLCC <- preparedLCC$rstLCC 
        
        if (P(sim)$train) {
          # Prepare input data for the fireSense_FrequencyFit module

          browser() # Understand what the heck is going on down here. 
          # This only happens in training... so in theory I don't need this to predict
          sim[["dataFireSense_FrequencyFit"]] <- bind_rows(
            sim[["dataFireSense_FrequencyFit"]],
            bind_cols(
              mod[["fires"]] %>%
                group_by(PX_ID, YEAR) %>%
                summarise(n_fires = n()) %>%
                ungroup %>%
                right_join(mod[["PX_ID"]], by = "PX_ID") %>%
                mutate(YEAR = time(sim, "year"), n_fires = ifelse(is.na(n_fires), 0, n_fires)),
              rename(
                as_tibble(mod[["MDC"]][mod[["PX_ID"]][["PX_ID"]]]),
                MDC04 = 1,
                MDC05 = 2,
                MDC06 = 3,
                MDC07 = 4,
                MDC08 = 5,
                MDC09 = 6
              ) %>% dplyr::select(MDC06),
              mod[["pp_lcc"]]
            )
          )
          
          #
          ## Filter out pixels where at least one variable evaluates to 0
          #
          ### Filter out pixels where none of the classes of interest are present
          #
          sim[["dataFireSense_FrequencyFit"]] %<>%
            dplyr::filter(
              (
                dplyr::select(., paste0("cl", c(1:32, 34:35))) %>% # 33, 36:39 do not burn. No need to estimate coefficients, it's 0.
                  rowSums()
              ) != 0
            )
          
          #
          ### Filter out pixels where MDC06 is > 0
          #
          sim[["dataFireSense_FrequencyFit"]] %<>% dplyr::filter(MDC06 > 0)
          
          #
          # Prepare input data for the fireSense_EscapeFit module
          #  
          fire_escape_data <- mod[["fires"]] %>%
            group_by(PX_ID, YEAR) %>%
            summarise(n_fires = n(), escaped = sum(SIZE_HA > 1)) %>%
            ungroup
          
          sim[["dataFireSense_EscapeFit"]] <- bind_rows(
            sim[["dataFireSense_EscapeFit"]],
            bind_cols(
              fire_escape_data,
              rename(
                as_tibble(mod[["MDC"]][fire_escape_data[["PX_ID"]]]),
                MDC04 = 1,
                MDC05 = 2,
                MDC06 = 3,
                MDC07 = 4,
                MDC08 = 5,
                MDC09 = 6
              ) %>% dplyr::select(MDC06),
              dplyr::filter(mod[["pp_lcc"]], mod[["PX_ID"]][["PX_ID"]] %in% fire_escape_data[["PX_ID"]])
            )
          )
        } else {
          if (!is.null(sim$MDC06)){
            names(sim$MDC06) <- "MDC06"
          } else stop("MDC06 is NULL. Debug fireSense_dataPrep.")
        }
        
    },
    prepEscapePredictData = {
      # TODO when we work with EscapePredict --> coming from fireSense_NWT_dataPrep, which will be extinguished
    },
    prepSpreadPredictData = {
    currentCohortData  <- copy(sim$cohortData)
    sim$dataFireSense_SpreadPredict <- raster::stack(classifyCohortsFireSenseSpread(
                                                            cohortData = currentCohortData,
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
    
      prepClimLays <- FALSE
      if (is.null(sim$MDC06)){
        prepClimLays <- TRUE
      } else {
        if (attr(sim$MDC06, "YEAR") != time(sim)){
          prepClimLays <- TRUE
        }
      }
      if (prepClimLays){
        sim$MDC06 <- usefulFuns::prepareClimateLayers(authEmail = sim$usrEmail,
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
        sim$MDC06 <- sim$MDC06[[paste0("year", time(sim))]]
        attributes(sim$MDC06)$YEAR <- time(sim)
        names(sim$MDC06) <- "MDC06"
      }
      # MDC06 is weather!
      MDC06 <- sim$MDC06 # Need a local MDC as it needs to be called weather for spread
      names(MDC06) <- "weather"
      sim$dataFireSense_SpreadPredict <- stack(sim$dataFireSense_SpreadPredict, MDC06)
      
      # Reschedule data prep
      sim <- scheduleEvent(sim, time(sim) + 1, "fireSense_dataPrep", "prepSpreadPredictData")
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", inputPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere("usrEmail", sim)){
    sim$usrEmail <- if (pemisc::user() %in% c("tmichele", "Tati")) "tati.micheletti@gmail.com" else NULL
  }
  
  if (!suppliedElsewhere(object = "studyArea", sim = sim)){
    sim$studyArea <- Cache(prepInputs,
                           url = extractURL("studyArea"),
                           destinationPath = inputPath(sim),
                           cloudFolderID = sim$cloudFolderID,
                           omitArgs = c("destinationPath", "cloudFolderID"))
  }
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)){
    sim$rasterToMatch <- Cache(prepInputs, url = extractURL("rasterToMatch"), 
                               studyArea = sim$studyArea,
                               targetFile = "RTM.tif", 
                               destinationPath = inputPath(sim), 
                               filename2 = NULL,
                               omitArgs = c("destinationPath", "cloudFolderID", 
                                            "useCloud", "overwrite", "filename2"))
  }
  if (any(!suppliedElsewhere(object = "cohortData2001", sim = sim),
          !suppliedElsewhere(object = "pixelGroupMap2001", sim = sim))){
    stop("Either cohortData2001 or pixelGroupMap2001 was not supplied. Consider running the Biomass_borealDataPrep 
for 2001 KNN layers")
  }
  if (any(!suppliedElsewhere(object = "cohortData2011", sim = sim),
          !suppliedElsewhere(object = "pixelGroupMap2011", sim = sim))){
    stop("Either cohortData2011 or pixelGroupMap2011 was not supplied. Consider running the Biomass_borealDataPrep 
for 2011 KNN layers")
  }
  
  if (!suppliedElsewhere(object = "firePolys", sim = sim))
  {
    sim$firePolys <- Cache(
      prepInputs, 
      archive = "NFDB_poly.zip",
      alsoExtract = "similar",
      url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip",
      fun = "sf::st_read",
      destinationPath = inputPath(sim),
      studyArea = sim$studyArea,
      useSAcrs = TRUE,
      filename2 = NULL,
      userTags = c("module:fireSense_NWT_DataPrep",
                   "objectName:NFDB_PO"))
  }
  
  if (!suppliedElsewhere(object = "firePoints", sim = sim))
  {
    sim$firePoints <- Cache(
      prepInputs, 
      url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip",
      fun = "sf::st_read",
      destinationPath = inputPath(sim),
      archive = "NFDB_point.zip",
      alsoExtract = "similar",
      omitArgs = "destinationPath",
      studyArea = sim$studyArea,
      useSAcrs = TRUE,
      filename2 = NULL,
      userTags = c("module:fireSense_NWT_DataPrep",
                   "objectName:firePoints")
    )
  }
  
  if (!suppliedElsewhere("wetLCC", sim)){
    message("wetLCC not supplied. Loading water layer for the NWT...")
    sim$wetLCC <- prepInputs(destinationPath = inputPath(sim), # Or another directory.
                             omitArgs = "destinationPath",
                             url = "https://drive.google.com/file/d/1YVTcIexNk-obATw2ahrgxA6uvIlr-6xm/view",
                             targetFile = "wetlandsNWT250m.tif",
                             rasterToMatch = sim[["rasterToMatch"]],
                             maskWithRTM = TRUE,
                             filename2 = NULL,
                             userTags = c("module:fireSense_NWT_DataPrep",
                                          "objectName:wetLCC")
    )
  }
  
  if (!suppliedElsewhere("rstLCC", sim)){
    message("rstLCC not supplied. Loading LCC05")
    sim$rstLCC <- Cache(prepInputs, url = paste0("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/",
                                                 "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
                        targetFile = file.path(Paths$inputPath, "LCC2005_V1_4a.tif"),
                        archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
                        destinationPath = Paths$inputPath,
                        studyArea = studyArea,
                        rasterToMatch = rasterToMatch,
                        maskWithRTM = TRUE,
                        method = "bilinear",
                        datatype = "INT2U",
                        filename2 = TRUE,
                        userTags = c(stepCacheTag,
                                     "objectName:rstLCC", "module:fireSense_dataPrep",
                                     "outFun:Cache"),
                        omitArgs = c("destinationPath", "filename2"))
  }
  

  return(invisible(sim))
}
