# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "fireSense_NWT_DataPrep",
  description = "Prepare climate and vegetation data needed to run the fireSense modules for BCR6 and BCR6 contained in the Northwest Territories.", #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.4", fireSense_NWT_DataPrep = "1.0.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_NWT_DataPrep.Rmd"),
  reqdPkgs = list("dplyr", "magrittr", "raster", "rlang", "sf", "tibble"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(name = "res", class = "numeric", default = 10000,
                    desc = "at which resolution should we aggregate the data? By
                            default, 10km."),
    defineParameter(name = "train", class = "logical", default = TRUE,
                    desc = "train or predict mode. Defaults is TRUE, or train mode."),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start 
                            time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = 1, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in years. By default, every year."),
    defineParameter(name = ".useCache", class = "logical", default = FALSE, 
                    desc = "Should this entire module be run with caching 
                            activated? This is generally intended for data-type
                            modules, where stochasticity and time are not relevant"),
    defineParameter(name = "RCP", class = "character", default = "85", min = NA, max = NA, 
                    desc = "Which RCP should be used? Default to 85"),
    defineParameter(name = "climateModel", class = "character", default = "CCSM4", min = NA, max = NA, 
                    desc = "Which climate model should be used? Default to CCSM4"),
    defineParameter(name = "ensemble", class = "character", default = "CCSM4", min = NA, max = NA, 
                    desc = "Which ensemble model should be used? Default to ''. CCSM4 doesn't have ensemble, just CanESM2 (r11i1p1)"),
    defineParameter(name = "climateResolution", class = "character", default = "3ArcMin", min = NA, max = NA, 
                    desc = "Which DEM resolution was used for generating the climate layers? Default to '3ArcMin'."),
    defineParameter(name = "climateFilePath", class = "character", default = "https://drive.google.com/open?id=17idhQ_g43vGUQfT-n2gLVvlp0X9vo-R8", min = NA, max = NA, 
                    desc = "URL to zipped climate file coming from ClimateNA, containing all climate variables for all years of simulation")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(
      objectName = "cloudFolderID",
      objectClass = "character",
      sourceURL = NA_character_,
      desc = "GDrive folder ID for cloud caching."
    ),
    expectsInput(
      objectName = "LCC05",
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/open?id=1ziUPnFZMamA5Yi6Hhex9aZKerXLpVxvz",
      desc = "Land Cover Map of Canada 2005 (LCC05)."
    ),
    expectsInput(
      objectName = "vegMap",
      objectClass = "RasterLayer",
      sourceURL = NA,
      desc = "Land Cover Map of Canada 2005 (LCC05) cropped."
    ),
    expectsInput(
      objectName = "MDC_BCR6_NWT_250m",
      objectClass = "RasterStack",
      sourceURL = NA_character_,
      desc = "Monthly Drought Code (April to September) within BCR6 as contained in the Northwest Territories."
    ),
    expectsInput(
      objectName = "NFDB_PO",
      objectClass = "sf",
      sourceURL = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip",
      desc = "National Fire DataBase polygon data (NFDB_PO)."
    ),
    expectsInput(
      objectName = "NFDB_PT",
      objectClass = "sf",
      sourceURL = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip",
      desc = "National Fire DataBase point data (NFDB_PT)."
    ),
    expectsInput(
      objectName = "rasterToMatch", 
      objectClass = "RasterLayer",
      sourceURL = "https://drive.google.com/open?id=1NIjFbkckG3sewkTqPGaBGQDQLboPQ0wc",
      desc = "a template raster describing the studyArea"
    ),
    expectsInput(
      objectName = "studyArea", 
      objectClass = "SpatialPolygonsDataFrame",
      sourceURL = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU",
      desc = "a template polygon describing the studyArea"
    ),
    expectsInput( # ~ TM added on 11AUG19 --> Not defining it was causing it to be NULL in simList
      objectName = "MDC06",
      objectClass = "RasterLayer",
      sourceURL = NA,
      desc = "Rasterlayer describing the Monthly Drougth Code of June for the current year."
    ), 
    expectsInput( # ~ TM added on 11AUG19 --> Not defining it was causing it to be NULL in simList
      objectName = "wetLCC",
      objectClass = "RasterLayer",
      sourceURL = NA,
      desc = "Rasterlayer with 3 values, generated from DUCKS unlimited, showing water = 1, wetlands = 2, and uplands = 3."
    ),
    expectsInput( # ~ TM added on 04DEC19 --> Not defining it was causing it to be NULL in simList
      objectName = "usrEmail",
      objectClass = "character",
      sourceURL = NA,
      desc = "User e.mail for GDrive authorization"
    )
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(
      objectName = "dataFireSense_EscapeFit", 
      objectClass = "data.frame", 
      desc = "Contains MDC, land-cover, fire data necessary to train the fireSense_EscapeFit SpaDES module for BCR6 as contained in the Northwest Territories."
    ),
    createsOutput(
      objectName = "dataFireSense_FrequencyFit", 
      objectClass = "data.frame", 
      desc = "Contains MDC, land-cover, fire data necessary to train the fireSense_FrequencyFit SpaDES module for BCR6 as contained in the Northwest Territories."
    ),
    createsOutput(
      objectName = "LCC", 
      objectClass = "RasterStack", 
      desc = "Contains LCC classes. Necessary to predict with fireSense for BCR6 as contained in the Northwest Territories."
    ),
    createsOutput(
      objectName = "MDC06", 
      objectClass = "RasterLayer", 
      desc = "Contains MDC06 for the current year. Necessary to predict with fireSense for BCR6 as contained in the Northwest Territories."
    )
  )
))

## event types
#   - type `init` is required for initialization

doEvent.fireSense_NWT_DataPrep = function(sim, eventTime, eventType) 
{
  switch(
    eventType,
    init = { 
      sim <- Init(sim)
      sim <- Run(sim) # Hack of the Friday afternoon --'
      sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime + 1, "fireSense_NWT_DataPrep", "run")
    },
    run = { 
      sim <- Run(sim) 
      
      if (!is.na(P(sim)$.runInterval))
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, "fireSense_NWT_DataPrep", "run")
    },
    warning(
      paste(
        "Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
        "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""
      )
    )
  )
  
  invisible(sim)
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) 
{
  
  # smallSR <- shapefile("~/Desktop/smallSR.shp")
  # names(smallSR) <- "PolyID"
  # 
  # sim[["vegMap"]] <- postProcess(
  #   sim[["vegMap"]], 
  #   studyArea = smallSR,
  #   filename2 = NULL
  # )
  
  # sim[["NFDB_PO"]] <- as(
  #   postProcess(
  #     as_Spatial(sim[["NFDB_PO"]]), 
  #     studyArea = smallSR,
  #     filename2 = NULL
  #   ),
  #   "sf"
  # )
  
  # sim[["MDC06"]] <- postProcess(
  #   sim[["MDC06"]],
  #   studyArea = smallSR,
  #   filename2 = NULL
  # )
  
  # wetLCC code for Water 1
  # wetLCC code for Wetlands 2
  # wetLCC code for Uplands 3

message("Reclassifying water in LCC05...")
  mod[["vegMap"]] <- sim[["vegMap"]]
  if (is.null(sim[["vegMap"]]))
    stop("vegMap is still NULL. Please debug .inputObjects")
  mod[["vegMap"]][sim$wetLCC == 1] <- 37 # LCC05 code for Water bodies
  mod[["vegMap"]][sim$wetLCC == 2] <- 19 # LCC05 code for Wetlands
  
  if (P(sim)$train){
    message("train is TRUE, preparing RTM. This should happen only if dataFireSense_EscapeFit \nand dataFireSense_FrequencyFit are not being passed.")
    mod[["RTM"]] <- Cache(
      aggregate,
      sim[["vegMap"]],
      fact = P(sim)$res / xres(sim[["vegMap"]]),
      fun = function(x, ...) if (anyNA(x)) NA else 1
    )
    
    mod[["PX_ID"]] <- tibble(PX_ID = which(!is.na(mod[["RTM"]][])))
    
    mod[["RTM_VT"]] <- bind_cols(
      st_as_sf(rasterToPolygons(mod[["RTM"]])),
      mod[["PX_ID"]]
    )
  }
  
  invisible(sim)
}

PrepThisYearMDC <- function(sim)
{

  mod[["MDC"]] <- 
    raster::stack(
      Cache(
    lapply, 
      raster::unstack(sim[["MDC_BCR6_NWT_250m"]]),
      postProcess,
      rasterToMatch = mod$RTM,
      destinationPath = tempdir(),
      omitArgs = "destinationPath",
      maskWithRTM = TRUE,
      method = "bilinear",
      datatype = "FLT4S",
      filename2 = NULL
    )
  )
  
  return(invisible(sim))
}

PrepThisYearLCC <- function(sim)
{
  year <- time(sim, "year")
  
  #
  # LCC05 with incremental disturbances
  #
  fires_this_year <- sim[["NFDB_PO"]] %>%
    dplyr::filter(YEAR > (year - 15) & YEAR <= year)
  
  if (nrow(fires_this_year) > 0)
  {
    # Setting the burned pixels of LCC05 to category 34 (recent burns)
    spatialUnified <- as(st_union(fires_this_year),"Spatial")
    spatialDF <- SpatialPolygonsDataFrame(Sr = spatialUnified, data = data.frame(ID = 1), match.ID = FALSE)
    sfDF <- st_as_sf(spatialDF)
    rasDF <- fasterize::fasterize(sf = sfDF, raster = mod[["vegMap"]])
    mod[["vegMap"]][rasDF == 1] <- 34  # LCC05 code for recent burns

    # This was way too slow and was failing for some reason...    
    # Cache(
    #   # cloudFolderID = sim[["cloudFolderID"]],
    #   `[<-`,
    #   x = mod[["vegMap"]],
    #   i = {
    #     # Calculate proportion of recently disturbed areas for each pixel of LCC05
    #     Cache(
    #       # cloudFolderID = sim[["cloudFolderID"]],
    #       rasterize,
    #       x = SpatialPolygonsDataFrame(
    #         as(
    #           st_union(
    #             fires_this_year
    #           ),
    #           "Spatial"
    #         ),
    #         data = data.frame(ID = 1),
    #         match.ID = FALSE
    #       ),
    #       y = mod[["vegMap"]],
    #       getCover = TRUE
    #     )[] >= .5
    #   },
    #   value = 34 # LCC05 code for recent burns
    # )
  }
  
  if (P(sim)$train)
  {
    n_lcc <- 39
    
    mod$pp_lcc <-
      lapply(
        1:n_lcc,
        function(cl_i)
        {
          calc_prop_lcc <- function(x, cl = cl_i, na.rm = TRUE)
          {
            if (anyNA(x)) return(NA)
            sum(x == cl, na.rm = na.rm) / (agg_fact ** 2)
          }
          
          col_name <- paste0("cl", cl_i)
          agg_fact <- P(sim)$res / xres(mod[["vegMap"]])
          
          tibble(
            !!col_name := aggregate(
              mod[["vegMap"]],
              fact = agg_fact,
              fun = calc_prop_lcc
            )[]
          )
        }
      ) %>% bind_cols %>% filter_at(2, all_vars(!is.na(.)))
  }
  else
  { # This happens for predicting
    sim[["LCC"]] <- setNames(
        raster::stack(lapply(c(1:32, 34:35), function(x) mod[["vegMap"]] == x)),
      nm = paste0("cl", c(1:32, 34:35))
    )
  }
  
  invisible(sim)
}

PrepThisYearFire <- function(sim)
{
  currentYear <- time(sim, "year")
  
  NFDB_PT <- sim[["NFDB_PT"]] %>%
    
    # Filter fire data for the current year
    dplyr::filter(YEAR == currentYear) %>%
    
    # Drop columns containing info we don't need
    dplyr::select(LATITUDE, LONGITUDE, YEAR, SIZE_HA, CAUSE) %>%
    
    # Keep only lightning fires
    dplyr::filter(CAUSE == "L")
  
  mod[["fires"]] <- st_set_geometry(
    mutate(
      filter(
        st_join(mod[["RTM_VT"]], NFDB_PT),
        !is.na(YEAR)
      ),
      YEAR = currentYear
    ), 
    NULL
  )
  
  invisible(sim)
}

Run <- function(sim){
  if (P(sim)$train)
  {
    sim <- PrepThisYearMDC(sim) 
    sim <- PrepThisYearFire(sim)
  } # Fire and MDC only get prepped when train == TRUE, while LCC gets prepped every time the module `fireSense_NWT_DataPrep runs`
  if (is.null(sim$usrEmail))
    warning("If in a non-interactive session, please make sure you supply the object `usrEmail` for google authentication")

  sim$MDC06 <- usefun::prepareClimateLayers(authEmail = sim$usrEmail,
                                                         pathInputs = inputPath(sim), studyArea = sim$studyArea,
                                                         rasterToMatch = sim$rasterToMatch, years = time(sim),
                                                         variables = "fireSense", model = "fireSense",
                                                         returnCalculatedLayersForFireSense = TRUE,
					                                               RCP = P(sim)$RCP,
                                                         climateModel = P(sim)$climateModel,
                                                         ensemble = P(sim)$ensemble, 
				                                                 climateFilePath = P(sim)$climateFilePath,
				                                                 fileResolution = P(sim)$climateResolution)
  sim$MDC06 <- sim$MDC06[[paste0("year", time(sim))]]
  sim <- PrepThisYearLCC(sim)

  if (P(sim)$train)
  {
    # Prepare input data for the fireSense_FrequencyFit module
browser() # Understand what the heck is going on down here. This only happens in training... so in theory I don't need this to predict
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
  }
  else
  {
    if (!is.null(sim[["MDC06"]])){
      names(sim[["MDC06"]]) <- "MDC06" # If is.null(sim[["MDC06"]]), it errors. Coming from (MDC_NWT_DataPrep)! Wasn't defined.
    } else stop("MDC06 is NULL. Possibly a problem in MDC_NWT_DataPrep module")
  }
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
  
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim))
  {
    sim[["rasterToMatch"]] <- Cache(
      prepInputs, 
      url = "https://drive.google.com/open?id=1NIjFbkckG3sewkTqPGaBGQDQLboPQ0wc",      
      targetFile = "BCR6_NWT-2.tif",
      destinationPath = tempdir(),
      omitArgs = "destinationPath"
    )
  }
  
  
  if (!suppliedElsewhere(object = "studyArea", sim = sim))
  {
    sim[["studyArea"]] <- Cache(
      prepInputs,
      url = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU",
      destinationPath = tempdir(),
      omitArgs = "destinationPath"
    )
  }
  
  if (!suppliedElsewhere(object = "vegMap", sim = sim) | 
      is.null(sim[["vegMap"]]))
  {
    sim[["vegMap"]] <- LandR::prepInputsLCC(destinationPath = dataPath(sim),
                                            studyArea = sim$studyArea,
                                            rasterToMatch = sim$rasterToMatch)
  }
  
  if (!suppliedElsewhere(object = "NFDB_PO", sim = sim))
  {
    sim[["NFDB_PO"]] <- Cache(
      prepInputs,
      url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip",
      fun = "sf::st_read",
      destinationPath = tempdir(),
      omitArgs = "destinationPath",
      studyArea = sim[["studyArea"]],
      useSAcrs = TRUE,
      filename2 = NULL,
      overwrite = TRUE
    )
  }
  
  if (!suppliedElsewhere(object = "NFDB_PT", sim = sim))
  {
    sim[["NFDB_PT"]] <- Cache(
      prepInputs, 
      url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip",
      fun = "sf::st_read",
      destinationPath = tempdir(),
      omitArgs = "destinationPath",
      studyArea = sim[["studyArea"]],
      useSAcrs = TRUE,
      filename2 = NULL,
      overwrite = TRUE
    )
  }
  
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere("usrEmail", sim)){
    sim$usrEmail <- if (pemisc::user() %in% c("tmichele", "Tati")) "tati.micheletti@gmail.com" else NULL
  }
  
  if (!suppliedElsewhere("wetLCC", sim)){
    message("wetLCC not supplied. Loading water layer for the NWT...")
    sim$wetLCC <- prepInputs(destinationPath = tempdir(), # Or another directory.
                             omitArgs = "destinationPath",
                             url = "https://drive.google.com/file/d/1YVTcIexNk-obATw2ahrgxA6uvIlr-6xm/view",
                             targetFile = "wetlandsNWT250m.tif",
                             rasterToMatch = sim[["rasterToMatch"]],
                             maskWithRTM = TRUE,
                             filename2 = NULL
    )
  }
  
  
  
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
