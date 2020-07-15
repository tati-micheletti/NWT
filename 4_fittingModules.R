############################################
############################################
#     F i t t i n g     M o d u l e s      #  
############################################
############################################

# Here we fit the fire modules for fireSense.

stepCacheTag <- c(paste0("cache:4_fittingModules"), 
                  paste0("runName:", runName))

SpaDES.core::setPaths(cachePath = fittingCache,
                      outputPath = Paths$inputPath)
getPaths()

if (!exists("fitTheseFireSenseModels")) fitTheseFireSenseModels <- NULL # ignition, escape, spread

##################################### Ignition

if (is(runNamesList()[RunName == runName, fireSenseIgnitionFitted], "character")){
  tryCatch({
    fireSenseIgnitionFitted <- reproducible::preProcess(url = runNamesList()[RunName == runName, 
                                                                             fireSenseIgnitionFitted],
                                                        # Here, outputPath is == inputPath
                                                        destinationPath = Paths$inputPath, 
                                                        filename2 = NULL,
                                                        userTags = c("objectName:fireSenseIgnitionFitted",
                                                                     stepCacheTag),
                                                        omitArgs = c("destinationPath", "filename2"))
    fitIgnition <- FALSE
  }, error = function(e) fitIgnition <- TRUE)
  if (all(!is.null(fitTheseFireSenseModels),
          "ignition" %in% fitTheseFireSenseModels))
    fitIgnition <- TRUE
} else {
  fitIgnition <- TRUE
}
  if (isTRUE(fitIgnition)){

    outputsIgnitionFit <- data.frame(objectName = c("fireSense_FrequencyFitted"),
                                  saveTime = 2011)
    
    # TODO If this doesn't exist, I need to run fireSense_IgnitionFit and save the output
    # in Paths$outputPath as fireSense_FrequencyFitted.rds, AND upload to the folder
    
    library("googledrive")
    drive_upload(file.path(Paths$inputPath, "fireSense_FrequencyFitted_year2001.rds"), 
                 as_id(runNamesList()[RunName == runName, fireSenseFolder]))
    
  }

##################################### Escape
  
if (is(runNamesList()[RunName == runName, fireSenseEscapeFitted], "character")){
  tryCatch({
    fireSenseEscapeFitted <- reproducible::preProcess(url = runNamesList()[RunName == runName, 
                                                                           fireSenseEscapeFitted],
                                                      destinationPath = Paths$inputPath,
                                                      filename2 = NULL,
                                                      userTags = c("objectName:fireSenseEscapeFitted",
                                                                   stepCacheTag),
                                                      omitArgs = c("destinationPath", "filename2"))
    fitEscape <- FALSE
  }, error = function(e) fitEscape <- TRUE)
  if (all(!is.null(fitTheseFireSenseModels),
          "escape" %in% fitTheseFireSenseModels))
    fitEscape <- TRUE
} else {
  fitEscape <- TRUE
}
  if (isTRUE(fitEscape)){
    
    outputsEscapeFit <- data.frame(objectName = c("fireSense_EscapeFitted"),
                                     saveTime = 2011)
    
    # TODO If this doesn't exist, I need to run fireSense_EscapeFit and save the output
    # in Paths$cachePath as fireSense_FrequencyFitted.rds, AND upload to the folder
    
    library("googledrive")
    drive_upload(file.path(Paths$inputPath, "fireSense_EscapeFitted_year2001.rds"), 
                 as_id(runNamesList()[RunName == runName, fireSenseFolder]))
  }
  
##################################### Spread

if (is(runNamesList()[RunName == runName, fireSenseSpreadFitted], "character")){
  if (all(!is.null(fitTheseFireSenseModels),
          "spread" %in% fitTheseFireSenseModels)){
      tryCatch({
        fireSenseSpreadFitted <- reproducible::preProcess(url = runNamesList()[RunName == runName, 
                                                                              fireSenseSpreadFitted],
                                                         destinationPath = Paths$inputPath,
                                                         filename2 = NULL,
                                                         # purge = TRUE,
                                                         # useCache = "overwrite",
                                                         userTags = c("objectName:fireSenseSpreadFitted",
                                                                      stepCacheTag),
                                                         omitArgs = c("destinationPath", "filename2"))
      fitSpread <- FALSE
    }, error = function(e){
      fitSpread <- TRUE
    })

    if (!isTRUE(fitSpread)){
      tryCatch({
        covMinMax <- readRDS(file.path(Paths$inputPath, "covMinMax_year2011.rds"))
      }, error = function(e){
        fitSpread <- TRUE
      })
      if (!exists("covMinMax")) fitSpread <- TRUE 
    }
    if ("spread" %in% fitTheseFireSenseModels)
      fitSpread <- TRUE
  } else {
    fitSpread <- FALSE
  }
  if (isTRUE(fitSpread)){
    message("Fitting fireSense spread...")
    parametersFitting <- parameters
    parametersFitting$fireSense_dataPrep$whichModulesToPrepare <- "fireSense_SpreadFit"
    parametersFitting$fireSense_SpreadFit$visualizeDEoptim <- FALSE
    if (!exists("onlyLoadDEOptim")) onlyLoadDEOptim <- FALSE
    parametersFitting$fireSense_SpreadFit$onlyLoadDEOptim <- onlyLoadDEOptim
    outputsSpreadFit <- data.frame(objectName = c("fireSense_SpreadFitted",
                                                  "covMinMax"),
                                   saveTime = 2011)
    fireSenseSpreadFit <- simInitAndSpades(times = list(start = 2011, end = 2011),
                                params = parametersFitting,
                                modules = list("fireSense_dataPrep",
                                               "fireSense_SpreadFit"),
                                objects = objects,
                                paths = getPaths(),
                                loadOrder = c("fireSense_dataPrep", 
                                              "fireSense_SpreadFit"),
                                clearSimEnv = TRUE,
                                outputs = outputsSpreadFit,
                                userTags = c(stepCacheTag, 
                                             "objective:fireSenseSpreadFit", 
                                             "time:year2011"))
    
    library("googledrive")
    lapply(c(file.path(Paths$inputPath, "fireSense_SpreadFitted_year2011.rds"),
             file.path(Paths$inputPath, "covMinMax_year2011.rds")), 
           drive_upload, as_id(runNamesList()[RunName == runName, fireSenseFolder]), overwrite = TRUE)
  }
  }

##################################### LOAD FIRE INPUTS
  
  inputs <- data.frame(
    files = c(file.path(Paths$inputPath, "fireSense_IgnitionFitted_year2011.rds"),
              file.path(Paths$inputPath, "fireSense_EscapeFitted_year2011.rds"),
              file.path(Paths$inputPath, "fireSense_SpreadFitted_year2011.rds")),
    functions = "base::readRDS",
    stringsAsFactors = FALSE
  )

  # Update other objects for fireSense
  objects <- c(objects, list(
    "kNN_SpeciesCoverPc" = objects[["speciesLayers"]],
    "covMinMax" = readRDS(file.path(Paths$inputPath, "covMinMax_year2011.rds"))
  ))
  