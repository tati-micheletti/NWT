makeCMIandATA <- function(pathToNormalRasters,
                          pathToFutureRasters, 
                          rasterPrefix,
                          climateModel,
                          outputDir,
                          origTemplate = NULL, 
                          studyArea,
                          rasterToMatch,
                          years = 2011:2100){
  # Check for the existence of the climate files
  areClimateFuturesThere <- list.files(path = pathToFutureRasters, 
                                            pattern = "asc$",
                                            recursive = TRUE, 
                                            # Any ClimateNA raster will do; it's just to check the files
                                            full.names = TRUE)
  
  areClimateNormalsThere <- list.files(path = pathToNormalRasters, 
                                       pattern = "asc$",
                                       recursive = TRUE,
                                       full.names = TRUE)
  
  if (length(areClimateNormalsThere) == 0){
    # In future versions, it will download and unzip the Canadian Normals
    # TODO
    stop(paste0("Please download the normals, unzip, and place them here:", 
                pathToNormalRasters))
  }
  # Check for the future climate files!
  if (length(areClimateFuturesThere) == 0){ # Climate files are not there. Check the zip.
    climateFilePath <- getAnnualClimateZipURL(scenario = climateModel)
    
    message(crayon::yellow(paste0("The climate files do not seem to exist in pathToFutureRasters ",
                         crayon::white(pathToFutureRasters), 
                         ". Searching for the climate zip file...")))
    
    fullDatasetName <- googledrive::drive_get(googledrive::as_id(climateFilePath))$name
    
    # If zip not there, download it and unzip!
    if (!file.exists(file.path(pathToFutureRasters, fullDatasetName))) {
      
      message(crayon::red(paste0("The climate zip ", fullDatasetName," do not exist in pathToFutureRasters ",
                          crayon::white(pathToFutureRasters), ". Attempting to download ", 
                          crayon::white(climateModel), " from ", crayon::white(climateFilePath))))
      
      checkPath(pathToFutureRasters, create = TRUE)
      googledrive::drive_download(file = climateFilePath, 
                                  path = file.path(pathToFutureRasters, fullDatasetName))
    } 
    # Assuming the download happened correctly or the file is present, unzip
      if (tools::file_ext(file.path(pathToFutureRasters, fullDatasetName)) == "zip"){
        system(paste0("unzip ", file.path(pathToFutureRasters, fullDatasetName), " -d ", pathToFutureRasters))
      } else {
        if (tools::file_ext(file.path(pathToFutureRasters, fullDatasetName)) == "7z"){
          system(paste0("7za x ", file.path(pathToFutureRasters, fullDatasetName)))   
        } else {
          stop("Climate data needs to be zipped as .zip or .7z")
        }
      }
  }
    
  if (is.null(origTemplate)){
    # origTemplate needs to be the rasterToMatch postProcessed to the original
    # raster's projection (ascCRS) and resolution
    origTemplate <- raster::raster(list.files(path = pathToFutureRasters, 
                                               pattern = "MAP.asc$",
                                               recursive = TRUE, 
                                              # Any ClimateNA raster will do; template to modify the RTM
                                               full.names = TRUE)[1])
    crs(origTemplate) <- "+init=epsg:4326 +proj=longlat"
    origTemplate <- postProcess(x = rasterToMatch,
                                destinationPath = outputDir,
                                studyArea = studyArea,
                                rasterToMatch = origTemplate)
  }
  ######################### NORMALS ############################

  ##### ~~~~~~~~~~~~~ CALCULATE NORMAL CMI ~~~~~~~~~~~~~ #####
  rasNamenCMI <- file.path(outputDir, paste0("normalCMI_", rasterPrefix, ".grd"))
if (!file.exists(rasNamenCMI)){
  message("Preparing normal CMI...")
  # 1. Calculate normal MAP: as we have 2 sets of periods (1951-1980; 1981-2010), 
  # we have 2 rasters
  normalMAPs <- list.files(path = pathToNormalRasters, 
                           pattern = "MAP.asc$",
                           recursive = TRUE, 
                           full.names = TRUE) %>%
    lapply(., FUN = raster) %>%
    stack(.)
  crs(normalMAPs) <- "+init=epsg:4326 +proj=longlat"
  normalMAPs <- postProcess(normalMAPs,
                            destinationPath = outputDir,
                            rasterToMatch = origTemplate,
                            maskWithRTM = TRUE,
                            method = "bilinear")
  normalMAPVals <- getValues(raster::calc(normalMAPs, fun = mean, na.rm = TRUE))
  # 2. Calculate normal Eref: as we have 2 sets of periods (1951-1980; 1981-2010), 
  # we have 2 rasters
  normalErefs <- list.files(path = pathToNormalRasters, 
                            pattern = "Eref.asc$",
                            recursive = TRUE, 
                            full.names = TRUE) %>%
    lapply(., FUN = raster) %>%
    stack(.)
  crs(normalErefs) <- "+init=epsg:4326 +proj=longlat"
  normalErefs <- postProcess(normalErefs,
                             destinationPath = outputDir,
                             rasterToMatch = origTemplate,
                             maskWithRTM = TRUE,
                             method = "bilinear")
  normalErefVals <- getValues(raster::calc(normalErefs, fun = mean, na.rm = TRUE))
  
  # 3. Calculate normal CMI
  normalCMI <- raster(origTemplate) %>%
    setValues(normalMAPVals - normalErefVals)
  names(normalCMI) <- paste0("normalCMI_", rasterPrefix)
  
  # Transform back to save!
  normalCMIReproj <- reproducible::projectInputs(x = normalCMI,
                                                 targetCRS = crs(rasterToMatch), 
                                                 method = "bilinear")
  normalCMI <- postProcess(normalCMIReproj,
                           destinationPath = outputDir,
                           rasterToMatch = rasterToMatch,
                           maskWithRTM = TRUE,
                           studyArea = studyArea,
                           method = "bilinear")
  writeRaster(normalCMI,
              filename = rasNamenCMI, 
              datatype = "INT2S", 
              overwrite = TRUE)
}
  ##### ~~~~~~~~~~~~~ END NORMAL CMI ~~~~~~~~~~~~~ #####
  
  ##### ~~~~~~~~~~~~~ CALCULATE NORMAL MAT ~~~~~~~~~~~~~ #####
  # 1. Calculate normal MAT: as we have 2 sets of periods (1951-1980; 1981-2010), 
  # we have 2 rasters
  rasNamenMAT <- file.path(outputDir, paste0("normalMAT_", rasterPrefix,".grd"))
  if (!file.exists(rasNamenMAT)){
    message("Preparing normal MAT (for ATA)...")
  normalMATs <- list.files(path = pathToNormalRasters, 
                           pattern = "MAT.asc$",
                           recursive = TRUE, 
                           full.names = TRUE) %>%
    lapply(., FUN = raster) %>%
    stack(.)
  crs(normalMATs) <- "+init=epsg:4326 +proj=longlat"
  normalMATs <- postProcess(normalMATs,
                            destinationPath = outputDir,
                            rasterToMatch = origTemplate,
                            maskWithRTM = TRUE,
                            method = "bilinear")
  normalMATVals <- getValues(raster::calc(normalMATs, fun = mean, na.rm = TRUE))
  normalMATs <- raster(origTemplate) %>%
    setValues(normalMATVals)
  names(normalMATs) <- paste0("normalMAT_", rasterPrefix)
# This raster should NOT be postProcessed yet!
  writeRaster(normalMATs,
              filename = rasNamenMAT, 
              datatype = "INT2S", 
              overwrite = TRUE)
  } else {
    normalMATVals <- getValues(raster::raster(rasNamenMAT))
    origTemplate <- tryCatch({
      # Assertion that this raster has the same resolution that the other asc
      testthat::expect_true(length(normalMATVals) == length(origTemplate), 
                            label = paste0("number of cells normalMAT ==  number of ",
                                           "cells .asc rasters "))
      origTemplate
    }, error = function(e){
      warning(paste0(e, ". Processing original Template to match future layers"), 
                     immediate. = TRUE)
      RTM <- raster::raster(rasNamenMAT)
      origTemplatePost <- postProcess(x = origTemplate,
                                  destinationPath = outputDir,
                                  studyArea = studyArea,
                                  rasterToMatch = RTM)
      return(origTemplatePost)
    })

}
  ##### ~~~~~~~~~~~~~ END NORMAL MAT ~~~~~~~~~~~~~ #####
  
  ######################### FUTURE ############################
  
  ##### ~~~~~~~~~~~~~ CALCULATE FUTURE CMI ~~~~~~~~~~~~~ #####
  rasNameCMI <- file.path(outputDir, paste0(climateModel, "_CMI", years[1], "-",
                                            tail(years, 1), ".grd"))
  if (!file.exists(rasNameCMI)) {
    message("Preparing future CMI...")

    ##### MAP ####
    MAPrasters <- list.files(pathToFutureRasters, 
                            pattern = "MAP.asc",
                            recursive = TRUE, 
                            full.names = TRUE)
    # Assertion. Make sure that the order is correct in function of the years!
    YearMSY <- usefulFuns::substrBoth(usefulFuns::substrBoth(strng = dirname(MAPrasters),
                                                             howManyCharacters = 7,
                                                             fromEnd = TRUE),
                                      howManyCharacters = 4,
                                      fromEnd = FALSE)
    testthat::expect_true(all(as.numeric(YearMSY) == years))
    
    MAPrasters <- raster::stack(lapply(MAPrasters, FUN = raster))
    crs(MAPrasters) <- "+init=epsg:4326 +proj=longlat"
    names(MAPrasters) <- paste0("MAP_", YearMSY)
    MAPname <- file.path(outputDir, paste0(climateModel, "_MAP", years[1], "-",
                                            tail(years, 1), ".grd"))
    if (file.exists(MAPname)){
      MAP <- raster::stack(MAPname)
    } else {
      MAP <- raster::stack(lapply(names(MAPrasters), function(rasName){
        message(crayon::white(paste0("Processing ", rasName)))
        ras <- MAPrasters[[rasName]]
        ras <- tryCatch({
          ras[] <- ras[]
          return(ras)
        }, error = function(e){
          message(crayon::red(paste0(e, " - ", rasName,
                                     " will be replaced by average of previous and following year")))
          rPos <- which(names(MAPrasters) == rasName)
          r1 <- MAPrasters[[rPos-1]]
          r2 <- MAPrasters[[rPos+1]]
          ras <- raster::calc(raster::stack(r1, r2), fun = mean, na.rm = TRUE)
          return(ras)
        })
        r <- postProcess(ras,
                         destinationPath = outputDir,
                         rasterToMatch = origTemplate,
                         maskWithRTM = TRUE,
                         method = "bilinear")
        rm(ras)
        gc()
        return(r)
      }))
      names(MAP) <- paste0("MAP_", YearMSY)
      writeRaster(MAP,
                  filename = MAPname, 
                  datatype = "INT2S")
    }

    ##### Eref ####

    ErefRasters <- list.files(pathToFutureRasters, 
                              pattern = "Eref.asc",
                              recursive = TRUE, 
                              full.names = TRUE)
    # Assertion. Make sure that the order is correct in function of the years!
    YearMSY <- usefulFuns::substrBoth(usefulFuns::substrBoth(strng = dirname(ErefRasters),
                                                             howManyCharacters = 7,
                                                             fromEnd = TRUE),
                                      howManyCharacters = 4,
                                      fromEnd = FALSE)
    testthat::expect_true(all(as.numeric(YearMSY) == years))
    
    ErefRasters <- raster::stack(lapply(ErefRasters, FUN = raster))
    crs(ErefRasters) <- "+init=epsg:4326 +proj=longlat"
    names(ErefRasters) <- paste0("Eref_", YearMSY)
    EREFname <- file.path(outputDir, paste0(climateModel, "_Eref", years[1], "-",
                                            tail(years, 1), ".grd"))
    if (file.exists(EREFname)){
      EREF <- raster::stack(EREFname)
    } else {
         EREF <- raster::stack(lapply(names(ErefRasters), function(rasName){
      message(crayon::white(paste0("Processing ", rasName)))
      ras <- ErefRasters[[rasName]]
      ras <- tryCatch({
        ras[] <- ras[]
        return(ras)
      }, error = function(e){
        message(crayon::red(paste0(e, " - ", rasName,
                                   " will be replaced by average of previous and following year")))
        rPos <- which(names(ErefRasters) == rasName)
        r1 <- ErefRasters[[rPos-1]]
        r2 <- ErefRasters[[rPos+1]]
        ras <- raster::calc(raster::stack(r1, r2), fun = mean, na.rm = TRUE)
        return(ras)
      })
      r <- postProcess(ras,
                       destinationPath = outputDir,
                       rasterToMatch = origTemplate,
                       maskWithRTM = TRUE,
                       method = "bilinear")
      rm(ras)
      gc()
      return(r)
    }))
         names(EREF) <- paste0("Eref_", YearMSY)
         writeRaster(EREF,
                     filename = EREFname, 
                     datatype = "INT2S")
         }
    
    # Calculate FUTURE CMI
    CMIstack <- raster::stack(lapply(years, FUN = function(year, 
                                        MAPStack = MAP, 
                                        ErefStack = EREF){
      MAPRaster <- MAPStack[[paste0("MAP_", year)]]
      erefRaster <- ErefStack[[paste0("Eref_", year)]]
      CMIvals <- getValues(MAPRaster) - getValues(erefRaster)
      CMI <- setValues(raster(origTemplate), CMIvals)
      crs(CMI) <- "+init=epsg:4326 +proj=longlat"
      # Transform back to save!
      futureCMIReproj <- reproducible::projectInputs(x = CMI,
                                                     targetCRS = crs(rasterToMatch), 
                                                     method = "bilinear")
      CMI <- postProcess(futureCMIReproj,
                              destinationPath = outputDir,
                              rasterToMatch = rasterToMatch,
                              maskWithRTM = TRUE,
                              studyArea = studyArea,
                              method = "bilinear")
      return(CMI)
    }))
    names(CMIstack) <- paste0("CMI", years)
    
    writeRaster(CMIstack,
                filename = rasNameCMI, 
                datatype = "INT2S")
  }
  ##### ~~~~~~~~~~~~~ END FUTURE CMI ~~~~~~~~~~~~~ #####
  
  ##### ~~~~~~~~~~~~~ CALCULATE FUTURE ATA ~~~~~~~~~~~~~ #####
  rasNameATA <- file.path(outputDir, paste0(climateModel, "_ATA", years[1], "-",
                                            tail(years, 1), ".grd"))
  if (!file.exists(rasNameATA)) {
    message("Preparing future ATA...")
    
    # MAT rasters
    MATrasters <- list.files(pathToFutureRasters, 
                             pattern = "MAT.asc",
                             recursive = TRUE, 
                             full.names = TRUE)
    
    # Assertion. Make sure that the order is correct in function of the years!
    YearMSY <- usefulFuns::substrBoth(usefulFuns::substrBoth(strng = dirname(MATrasters),
                                                             howManyCharacters = 7,
                                                             fromEnd = TRUE),
                                      howManyCharacters = 4,
                                      fromEnd = FALSE)
    testthat::expect_true(all(as.numeric(YearMSY) == years))
    
    MATrasters <- raster::stack(lapply(MATrasters, FUN = raster))
    crs(MATrasters) <- "+init=epsg:4326 +proj=longlat"
    names(MATrasters) <- paste0("MAT_", YearMSY)
    MATname <- file.path(outputDir, paste0(climateModel, "_MAT", years[1], "-",
                                           tail(years, 1), ".grd"))
    if (file.exists(MATname)){
      MAT <- raster::stack(MATname)
    } else {
      MAT <- raster::stack(lapply(names(MATrasters), function(rasName){
        message(crayon::white(paste0("Processing ", rasName)))
        ras <- MATrasters[[rasName]]
        ras <- tryCatch({
          ras[] <- ras[]
          return(ras)
        }, error = function(e){
          message(crayon::red(paste0(e, " - ", rasName,
                                     " will be replaced by average of previous and following year")))
          rPos <- which(names(MATrasters) == rasName)
          r1 <- MATrasters[[rPos-1]]
          r2 <- MATrasters[[rPos+1]]
          ras <- raster::calc(raster::stack(r1, r2), fun = mean, na.rm = TRUE)
          return(ras)
        })
        r <- postProcess(ras,
                         destinationPath = outputDir,
                         rasterToMatch = origTemplate,
                         maskWithRTM = TRUE,
                         method = "bilinear")
        rm(ras)
        gc()
        return(r)
      }))
      names(MAT) <- paste0("MAT_", YearMSY)
      writeRaster(MAT,
                  filename = MATname, 
                  datatype = "INT2S")
    }
    
    # ATA rasters
    ATAstack <- raster::stack(lapply(years, FUN = function(year, 
                                                           MATstack = MAT,
                                                           normal = normalMATVals) {
      MATvals <- raster::getValues(MATstack[[paste0("MAT_", year)]])
      ATA <- (MATvals - normal) #transform for saving as INT2S
      ATA <- setValues(raster(origTemplate), ATA)
      crs(ATA) <- "+init=epsg:4326 +proj=longlat"
      
      # Transform back to save!
      futureATAReproj <- reproducible::projectInputs(x = ATA,
                                                     targetCRS = crs(rasterToMatch), 
                                                     method = "bilinear")
      ATA <- postProcess(futureATAReproj,
                              destinationPath = outputDir,
                              rasterToMatch = rasterToMatch,
                              maskWithRTM = TRUE,
                              studyArea = studyArea,
                              method = "bilinear")
      
      return(ATA)
    }))
    names(ATAstack) <- paste0("ATA", years)
    
    writeRaster(ATAstack,
                filename = rasNameATA, 
                datatype = "INT2S",
                overwrite = TRUE)
  }
  ##### ~~~~~~~~~~~~~ END FUTURE ATA ~~~~~~~~~~~~~ #####
  return(list(normalsCMI = raster::raster(rasNamenCMI),
              futureATA = raster::stack(rasNameATA),
              futureCMI = raster::stack(rasNameCMI)))
}

