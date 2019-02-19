corePrediction <- function(x, successionLayers = successionLayers,
                           staticLayers = staticLayers,
                           currentTime = currentTime,
                           modelList = modelList,
                           overwritePredictions = overwritePredictions,
                           pathData = pathData){
  
  reproducible::Require("magrittr")
  
  message(crayon::yellow("Predicting for ", x , ". Prediction for time ", currentTime))
  suppressWarnings(dir.create(file.path(pathData, "predicted")))
  
  models <- modelList[[x]]
  if ("glmerMod" %in% class(models)){
    nameStackRas1 <- names(models@frame)[2]
    nameStackRas2 <- names(models@frame)[3]
  } else {
    if ("glm" %in% class(models)){
      nameStackRas1 <- names(models$coefficients)[2]
      nameStackRas2 <- names(models$coefficients)[3]
    } else {
      if ("gbm" %in% class(models)){ # If gbm, do everything in here, else, do outside
        stkLays <- raster::stack(successionLayers, staticLayers)
        predictedName <- file.path(pathData, paste0("predicted/predicted", x, "Year", currentTime, ".tif"))
        if (isTRUE(overwritePredictions)||!file.exists(predictedName)){
          predicted <- gbm::predict.gbm(object = models, newdata = raster::as.data.frame(stkLays, row.names = TRUE),
                                        type = "response",
                                        n.trees = models$n.trees)
          message(crayon::green("Masking ", x , " prediction to studyArea for time ", currentTime))
          basePlot <- stkLays[[1]] %>% 
            setValues(predicted) %>%
            reproducible::fastMask(y = studyArea)
          
          names(basePlot) <- paste0("predicted", x)
          raster::writeRaster(x = basePlot, filename = predictedName,
                              format = "GTiff", overwrite = TRUE)
          
        }
        predicted <- raster(predictedName)
        gc()
        return(predicted)
      }
    }
  }
  focDis <- as.numeric(gsub("[^\\d]+", "", nameStackRas1, perl=TRUE))
  predictedName <- file.path(pathData, paste0("predicted/predictedFocal", focDis, "m", x, currentTime, ".tif"))
  if (isTRUE(overwritePredictions)||!file.exists(predictedName)){
    birdD <- raster::raster(birdDensityRasters[[x]])
    valsD <- log(raster::getValues(birdD)) # log the value of densities so it is the same of the original model
    valsD[valsD < -0.99] <- -1
    birdD <- raster::setValues(birdD, valsD)
    rm(valsD)
    gc()
    if (any(!identical(round(raster::extent(x = disturbanceRas), 10^-100), round(raster::extent(birdD), 10^-100)),
            !identical(raster::res(x = disturbanceRas), raster::res(birdD)),
            !identical(raster::crs(x = disturbanceRas), raster::crs(birdD)))){
      disturbanceRas <- postProcess(x = disturbanceRas, rasterToMatch = birdD,
                                    maskWithRTM = TRUE, 
                                    filename2 = file.path(pathData, "predicted", paste0(disturbanceRas@data@names, "Fixed")),
                                    format = "GTiff", overwrite = TRUE, useCache = FALSE)
    }
    stackRas <- raster::stack(disturbanceRas, birdD) # Might need to remove individual rasters here
    names(stackRas)[1] <- nameStackRas1
    names(stackRas)[2] <- nameStackRas2
    suppressWarnings(predicted <- fitModel(inRas = stackRas, 
                                           inputModel = models, 
                                           x = x, 
                                           tileYear = currentTime))
    raster::writeRaster(x = predicted, filename = predictedName, 
                        format = "GTiff", overwrite = TRUE)
  }
  predicted <- raster(predictedName)
  gc()
  return(predicted) # The predicted value is NOT multiplied by 1000! For that, need to change fitModel!
}