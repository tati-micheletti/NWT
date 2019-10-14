plotMaxAge <- function(years = c(2001, 2100), 
                       folderData, 
                       typeSim, 
                       colNA = "grey85", saveRAS = TRUE){
  library("usefun")
  library("LandR")
  library("reproducible")
  library("SpaDES.tools")
  library("data.table")
  folder <- folderData #"04JUL19" #18JUN19_CS_SCFM" #"08JUN19" #"29JUN19" 12JUL19 --> NoCS  12JUL19 --> CS
  simul <- typeSim
  folderPath <- paste0("/mnt/data/Micheletti/NWT/outputs/", folder,"/")
  
  cohorDataList <- bringObjectTS(path = folderPath, rastersNamePattern = "cohortData")
  pixelGroupList <- bringObjectTS(path = folderPath, rastersNamePattern = "pixelGroupMap")
  
  # MAX AGE
  maxAge <- data.table::rbindlist(lapply(X = 1:length(cohorDataList), function(index){
    cohort <- cohorDataList[[index]]
    pixelGroup <- pixelGroupList[[index]]
    a <- cohort[, list(maxAge = max(age, na.rm = TRUE)), by = "pixelGroup"]
    r <- rasterizeReduced(a, pixelGroup, "maxAge", "pixelGroup")
    return(list(meanAge = mean(r[], na.rm = TRUE),
                minAge = min(r[], na.rm = TRUE),
                maxAge = max(r[], na.rm = TRUE),
                medianAge = median(r[], na.rm = TRUE),
                years = as.numeric(usefun::substrBoth(strng = index, 
                                                      howManyCharacters = 4, 
                                                      fromEnd = TRUE))))
  }))
  # AGE ~~~~~~~~~~~~~~~~

  maxAgePlot <- lapply(X = years, function(index){
    cohort <- cohorDataList[[paste0("Year", index)]]
    pixelGroup <- pixelGroupList[[paste0("Year", index)]]
    a <- cohort[, list(maxAge = max(age, na.rm = TRUE)), by = "pixelGroup"]
    r <- SpaDES.tools::rasterizeReduced(a, pixelGroup, "maxAge", "pixelGroup")
    return(r)
  })
  names(maxAgePlot) <- paste0("maxAgeYear", years)
  if (saveRAS){
    lapply(1:length(maxAgePlot), function(index){
      writeRaster(x = maxAgePlot[[index]], filename = paste0(folderPath, "RAS", names(maxAgePlot)[index]), 
                  format = "GTiff", overwrite = TRUE)
    })
  }
  rng = range(c(raster::getValues(maxAgePlot[[1]]), raster::getValues(maxAgePlot[[2]])), na.rm = TRUE)
  brks <- c(seq(min(rng), max(rng), by = 10))
  nb <- length(brks)-1
  cols <- rev(heat.colors(nb))
  parSetup <- par()
  invisible(on.exit(par(parSetup)))
  if (length(years < 4)){
    par(mfrow=c(1,length(years)))
  } else {
    if (all(length(years > 3), length(years < 7))){
      par(mfrow=c(length(years)/2,length(years)))
    }
  }
  # Did this for ggplot, but it is super slow on this size of raster
  # df <- rbindlist(lapply(X = names(maxAgePlot), function(plt){
  #   coord <- data.table::data.table(coordinates(obj = maxAgePlot[[plt]]))
  #   vals <- data.table::data.table(age = raster::getValues(x = maxAgePlot[[plt]]))
  #   year <- data.table::data.table(year = as.numeric(usefun::substrBoth(strng = plt, howManyCharacters = 4, fromEnd = TRUE)))
  #   dt <- cbind(coord, vals, year)
  #   return(dt)
  # })
  # )
  library("ggplot2")
  library("raster")
  plot(maxAgePlot[[1]], breaks=brks, col=cols, lab.breaks=brks,
       main=paste0('Max Age ', names(maxAgePlot)[[1]], " - ", typeSim), colNA = colNA)
  plot(maxAgePlot[[2]], breaks=brks, col=cols, lab.breaks=brks,
       main=paste0('Max Age ', names(maxAgePlot)[[2]], " - ", typeSim), colNA = colNA)
  p <- recordPlot()
  return(p)
}