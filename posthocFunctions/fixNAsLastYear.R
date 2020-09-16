fixNAsLastYear <- function(rasterList, years){
  scenarioNames <- names(rasterList)
  firstYear <- min(years)
  lastYear <- max(years)
  ras <- lapply(scenarioNames, function(scenario){
    message(paste0("Fixing last year's raster of ", scenario))
    ras <- rasterList[[scenario]]
    rasFirstYear <- ras[[paste0("year", firstYear)]]
    rasLastYear <- ras[[paste0("year", lastYear)]]
    index <- which(!is.na(rasFirstYear[]) & is.na(rasLastYear[]))
    rasLastYear[index] <- 0
    attr(rasLastYear, "indexNA") <- index
    attr(rasLastYear, "totalNAto0") <- length(index)
    rasToReturn <- list(rasFirstYear, rasLastYear)
    names(rasToReturn) <- paste0("year", c(firstYear, lastYear))
    return(rasToReturn)
  })
  names(ras) <- scenarioNames
  return(ras)
}

