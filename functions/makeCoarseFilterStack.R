makeCoarseFilterStack <- function(stk, 
                                  years, 
                                  nBreaks = 6, 
                                  maxSizeForVerySmall = NULL){
  # The last is a named list with the ;layer names and the max area size
  # in ha that a feature can be classified as very small. The others are 
  # ranked as Jenks Natural Breaks
  Require("BAMMtools")
  stkFeatures <- raster::stack(lapply(X = names(stk), FUN = function(layName){
    lay <- stk[[layName]]
    message(paste0("Reclassifying ", layName))
    DT <- data.table(vals = getValues(lay))
    DT[, counted := .N, by = "vals"]
    DT[is.na(vals), counted := NA]
    uniqueValues <- unique(DT[!is.na(counted), counted])
    layCounts <- setValues(raster(lay), DT[["counted"]])
    if (is.null(maxSizeForVerySmall)){
      # Apply the Jenks Natural Breaks to these values
      jenkBreaks <- getJenksBreaks(uniqueValues, 6)
      reclass <- matrix(c(jenkBreaks[-length(jenkBreaks)],
                          jenkBreaks[-1],
                          c(100, 25, 20, 15, 10)), ncol = 3)
      reclass[1] <- 0
    } else {
      maxNpixels <- maxSizeForVerySmall[[layName]]/6.25 
      # Divide the total max size by the size of each pixel to get the max amount 
      # of pixels that classify as very small
      uniqueValues <- uniqueValues[uniqueValues > maxNpixels]
      jenkBreaks <- getJenksBreaks(uniqueValues, 4)
      reclass <- matrix(c(0, maxNpixels, jenkBreaks[-length(jenkBreaks)],
                          maxNpixels, jenkBreaks,
                          c(100, 25, 20, 15, 10)), ncol = 3)
      }
    layReclass <- raster::reclassify(x = layCounts, rcl = reclass)
    names(layReclass) <- layName
    return(layReclass)
  }))
  stkFeaturesList <- lapply(years, function(Y){
    stkY <- stkFeatures
    return(stkY)
  })
  names(stkFeaturesList) <- paste0("Year", years)
  
  return(stkFeaturesList)
}
