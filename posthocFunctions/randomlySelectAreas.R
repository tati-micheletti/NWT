randomlySelectAreas <- function(ras, percentageArea, reps = 100){
  totalArea <- sum(ras[], na.rm = TRUE)
  totPix <- round(percentageArea*totalArea, 0)
  ras[] <- ras[]
  allPix <- na.omit(data.table(pixelID = 1:ncell(ras),
                               vals = getValues(ras)))
  if (reps > 1){
    bootSelected <- raster::calc(raster::stack(lapply(1:reps, function(Rep){
      selectedPix <- sample(allPix[["pixelID"]], totPix)
      selected <- ras
      selected[!is.na(ras[])] <- 0
      selected[selectedPix] <- 1
      return(selected)
    })), fun = mean, na.rm = TRUE)
  } else {
    selected <- sample(allPix[["pixelID"]], totPix)
    bootSelected <- ras
    bootSelected[!is.na(ras[])] <- 0
    bootSelected[selected] <- 1
  }
  return(bootSelected)
}
