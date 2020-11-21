calcColonization <- function(rasT0Path, rasT1Path = NULL, 
                             percentToDiscard = 0.3, rasName,
                             typeOfAnalysis = "colonization"){
  if (is(rasT0Path, "character")){
    rasT0Path <- raster::raster(rasT0Path)
  }
  if (!is.null(rasT1Path)){
    if (is(rasT1Path, "character")){
      rasT1Path <- raster::raster(rasT1Path)
    }
  }
  rasT0 <- .presenceAbsenceRas(rasT0Path, percentToDiscard, typeOfAnalysis)
  if (!is.null(rasT1Path)){
    rasT1 <- .presenceAbsenceRas(rasT1Path, percentToDiscard, typeOfAnalysis)
    rasCol <- rasT1 - rasT0
  } else {
    rasCol <- rasT0
  }
  writeRaster(rasCol, filename = rasName, format = "GTiff")
  return(raster::raster(paste0(rasName, ".tif")))
}

.presenceAbsenceRas <- function(ras, percentToDiscard = 0.3, 
                                typeOfAnalysis = typeOfAnalysis){
  CSdt <- data.table::data.table(pixelID = 1:ncell(ras),
                                 val = getValues(ras))
  CSdt <- na.omit(CSdt)
  data.table::setkey(CSdt, val)
  CSdt[, CUM := cumsum(val)]
  CSdt[, CUMstd := CUM/sum(val)]
  CSdt[, PA := CUMstd > percentToDiscard]
  if (typeOfAnalysis == "colonization"){
    BIRDpres <- raster(ras)
    BIRDpres[CSdt[PA == TRUE, pixelID]] <- 1
    BIRDpres[CSdt[PA == FALSE, pixelID]] <- 0
  } else if (typeOfAnalysis == "density"){
    BIRDpres <- ras
    BIRDpres[CSdt[PA == FALSE, pixelID]] <- 0
  } else stop("typeOfAnalysis needs to be either 'colonization' or 'density'")

  return(BIRDpres)
}