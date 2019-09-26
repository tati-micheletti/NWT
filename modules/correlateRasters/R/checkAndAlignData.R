checkAndAlignData <- function(raster1, raster2){
  # Changes raster2 in function of raster1 if needed
  tryCatch({
    stack(raster1, raster2)
  }, error = function(e){
    ras2 <- reproducible::postProcess(raster2, rasterToMatch = raster1, filename2 = NULL)
    tryCatch({
      stack(raster1, ras2)
      message(crayon::green("Rasters are aligned and overlapping"))
      return(ras2)
    }, error = function(e){
      stop("rasters do not align even after postProcessing. Please debug")
    })
  })
}