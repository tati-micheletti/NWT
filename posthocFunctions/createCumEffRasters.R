createCumEffRasters <- function(species = c("CAWA", "OSFL", "RUBL"),
                                rasFolder = "/mnt/data/Micheletti/NWT/outputs/PAPER/effectsRasters",
                                googlefolderID = NULL,
                                overwrite = FALSE){
  library(raster)
  library(googledrive)
  library(future)
  library(future.apply)
  library(usefulFuns)
  plan("multicore")
  
  future_lapply(species, function(SP){ 
    birdsPaths <- grepMulti(list.files(rasFolder, full.names = TRUE),
                            patterns = c("cumulativeEffect", SP, "delta.tif"))
    spStack <- stack(lapply(birdsPaths, function(pths){
      B <- raster(pths)
      B[] <- B[]
      return(B)
    })
    )
    message("Calculating averaged cummulative effects...")
    averageSP <- calc(spStack, fun = mean)
    names(averageSP) <- paste0("averageDelta", SP, ".tif")
    message("Calculating deviation cummulative effects...")
    sdSP <- calc(spStack, fun = sd)
    names(sdSP) <- paste0("sdDelta", SP, ".tif")
    aveName <- file.path(rasFolder, paste0("averageDelta", SP, ".tif"))
    sdName <- file.path(rasFolder, paste0("sdDelta", SP, ".tif"))
    writeRaster(averageSP, filename = aveName, format = "GTiff", overwrite = overwrite)
    writeRaster(sdSP, filename = sdName, format = "GTiff", overwrite = overwrite)
    if (!is.null(googlefolderID)){
      lapply(c(aveName, sdName), function(ras){
        drive_upload(ras, as_id(googlefolderID))
      })
    }
  })
  print(paste0("Rasters created for ", species))
}
