createCumEffRasters <- function(species = c("CAWA", "OSFL", "RUBL"),
                                rasFolder = "/mnt/data/Micheletti/NWT/outputs/PAPER/effectsRasters",
                                googlefolderID = NULL){
  library(raster)
  library(googledrive)
  library(future)
  library(future.apply)
  library(usefun)
  plan("multicore")
  
  future_lapply(species, function(SP){
    birdsPaths <- grepMulti(list.files(rasFolder, full.names = TRUE),
                            patterns = c("cumulativeEffect", SP, "delta.tif"))
    SP <- stack(lapply(birdsPaths, function(pths){
      B <- raster(pths)
      B[] <- B[]
      return(B)
    })
    )
    averageSP <- calc(SP, fun = mean)
    sdSP <- calc(SP, fun = sd)
    aveName <- file.path(rasFolder, paste0("averageDelta_abs_", SP, ".tif"))
    sdName <- file.path(rasFolder, paste0("sdDelta_abs_", SP, ".tif"))
    writeRaster(averageSP, filename = aveName, format = "GTiff")
    writeRaster(sdSP, filename = sdName, format = "GTiff")
    if (!is.null(googlefolderID)){
      lapply(c(aveName, sdName), function(ras){
        drive_upload(ras, as_id(googlefolderID))
      })
    }
  })
  print(paste0("Rasters created for ", species))
}
