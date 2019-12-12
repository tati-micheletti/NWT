createCumEffRasters <- function(birds = c("CAWA", "OSFL", "RUBL"),
                                rasFolder = "/mnt/data/Micheletti/NWT/outputs/06DEC19/effectsRasters",
                                googlefolderID = "1lnM3Va3UklcGy_Swlc7AY1Ww7nImPYLM"){
  library(raster)
  library(googledrive)
  library(future)
  library(future.apply)
  library(usefun)
  plan("multicore")
  
  future_lapply(birds, function(BIRD){
    birdsPaths <- grepMulti(list.files(rasFolder, full.names = TRUE), 
                            patterns = c("cumulativeEffect", "_abs_", BIRD, "delta.tif"))
    BIRDS <- stack(lapply(birdsPaths, function(pths){
      B <- raster(pths)
      B[] <- B[]
      return(B)
    })
    )
    averageBIRD <- calc(BIRDS, fun = mean)
    sdBIRD <- calc(BIRDS, fun = sd)
    aveName <- file.path(rasFolder, paste0("averageDelta_abs_", BIRD, ".tif"))
    sdName <- file.path(rasFolder, paste0("sdDelta_abs_", BIRD, ".tif"))
    writeRaster(averageBIRD, filename = aveName, format = "GTiff")
    writeRaster(sdBIRD, filename = sdName, format = "GTiff")
    lapply(c(aveName, sdName), function(ras){
      drive_upload(ras, as_id(googlefolderID))
    })
  })
  print(paste0("Rasters created for ", birds))
}
