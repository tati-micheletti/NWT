buildClimateLayers <- function(rasterResolution,
                               rtm,
                               dataPathSim,
                               gcm,
                               rcp,
                               period){
 climateRasStack <- setNames(
    raster::stack(
      lapply(
        dir(dataPathSim, pattern = paste0(gcm, "_rcp", rcp, "_", period, ".*[.]asc"), 
            full.names = TRUE, recursive = TRUE),
        function(file)
        {
          r <- raster(file)
          crs(r) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +units=m +no_defs +datum=WGS84"
          
          postProcess(
            x = r,
            rasterToMatch = rtm,
            maskWithRTM = TRUE,
            filename2 = NULL,
            method = "bilinear",
            destinationPath = tempdir(),
            omitArgs = "destinationPath"
          )
        }
      )
    ),
    nm = c(
      paste0("PPT", stringr::str_pad(1:12, width = 2, pad = 0)),
      paste0("Tave", stringr::str_pad(1:12, width = 2, pad = 0)),
      paste0("Tmax", stringr::str_pad(1:12, width = 2, pad = 0)),
      paste0("Tmin", stringr::str_pad(1:12, width = 2, pad = 0))
    )
  )
  return(climateRasStack)
}