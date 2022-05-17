runSquarenessTest2 <- function(climateModel, ensemble, 
                               inputPath = Paths$inputPath, 
                               SA){
  rTest <- raster::raster(file.path(inputPath, paste0(
                                 climateModel,"_85/CCSM4_RCP85_annual/", climateModel,"_RCP85_", 
                                 ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                 "2100MSY/PPT_wt.asc")))
  rTest2 <- raster::raster(file.path(inputPath, paste0( 
                                  climateModel,"_85/CCSM4_RCP85_annual/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2100MSY/Tave_sm.asc")))
  rTest3 <- raster::raster(file.path(inputPath, paste0( 
                                  climateModel,"_85/CCSM4_RCP85_annual/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2100MSY/MSP.asc")))
  rTest4 <- raster::raster(file.path(inputPath, paste0(
                                  climateModel,"_85/CCSM4_RCP85_annual/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2100MSY/DD18.asc")))
  stk2 <- raster::stack(rTest, rTest2, rTest3, rTest4)
  rTest <- raster::raster(file.path(inputPath, paste0(
                                 climateModel,"_85/CCSM4_RCP85_annual/", climateModel,"_RCP85_", 
                                 ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                 "2011MSY/PPT_wt.asc")))
  rTest2 <- raster::raster(file.path(inputPath, paste0( 
                                  climateModel,"_85/CCSM4_RCP85_annual/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2011MSY/Tave_sm.asc")))
  rTest3 <- raster::raster(file.path(inputPath, paste0( 
                                  climateModel,"_85/CCSM4_RCP85_annual/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2011MSY/MSP.asc")))
  rTest4 <- raster::raster(file.path(inputPath, paste0(
                                  climateModel,"_85/CCSM4_RCP85_annual/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2011MSY/DD18.asc")))
  stk <- raster::stack(rTest, rTest2, rTest3, rTest4)
  stkRes <- stk2-stk
  stkResOK <- raster::stack(lapply(1:nlayers(stkRes), function(rNum){
    ras <- stkRes[[rNum]]
    crs(ras) <- sp::CRS(paste0('+init=epsg:4326 +proj=longlat +ellps=WGS84 ",
                                  "+datum=WGS84 +no_defs +towgs84=0,0,0'))
    r <- reproducible::postProcess(ras, 
                                   studyArea = SA, 
                                   destinationPath = tempdir())
    return(r)
  }))
  raster::plot(stkResOK[[1]], main = paste0(climateModel, " - PPT_wt"))
  raster::plot(stkResOK[[2]], main = paste0(climateModel, " - Tave_sm"))
  raster::plot(stkResOK[[3]], main = paste0(climateModel, " - MSP"))
  raster::plot(stkResOK[[4]], main = paste0(climateModel, " - DD18"))
  print("Check plots!")
}
