runSquarenessTest <- function(climateModel, ensemble, inputPath = Paths$inputPath){
  rTest <- raster::raster(file.path(inputPath, paste0(
                                 climateModel,"_RCP85/", climateModel,"_RCP85_", 
                                 ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                 "2100MSY/PPT_wt.asc")))
  rTest2 <- raster::raster(file.path(inputPath, paste0( 
                                  climateModel,"_RCP85/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2100MSY/Tave_sm.asc")))
  rTest3 <- raster::raster(file.path(inputPath, paste0( 
                                  climateModel,"_RCP85/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2100MSY/MSP.asc")))
  rTest4 <- raster::raster(file.path(inputPath, paste0(
                                  climateModel,"_RCP85/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2100MSY/DD18.asc")))
  stk2 <- raster::stack(rTest, rTest2, rTest3, rTest4)
  rTest <- raster::raster(file.path(inputPath, paste0(
                                 climateModel,"_RCP85/", climateModel,"_RCP85_", 
                                 ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                 "2011MSY/PPT_wt.asc")))
  rTest2 <- raster::raster(file.path(inputPath, paste0( 
                                  climateModel,"_RCP85/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2011MSY/Tave_sm.asc")))
  rTest3 <- raster::raster(file.path(inputPath, paste0( 
                                  climateModel,"_RCP85/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2011MSY/MSP.asc")))
  rTest4 <- raster::raster(file.path(inputPath, paste0(
                                  climateModel,"_RCP85/", climateModel,"_RCP85_", 
                                  ifelse(ensemble != "", paste0(ensemble, "_"), ""),
                                  "2011MSY/DD18.asc")))
  stk <- raster::stack(rTest, rTest2, rTest3, rTest4)
  stkRes <- stk2-stk
  raster::plot(stkRes[[1]], main = paste0(climateModel, " - PPT_wt"))
  raster::plot(stkRes[[2]], main = paste0(climateModel, " - Tave_sm"))
  raster::plot(stkRes[[3]], main = paste0(climateModel, " - MSP"))
  raster::plot(stkRes[[4]], main = paste0(climateModel, " - DD18"))
  print("Check plots!")
}
