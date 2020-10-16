checkBirdPredictions <- function(species = c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "BAWW", "BBWA", "BBWO", 
                                             "BCCH", "BHCO", "BHVI", "BLPW", "BOCH", "BRBL", "BRCR", "BTNW", 
                                             "CAWA", "CHSP", "CORA", "COYE", "DEJU", "EAKI", "EAPH", "FOSP", 
                                             "GRAJ", "HETH", "HOLA", "LCSP", "LEFL", "LISP", "MAWA", "NOFL", 
                                             "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PISI", "PIWO", "PUFI", 
                                             "RBGR", "RBNU", "RCKI", "REVI", "RUGR", "RWBL", "SAVS", "SOSP", 
                                             "SWSP", "SWTH", "TEWA", "TRES", "WAVI", "WCSP", "WETA", "WEWP", 
                                             "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA", "YRWA"), 
                                 outputFolderForPlots = "~/projects/NWT/outputs/SIMULATIONS/BirdPlots/",
                                 inputFolderPredictedRasters = "~/projects/NWT/outputs/SIMULATIONS/",
                                 run = "run1",
                                 birdModel = "V6a",
                                 years = c(2011, 2031, 2051, 2071, 2091, 2100),
                                 plotLOGvalues = TRUE,
                                 upload = TRUE,
                                 googleFolderID = "1q7vXFH3aZLAI-yJP-FeJaCMp6wGWL4uc"){
  
  allOk <- sapply(species, function(B){
    sapply(c("LandR.CS_fS", "LandR.CS_SCFM", "LandR_fS", "LandR_SCFM"), function(SIMUL){
      fileNAME <- paste0(outputFolderForPlots, 
                         SIMUL,"_", B,'.png')
      if (!file.exists(fileNAME)){
        allYears <- lapply(years, function(Y){
          ras2011 <- raster::raster(paste0(inputFolderPredictedRasters, SIMUL,
                                           "/", run,"/birdPredictions", birdModel, "/", 
                                           run , SIMUL,"predicted",B,
                                           "Year", Y,".tif"))
        })
        BIRD <- raster::stack(allYears)
        names(BIRD) <- paste0(B, years)
        if (plotLOGvalues)
          BIRD <- raster::stack(log(BIRD))
        library(rasterVis)
        library(viridis)
        names(BIRD) <- paste0(SIMUL, "_", B, years)
        plot(BIRD, col = plasma(100), zlim = c(min(sapply(BIRD@layers, 
                                                          function(LAY) LAY@data@min)), 
                                               max(sapply(BIRD@layers, 
                                                          function(LAY) LAY@data@max))))
        dev.copy(png, fileNAME, height = 1000, width = 1400)
        dev.off()
        dev.off()
      }
      return("OK")
    })
    if (upload){
      fl <- grepMulti(x = list.files(outputFolderForPlots, full.names = TRUE),
                      patterns = "LandR")
      lapply(X = fl, FUN = drive_upload, path = as_id(googleFolderID))
    }
    return(allOk)
  })
}
  