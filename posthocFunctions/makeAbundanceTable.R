makeAbundanceTable <- function(Species = c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "BAWW", "BBWA", "BBWO", 
                                           "BCCH", "BHCO", "BHVI", "BLPW", "BOCH", "BRBL", "BRCR", "BTNW", 
                                           "CAWA", "CHSP", "CORA", "COYE", "DEJU", "EAKI", "EAPH", "FOSP", 
                                           "GRAJ", "HETH", "HOLA", "LCSP", "LEFL", "LISP", "MAWA", "NOFL", 
                                           "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PISI", "PIWO", "PUFI", 
                                           "RBGR", "RBNU", "RCKI", "REVI", "RUGR", "RWBL", "SAVS", "SOSP", 
                                           "SWSP", "SWTH", "TEWA", "TRES", "WAVI", "WCSP", "WETA", "WEWP", 
                                           "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA", "YRWA"),
                               folderAbundance, 
                               folderColonization, 
                               year, 
                               useFuture = TRUE){
  
  # NEEDS SOME REVIEW FOR THE SECOND PAPER [TM 24NOV20]
  fullTablePath <- file.path(folderColonization, "finalAbundanceMaps", 
                             paste0("abundanceTable_", year,".qs"))
  if (!file.exists(fullTablePath)){
    if (useFuture) plan("multicore")
    abund <- rbindlist(lapply(Species, function(sp){
      message(paste0("Calculating mean predicted values for ", sp, " (", which(Species == sp)," of ", 
                     length(Species),"; ", round(100*(which(Species == sp)/length(Species)), 
                                                 digits = 1),"%)"))
      fileNameFinalMap <- file.path(folderColonization, "finalAbundanceMaps", 
                                    paste0("abundanceMap_", sp, "_", year,".tif"))
      if (!file.exists(fileNameFinalMap)){
        stkPredictedFiles <- raster::stack(lapply(grepMulti(list.files(folderAbundance, full.names = TRUE, 
                                                                       pattern = sp, recursive = TRUE), 
                                                            patterns = c(year, ifelse(year == 2100, "V6a", "V4"))),
                                                  raster::raster))
        tic(paste0("Time elapsed calculate mean predicted values for ", sp, " for year ", year))
        averagedStk <- raster::calc(x = stkPredictedFiles, fun = mean, na.rm = TRUE)
        toc()
        probabilityPresence <- raster::raster(file.path(folderColonization,
                                                        paste0("probabilityPresence_", year,"_", 
                                                               ifelse(year == 2100,
                                                                      "LandR_SCFM_V6a_", 
                                                                      "LandR_SCFM_V4_"), sp,".tif"))) # To compare
        predAbund <- probabilityPresence*averagedStk
        writeRaster(predAbund, fileNameFinalMap, format = "GTiff")
      } else {
        predAbund <- raster::raster(fileNameFinalMap)
      }
      DT <- data.table::data.table(species = sp,
                                   year = year,
                                   totalAbundance = sum(predAbund[], na.rm = TRUE))
      return(DT)
    }))
    plan("sequential")
    qs::qsave(abund, fullTablePath)
  } else {
    message(paste0("Full table exists for year ", year, ". Returning."))
  }
  return(qs::qread(fullTablePath))
}