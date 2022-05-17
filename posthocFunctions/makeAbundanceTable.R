makeAbundanceTable <- function(Species = c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "BAWW", "BBWA", "BBWO", 
                                           "BCCH", "BHCO", "BHVI", "BLPW", "BOCH", "BRBL", "BRCR", "BTNW", 
                                           "CAWA", "CHSP", "CORA", "COYE", "DEJU", "EAKI", "EAPH", "FOSP", 
                                           "GRAJ", "HETH", "HOLA", "LCSP", "LEFL", "LISP", "MAWA", "NOFL", 
                                           "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PISI", "PIWO", "PUFI", 
                                           "RBGR", "RBNU", "RCKI", "REVI", "RUGR", "RWBL", "SAVS", "SOSP", 
                                           "SWSP", "SWTH", "TEWA", "TRES", "WAVI", "WCSP", "WETA", "WEWP", 
                                           "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA", "YRWA"),
                               tableThreshold,
                               folderColonization, 
                               listOfRasters, # this will determine which scenario to run for 
                               year,
                               identifier = "",
                               noCorner,
                               useFuture = TRUE){
  # This compares 
  # NEEDS SOME REVIEW FOR THE SECOND PAPER [TM 24NOV20]
  # [UPDATE] Code revised on 18MAY21. Seemed fine after just fixing the probability
  # of presence from half of the plots to all 80 for 2011 ~TM
  # Here I compare models full non-CS 2011 to full CS 2100 to 
  # derive the effect of climate change on abundance.
  # [UPDATE 2] I have decided it was not a good idea to do what I was doing before. I have
  # now used the threshold table to cut off too low values and
  fullTablePath <- file.path(folderColonization, "finalAbundanceMaps", 
                             paste0("abundanceTable_", year, 
                                    identifier,".qs"))
  if (!file.exists(fullTablePath)){
    # if (useFuture) plan("multiprocess", workers = round(length(Species)/2, 0))
    tic("One species time to process: ")
    abund <- rbindlist(lapply(Species, function(sp){ # <~~~~~~~~~~~~~~~~~~~~ future_lapply
      message(paste0("Calculating mean predicted values for ", sp, " (", which(Species == sp)," of ", 
                     length(Species),"; ", round(100*(which(Species == sp)/length(Species)), 
                                                 digits = 1),"%)"))
      fileNameFinalMap <- file.path(folderColonization, "finalAbundanceMaps", 
                                    paste0("abundanceMap_", sp, "_", year, 
                                           identifier,
                                           ".tif"))
      if (!file.exists(fileNameFinalMap)){
        stkPredictedFiles <- unlist(listOfRasters[[sp]])
        message("Total number of rasters for ", sp, ": ", length(stkPredictedFiles))
        stkPredictedFiles_ok <- raster::stack(lapply(1:length(stkPredictedFiles), function(index){
              stk2100 <- stkPredictedFiles[[index]]
              thresholdVal <- tableThreshold[spec == sp, meanDensity]
              stk2100[stk2100[] < thresholdVal] <- 0  # ATTENTION: We are correcting only for small values. 
                                                      # Large values may still be a problem!
              message(paste0("Threshold of ", thresholdVal, " applied to ", sp, " to raster ", index))
              return(stk2100)
            }))
        tic(paste0("Time elapsed calculate mean predicted values for ", sp, " for year ", year))
        averagedStk <- raster::calc(x = stkPredictedFiles_ok,
                                    fun = mean, na.rm = TRUE)
        rm(stkPredictedFiles_ok)
        gc()
        toc()
            # TEMPORARY ####################
            # ### TODO >>>> HERE CUT THE CORNER
        averagedStk[] <- averagedStk[]
        averagedStk[is.na(noCorner)] <- NA # <~~~~~REMOVE
            # SAVE AGAIN!
            writeRaster(6.25*averagedStk,
                        filename = fileNameFinalMap,
                        overwrite = TRUE, format = "GTiff")
            rm(averagedStk)
            gc()
            message(crayon::green(paste0("Corner removed for ", 
                                         sp)))
            # TEMPORARY ####################
            predAbund <- raster::raster(fileNameFinalMap)
      } else {
        predAbund <- raster::raster(fileNameFinalMap)
      }
      DT <- data.table::data.table(species = sp,
                                   year = year,
                                   totalAbundance = sum(predAbund[], na.rm = TRUE))
      return(DT)
    }))
    toc()
    # plan("sequential")
    qs::qsave(abund, fullTablePath)
  } else {
    message(paste0("Full table exists for year ", 
                   year, ". Returning."))
  }
  return(qs::qread(fullTablePath))
}