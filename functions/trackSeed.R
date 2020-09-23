trackSeed <- function(clear = TRUE){
  if (clear){
    set.seed(NULL)
    seed <- sample(1e10, 1)
    set.seed(seed)
    print(paste("random seed:", seed))
  } else {
    fseed <- file.path("seed.rds")
    fseed2 <- raster::extension(fseed, "txt")
    if (file.exists(fseed)) {
      seed <- readRDS(fseed)
    } else {
      seed <- sample(1e10, 1)
      saveRDS(seed, fseed)
      print(paste("random seed:", seed))
      set.seed(seed)
    }
  }
    cat(paste("Setting seed on run ", definedRun$whichReplicate, 
              " for ", runName,":", 
              seed, " [", 
              Sys.time(),"]"), 
        file = fseed2, sep = "\n")
    SpaDES.core::writeRNGInfo(fseed2, append = TRUE)
}
