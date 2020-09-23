trackSeed <- function(clear = TRUE, 
                      replic = "UNKNOWN", 
                      runName = "UNKNOWN"){
  if (clear){
    set.seed(NULL)
    seed <- sample(1e9, 1)
    set.seed(as.integer(seed))
    print(paste("random seed:", seed))
    fseed <- file.path("seed.rds")
    fseed2 <- raster::extension(fseed, "txt")
  } else {
    fseed <- file.path("seed.rds")
    fseed2 <- raster::extension(fseed, "txt")
    if (file.exists(fseed)) {
      seed <- readRDS(fseed)
    } else {
      seed <- sample(1e9, 1)
      saveRDS(seed, fseed)
      print(paste("random seed:", seed))
      set.seed(seed)
    }
  }
    cat(paste("Setting seed on run ", replic, 
              " for ", runName,":", 
              seed, " [", 
              Sys.time(),"]"), 
        file = fseed2, sep = "\n")
    SpaDES.core::writeRNGInfo(fseed2, append = TRUE)
}
