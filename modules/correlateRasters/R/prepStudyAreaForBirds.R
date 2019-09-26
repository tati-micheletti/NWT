prepStudyAreaForBirds <- function(studyArea, folder){
  if (is(studyArea, "character")){
    studyArea <- prepInputs(url = studyArea, targetFile = "birdRTMEdehzhie.tif",
                            destinationPath = folder,
                            userTags = "birdRTMEdehzhie", filename2 = "birdRTMEdehzhie")
  }
  location <- raster::getValues(studyArea)
  location[location == 0] <- NA
  return(location)
  }