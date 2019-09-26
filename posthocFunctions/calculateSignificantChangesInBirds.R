calculateSignificantChangesInBirds <- function(folder, years, species = NULL, pixelBased = TRUE,
                                               sampleSize = "auto", redFactorTimes = 15, 
                                               studyArea = NULL, repetition = NULL){ # limited to 2 years!
  
  if (is.null(species)){
    spFiles <- usefun::grepMulti(x = list.files(folder), patterns = c(2001,".tif"))
    splitted <- sapply(strsplit(x = spFiles, split = "CSpredicted"), '[[', 2)
    species <- usefun::substrBoth(strng = splitted, howManyCharacters = 4, fromEnd = FALSE)
  }
  if (length(years)>2) stop("Currently this function only compares 2 years")
  tableOfChanges <- data.table::rbindlist(lapply(species, function(bird){
    birdInyears <- lapply(years, function(y){
      rasPath <- usefun::grepMulti(x = list.files(folder, recursive = TRUE, full.names = TRUE), patterns = c(bird, y))
      rasValue <- data.table::data.table(raster::getValues(raster::raster(rasPath)))
      return(rasValue)
    })
    names(birdInyears) <- years
    dtForTest <- usefun::cbindFromList(birdInyears)
    if (!is.null(studyArea)){
      rasPath <- usefun::grepMulti(x = list.files(folder, recursive = TRUE, full.names = TRUE), patterns = c(bird)) # JUST A TEMPLATE!
      source('/mnt/data/Micheletti/NWT/modules/correlateRasters/R/prepStudyAreaForBirds.R') # [ FIX ] usefun
      # location <- reproducible::Cache(prepStudyAreaForBirds, studyArea = studyArea, folder = folder, RTMpath = rasPath[1],
      #                                 userTags = c("object:location", "purpose:Edehzhie"))
      dtForTest <- cbind(dtForTest, data.table::data.table(location = studyArea))
      uniqueLocations <- unique(dtForTest$location)[!is.na(unique(dtForTest$location))]
      byLocationModelList <- lapply(uniqueLocations, function(locality){
        dtForTestloc <- dtForTest[location == locality,]
        t <- Sys.time()
        mod <- calculatePvalueOfRasters(dtForTest = dtForTestloc,
                                        pixelBased = pixelBased, sampleSize = sampleSize, 
                                        species = species, 
                                        redFactorTimes = redFactorTimes,  bird = bird)
        message(crayon::white("Finished calculations for ", bird,
                              " (locality ", locality, " repetition ", repetition,") ",
                              "Elapsed Time: ", Sys.time() - t))
        return(mod)
      })
      names(byLocationModelList) <- uniqueLocations
      dt <- data.table::rbindlist(lapply(uniqueLocations, function(loc){
        direction <- ifelse(byLocationModelList[[loc]]$p.value>=0.05,"no change",
                            ifelse(byLocationModelList[[loc]]$mean1 < byLocationModelList[[loc]]$mean2, 
                                   "increased","decreased"))
        dt <- data.table::data.table(species = bird, tTest = byLocationModelList$p.value, 
                                     result = direction, location = loc)
      }))
    } else {
      r <- calculatePvalueOfRasters(dtForTest = dtForTest, 
                                    pixelBased = pixelBased, sampleSize = sampleSize, 
                                    species = species, 
                                    redFactorTimes = redFactorTimes, bird = bird)
      direction <- ifelse(r$p.value>=0.05,"no change",
                          ifelse(r$mean1 < r$mean2, "increased","decreased"))
      dt <- data.table::data.table(species = bird, tTest = r$p.value, result = direction)
    }
    return(dt)
  })
  )
  return(tableOfChanges)
}