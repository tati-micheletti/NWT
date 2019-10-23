bootstrapPercentChanges <- function(folder = "/mnt/data/Micheletti/NWT/outputs/18JUL19/birdPredictionsV3_Fixed/", 
                                    years = c(2001, 2100), sampleSize = "auto", n = 100, shp = NULL){
  #shp needs to be a raster agreeing with the bird rasters, or a character string of a location os it in GDrive
  library("reproducible")
  source('/mnt/data/Micheletti/NWT/posthocFunctions/calculateSignificantChangesInBirds.R')
  source('/mnt/data/Micheletti/NWT/functions/calculatePercentageChanges.R') # ADD TO USEFun
  source('/mnt/data/Micheletti/NWT/posthocFunctions/whichSpeciesChange.R') # usefun
  source('/mnt/data/Micheletti/NWT/posthocFunctions/calculatePvalueOfRasters.R') # Usefun
  source('/mnt/data/Micheletti/NWT/modules/correlateRasters/R/prepStudyAreaForBirds.R')

  if (class(shp) == "character"){
    studyArea <- Cache(prepStudyAreaForBirds, studyArea = shp, 
                       folder = reproducible::checkPath(file.path(folder, "birdRTMEdehzhieRAS"), create = TRUE))
  } else {
    studyArea <- shp
  }
  fullTable <- lapply(1:n, function(repetition){
    message(crayon::yellow("Starting calculateSignificantChangesInBirds for repetition ", repetition, " TIME: ", Sys.time()))
    t <- Sys.time
    changesTable <- calculateSignificantChangesInBirds(folder = folder, years = years, 
                                                       sampleSize = sampleSize, studyArea = studyArea, 
                                                       repetition = repetition)
    message(crayon::green("FINISHED calculateSignificantChangesInBirds for repetition ", repetition, " ELAPSED: ", Sys.time() - t))
    t <- Sys.time()
    percentChange <- calculatePercentageChanges(changesTable = changesTable, column = "result")
    return(list(changesTable = changesTable, percentChange = percentChange))
  })
  changesTableList <- lapply(fullTable, '[[',"changesTable")
  constantSpecies <- whichSpeciesChange(changesTable = changesTableList)
  percChange <- data.table::rbindlist(lapply(fullTable, '[[',"percentChange"))
  if (!is.null(studyArea)){
    dt <- data.table::rbindlist(lapply(unique(percChange[["location"]]), function(locality){
      percChangeLoc <- percChange[location == locality,]
      dt <- calcICPercentChange(percChange = percChangeLoc)
      dt[["location"]] <- locality
      return(dt)
    })
    )
  } else {
    dt <- calcICPercentChange(percChange = percChange)
  }
  return(list(constantSpecies = constantSpecies, tableIC = dt))
}

calcICPercentChange <- function(percChange){
  increasedIC <- tryCatch({
    t.test(percChange[direction == "increased", value], conf.level = 0.95)$conf.int}, 
    error = function(e) return(NA))
  decreasedIC <- tryCatch({
    t.test(percChange[direction == "decreased", value], conf.level = 0.95)$conf.int}, 
    error = function(e) return(NA))
  noChangeIC <- tryCatch({
    t.test(percChange[direction == "no change", value], conf.level = 0.95)$conf.int}, 
    error = function(e) return(NA))
  dt <- data.table::data.table(direction = c("increased", "decreased", "noChange"),
                               lower95 = c(increasedIC[1], decreasedIC[1], noChangeIC[1]), 
                               upper95 = c(increasedIC[2], decreasedIC[2], noChangeIC[2]))
  return(dt)
}
