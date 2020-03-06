getFirePolygons <- function(years, studyArea, pathInputs, version = NULL){
  if (is.null(version)){
    version <- c(20191129, 20190919)
  }
  firePolygonsList <- lapply(years, function(ys){
    tryCatch({
      url <- paste0("https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_", ys, "_r9_", 
                    version[1],".zip")
      
      polyYear <- prepInputs(url = url, studyArea = studyArea,
                             destinationPath = pathInputs,
                             userTags = c("object:firePolygons_NBAC", paste0("year:", ys), 
                                          paste0("version:", version[1])))
      return(polyYear)
    }, error = function(e){
      url <- paste0("https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_", ys, "_r9_", version[2],".zip")
    polyYear <- prepInputs(url = url, studyArea = studyArea,
                           destinationPath = pathInputs,
                           userTags = c("object:firePolygons_NBAC", paste0("year:", ys), 
                                        paste0("version:", version[2])))
    return(polyYear)
    })
  names(firePolygonsList) <- paste0("Years", years)
  })
}