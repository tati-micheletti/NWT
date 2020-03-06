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
                             alsoExtract = "similar",
                             archive = paste0("nbac_", ys, "_r9_",version[1],".zip"),
                             targetFile = paste0("nbac_", ys, "_r9_",version[1],".shp"),
                             userTags = c("object:firePolygons_NBAC", paste0("year:", ys), 
                                          paste0("version:", version[1])))
      return(polyYear)
    }, error = function(e){
      url <- paste0("https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_", ys, "_r9_", version[2],".zip")
    polyYear <- prepInputs(url = url, studyArea = studyArea,
                           destinationPath = pathInputs,
                           alsoExtract = "similar",
                           archive = paste0("nbac_", ys, "_r9_",version[2],".zip"),
                           targetFile = paste0("nbac_", ys, "_r9_",version[2],".shp"),
                           userTags = c("object:firePolygons_NBAC", paste0("year:", ys), 
                                        paste0("version:", version[2])))
    return(polyYear)
    })
  })
  names(firePolygonsList) <- paste0("Year", years)
  return(firePolygonsList)
}
