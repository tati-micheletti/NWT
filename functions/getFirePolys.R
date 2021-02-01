#' Download and prepare fire data from National Fire Database
#'
#' @param years DESCRIPTION NEEDED
#' @param studyArea DESCRIPTION NEEDED
#' @param pathInputs DESCRIPTION NEEDED
#' @param version DESCRIPTION NEEDED
#'
#' @return DESCRIPTION NEEDED
#'
#' @export
#' @importFrom reproducible prepInputs
getFirePolys <- function(years, studyArea, pathInputs,
                            version = NULL) {
  if (is.null(version)) {
    version <- c(20191129, 20190919)
  }
  polyYear <- NULL
  firePolygonsList <- lapply(years, function(ys) {
    vCounter <- 1
    while (any(is(polyYear, "error"), is.null(polyYear))){
      message(crayon::blue(paste0("Trying version ", version[vCounter], 
                                      " for year ", ys)))
      if (vCounter > length(version))
        stop(paste0("Please pass the correct version dates for fire polygon's data by checking on ",
                    "https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/"))
      polyYear <- tryCatch({
        url <- paste0("https://cwfis.cfs.nrcan.gc.ca",
                      "/downloads/nbac/nbac_", ys, 
                      "_r9_", version[vCounter], ".zip")
        polyYear <- reproducible::prepInputs(
          url = url, studyArea = studyArea,
          destinationPath = pathInputs,
          alsoExtract = "similar",
          archive = paste0("nbac_", ys, "_r9_", version[vCounter], ".zip"),
          targetFile = paste0("nbac_", ys, "_r9_", version[vCounter], ".shp"),
          userTags = c(
            "object:firePolygons_NBAC", paste0("year:", ys),
            paste0("version:", version[vCounter])
          ))
        return(polyYear)
      },
      error = function(e) e)
      vCounter <- vCounter + 1
    }
    polyYear[seq_len(NROW(polyYear)), "POLY_HA"] <- rgeos::gArea(polyYear, byid = TRUE) / (1e4)
    return(polyYear)
  })
  names(firePolygonsList) <- paste0("Year", years)
  return(firePolygonsList)
}
