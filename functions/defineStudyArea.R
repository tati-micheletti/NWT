#' @title
#' Preparing study area for Simulation of effects of climate change on fire regime: 
#' implications for BCR6 Caribou and landbird communities in the Northwest Territories project
#' 
#' @description
#' Downloads, reprojects, crops and masks to speficic areas in canada such as: 
#' BCR6, random areas, provinces and territories, or any of the last in the 
#' BCR6.
#'
#' @param testArea          Logical. Indicates if the test area should 
#'                          be anything other than `NULL`. Default is `NULL`. 
#' @param specificTestArea  A character string with a province or territory, or 'BCR6'. 
#'                          if BCR6 region (following Brandt et al., 2013) is wanted.
#'                          Default is `NULL`.
#' @param mapSubset         If specificTestArea is supplied as 'BCR6', this can be set 
#'                          as a character string with a province or territory that is contained 
#'                          in the BCR6 or 'Canada' if the whole Canadian BCR6 is wanted.
#'                          Default is `NULL`.
#' @param destinationPath   Path to where to save downloaded files. Default is `tempdir()`.
#' 
#' @param ...               Arguments to be passed to `prepInputs` or `Cache` (i.e. targetFile, 
#'                          cacheId, destinationPath, overwrite, etc.).
#'
#' @author Tati Micheletti and Geneviève Degré-Timmons
#' @export
#' @importFrom SpaDES.tools randomPolygon 
#' @importFrom reproducible prepInputs
#' @rdname defineStudyArea
#'
#' @examples
#' rp <- defineStudyArea(testArea = TRUE, specificTestArea = "BCR6", mapSubset = NULL) ## rp is the whole Boreal Taiga plains Bird conservation regions (BCR6)
#' rp <- defineStudyArea(testArea = TRUE, specificTestArea = "Northwest Territories", mapSubset = NULL) ## Northwest Territories
#' rp <- defineStudyArea(testArea = TRUE, specificTestArea = "BCR6", mapSubset = "Northwest Territories") ## Nortwest Territories inside BCR6

library(SpaDES.tools)
library(reproducible)

defineStudyArea <- function(testArea = NULL, specificTestArea = NULL, mapSubset = NULL, ...) {
  dots <- list(...)
  rP <- NULL
  if (any(is.null(testArea), (!is.null(testArea) &
                              testArea == FALSE))) {
    if (!is.null(specificTestArea)) {
      warning(crayon::yellow(paste0(
        "Test area is FALSE or NULL, but specificTestArea is not. Ignoring 'specificTestArea' and running the analysis without a study area. ",
        "To set a study area, use testArea == TRUE.")))
    } else {
      message(crayon::yellow("Test area is FALSE or NULL. Running the analysis without a study area."))
    }
  } else {
    if (is.null(specificTestArea)) {
      polyMatrix <- matrix(c(-79.471273, 48.393518), ncol = 2) 
      areaSize <- 10000000
      set.seed(1234)
      rP <- SpaDES.tools::randomPolygon(x = polyMatrix, hectares = areaSize) # Create Random polygon in southern Ontario
      message(crayon::yellow("Test area is TRUE, specificTestArea is 'NULL'. Cropping and masking to an area in south Ontario, Canada."))
    } else {
      if (specificTestArea == "BCR6") {
        if (is.null(mapSubset)) {
          message(crayon::yellow("Test area is TRUE, specificTestArea is 'BCR6', and mapSubset is NULL. Cropping and masking to the whole BCR6."))
          rP <- reproducible::prepInputs(url = "https://drive.google.com/open?id=1sEiXKnAOCi-f1BF7b4kTg-6zFlGr0YOH",
                                         ...)
        }
        if (!is.null(mapSubset) && mapSubset != "Canada") {
          sA <- reproducible::prepInputs(url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip",
                                         ...) %>%
            raster::subset(PRENAME %in% mapSubset)
          if (nrow(sA@data) == 0) {
            stop(paste0("There is no Canadian Province called ",
                        mapSubset,
                        ". Please provide a Canadian province name in English for subsetMap, ",
                        "or use 'NULL' (does not subset BCR6, dangerous when dealing with higher resolution)."))
          }
          dots <- list(...)
          dots$targetFile <- "BCR6.shp"
          dots$url <- "https://drive.google.com/open?id=1sEiXKnAOCi-f1BF7b4kTg-6zFlGr0YOH"
          dots$studyArea <- sA
          rP <- do.call(reproducible::prepInputs, dots)
          return(rP)
        } else {
        }
        if (!is.null(mapSubset) && mapSubset == "Canada") {
          message(crayon::yellow("Test area is TRUE. Cropping and masking to the Canadian BCR6."))
          rP <- reproducible::prepInputs(url = "https://drive.google.com/open?id=187R47guUko6p8WY-wCAlz_lPHKaGguGX",
                                         ...)
          return(rP)
        }
      } else {
        if (!is.null(specificTestArea)) {
          rP <- reproducible::prepInputs(url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip",
                                         ...) %>%
            raster::subset(PRENAME == specificTestArea)
          if (nrow(rP@data) == 0) {
            stop(paste0("There is no Canadian Province called ",
                        specificTestArea,
                        ". Please provide a Canadian province name in English for specificTestArea, ",
                        "use 'BCR6', or use 'NULL' (creates a random area in South Ontario, Canada)."))
          } else {
            message(crayon::yellow(paste0("Test area is TRUE. Cropped and masked to ",
                                          specificTestArea)))
            return(rP)
            
          }
        }
      }
    }
  }
  return(rP)
}
