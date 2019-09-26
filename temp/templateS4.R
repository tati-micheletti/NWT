#' classifyWetlands classifies wetlands (really!) using the wetlands layer set as input and a either LCC05 or LCC2010
#'
#' @param LCC numeric. 2005 (250m resolution) or 2010 (30m resolution) landcover  rasters.
#'
#' @param wetLayerInput Character vector of objects to be digested. This is only applicable
#'                      if there is a list, environment (or similar) named objects
#'                      within it. Only this/these objects will be considered for caching,
#'                      i.e., only use a subset of
#'                      the list, environment or similar objects.
#'                
#' @return As with \code{\link[archivist]{cache}}, returns the value of the
#' function call or the cached version (i.e., the result from a previous call
#' to this same cached function with identical arguments).
#'
#' @author Tati Micheletti
#' @export
#' @importFrom LandR prepInputsLCC 
#' @importFrom raster raster projectRaster extract
#' @importFrom data.table data.table 
#' @importFrom reproducible prepInputs postProcess Require
#' @rdname classifyWetlands