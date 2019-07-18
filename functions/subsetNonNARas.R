#' subsetNonNARas get all the values of a raster and remove the NA's, keeping pixel ID.
#'
#' @param ras RasterLayer.      
#' @param N numeric. If passed, subsets N pixels from the raster. If NULL, 
#'          returns the whole non-NA raster values. Default is NULL.
#' 
#' @return data.table of pixel values and pixel ID
#'
#' @author Tati Micheletti
#' @export
#' @importFrom data.table data.table
#' @importFrom raster ncell getValues
#' 
#' @rdname subsetNonNARas
#' 
subsetNonNARas <- function(ras, N = NULL){
  if (is.null(N)) N <- raster::ncell(ras)
  ras <- getValues(ras)
  ras <- data.table::data.table(ID = 1:length(ras), val = ras)
  sbset <- ras[!is.na(val), ID][1:N]
  return(sbset)
}