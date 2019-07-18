#' makeReclassifyMatrix creates a matrix to use for reclassification of rasters
#'
#' @param table data.frame or data.table that has the original classificaton 
#'              and the desired classification to replace the first.
#' @param originalCol character. Column in the table that identifies the original classification.
#' @param reclassifiedTo character. Column in the table that identifies the desired classification.
#' 
#' @return Matrix to use for reclassification of rasters
#'
#' @author Tati Micheletti
#' @export
#' @rdname makeReclassifyMatrix

makeReclassifyMatrix <- function(table, originalCol, reclassifiedTo){
  # Matrix with 3 columns: from X to Y reclass to Z
  m <- matrix(data = c(table[[originalCol]], table[[originalCol]], 
                       table[[reclassifiedTo]]), ncol = 3, byrow = FALSE)
  return(m)
}
