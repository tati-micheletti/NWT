#' createEquation writes an equation based on a data.frame (or data.table), bootstraping a 
#' set of covariates around the standard error present in the table.
#'
#' @param model data.frame. Needs to have as column names 'Coefficient', and 'Value' and 'StdErr' for a 
#'              given parameter. The model table NEEDS to have at least two rows, one being the 'Intercept'
#'              as 'Coefficient'. If 'Intercept' is not found, it enters \code{browser()} mode.
#'
#' @param replicates numeric. Number of repetitions to be added to the bootstrapping.
#'                
#' @return As with \code{\link[archivist]{cache}}, returns the value of the
#' function call or the cached version (i.e., the result from a previous call
#' to this same cached function with identical arguments).
#'
#' @author Tati Micheletti
#' @rdname createEquation

createEquation <- function(model, replicates = 100){
  howManyCovars <- length(model$Coefficient) - 1
  if (!"Intercept" %in% model$Coefficient){
    message("The model intercept ('Intercept') couldn't be found in the table. Review your data.")
    browser()
  }
  coeffs <- model[Coefficient != "Intercept", Coefficient]
  eq <- model[Coefficient == "Intercept", Value]
  for (coef in coeffs){
    eq <- paste0(eq, " + ", coef, " * ", "rnorm(n = ", replicates, ", mean = ", model[Coefficient == coef, Value], ", 
                 sd = ", model[Coefficient == coef, StdErr], ")")
  }
  return(eq)
}