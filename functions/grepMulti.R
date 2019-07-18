#' grepMulti works similarly to \code{grepl}, but for multiple patterns and returning the object.
#'
#' @param x object where to look for patterns.
#'
#' @param patterns Character vector of patterns to look for objects.
#'                
#' @return The objects with specified patterns combined
#' 
#' @author Tati Micheletti
#' @export
#' @rdname grepMulti

grepMulti <- function(x, patterns) {
  rescued <- sapply(x, function(fun) all(sapply(X = patterns, FUN = grepl, fun)))
  return(x[rescued])
}