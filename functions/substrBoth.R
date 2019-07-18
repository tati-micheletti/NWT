#' substrBoth get a sub-string based on the number of characters and the side to start
#'
#' @param string String from which to grab a subset      
#' @param howManyCharacters numeric. How many characters should be returned in the sub-string?
#' @param fromEnd logical. Default is TRUE. Should te subset start in the end of the string?
#' 
#' @return character string of the subset.
#'
#' @author Tati Micheletti
#' @export
#' 
#' @rdname substrBoth

substrBoth <- function(string, howManyCharacters, fromEnd = TRUE){
  if (fromEnd) return(substr(string, nchar(string)-string+1, nchar(string))) else
  return(substr(string = string, 1, stop = nchar(string)-string))
}

