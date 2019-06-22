substrBoth <- function(x, n, fromEnd = TRUE){
  if (fromEnd) return(substr(x, nchar(x)-n+1, nchar(x))) else
  return(substr(x = x, 1, stop = nchar(x)-n))
}

