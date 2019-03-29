grepMulti <- function(x, patterns) {
  i <- sapply(x, function(fn) all(sapply(X = patterns, FUN = grepl, fn)))
  return(x[i])
}
