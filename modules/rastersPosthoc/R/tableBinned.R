tableBinned <- function(vec, bins, plot = FALSE) {
  
  freq <- hist(vec, breaks = bins, include.lowest = TRUE, plot = plot)
  ranges <- paste(head(freq$breaks,-1), freq$breaks[-1], sep = " - ")
  
  return(data.table::data.table(range = ranges, frequency = freq$counts))
  
}
