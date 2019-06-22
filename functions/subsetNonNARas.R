subsetNonNARas <- function(ras, N){
  ras <- getValues(ras)
  ras <- data.table::data.table(ID = 1:length(ras), val = ras)
  sbset <- ras[!is.na(val), ID][1:N]
  return(sbset)
}