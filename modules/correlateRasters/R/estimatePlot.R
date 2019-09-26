estimatePlot <- function(fittedDT, byLocation = FALSE){
  if (byLocation){
    uniqueLocations <- names(fittedDT)
    byLocationDT <- rbindlist(lapply(uniqueLocations, function(locality){
      fittedDTloc <- fittedDT[[locality]]
      dt <- createPlotTable(fittedDT = fittedDTloc, byLocation = TRUE, locality = locality)
      return(dt)
    }))
  } else {
    return(createPlotTable(fittedDT = fittedDT, byLocation = FALSE))
  }
  return(byLocationDT)
}