groupSAR <- function(speciesList){
  if (is.null(allBirds))
    return(NULL)
  SARlist <- c("HASP", "OSFL", "HOGR", "EVGR", "REKN", "CONI", "SEOW", "YEAR", "BBSA", "CAWA")
  spList <- list()
  for (BIRD in speciesList){
    if (BIRD %in% SARlist){
      spList[["SAR"]] <- c(spList[["SAR"]], BIRD)
    } else {
      spList[["nonSAR"]] <- c(spList[["nonSAR"]], BIRD)
    }
  }
  
  return(spList)
  
}
