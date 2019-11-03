makeRastersSummary <- function(listOfRasters, 
                               years = c(2000, 2100)){
  wholeTable <- rbindlist(lapply(X = names(listOfRasters), FUN = function(simul){
    simulLists <- listOfRasters[[simul]]
    speciesList <- rbindlist(lapply(X = names(simulLists), FUN = function(species){
      spStack <- simulLists[[species]]
      stkVals <- data.table::data.table(raster::getValues(spStack))
      dt <- rbindlist(lapply(X = names(stkVals), FUN = function(y){
        r <- stkVals[, ..y]
        dt <- data.table(scenario = simul,
                         species = species,
                         year = as.numeric(substrBoth(strng = y, howManyCharacters = 4, fromEnd = TRUE)),
                         mean = mean(r[[1]], na.rm = TRUE),
                         min = min(r[[1]], na.rm = TRUE),
                         max = max(r[[1]], na.rm = TRUE),
                         median = median(r[[1]], na.rm = TRUE),
                         sd = sd(r[[1]], na.rm = TRUE))
        return(dt)
      }))
      return(dt)
    }))
    return(speciesList)
  }))
  return(wholeTable)
}