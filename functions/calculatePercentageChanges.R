calculatePercentageChanges <- function(changesTable, column){
  if ("location" %in% names(changesTable)){
    dt <- data.table::rbindlist(lapply(unique(changesTable$location), function(locality){
      changesVector <- table(changesTable[location == locality, ..column,])
      dt <- data.table::data.table(direction = names(changesVector),
                                   value = as.numeric(changesVector),
                                   percent = 100*(as.numeric(changesVector)/NROW(changesTable[location == locality, ..column,])), 
                                   location = locality)
    }))
  } else {
    changesVector <- table(changesTable[, ..column])
    dt <- data.table::data.table(direction = names(changesVector),
                                 value = as.numeric(changesVector),
                                 percent = 100*(as.numeric(changesVector)/NROW(changesTable)))
  }
  return(dt)
}