whichSpeciesChange <- function(changesTable){
  if (!is(changesTable, "list"))
    stop("changesTable needs to be a list, even if of only one element")
  listOfChanges <- lapply(1:length(changesTable), function(repetition){
      tb <- changesTable[[repetition]]
      if ("location" %in% names(tb)){
        uniqueLocations <- unique(tb$location)
        dt  <- lapply(uniqueLocations, function(locality){
          dt <- whichSpeciesChangeDT(tb = tb[location == locality])
          return(dt)
        })
        names(dt) <- paste0("location", uniqueLocations)
      } else {
       dt <- whichSpeciesChangeDT(tb = tb)
      }
    return(dt)
  })
  names(listOfChanges) <- paste0("repetition", (1:length(changesTable)))
  # Reorder the three elements, needs to make a reduce for each
  if (is(changesTable, "list")){
    if ("location" %in% names(changesTable[[1]])){
      tb <- changesTable[[1]]
      uniqueLocations <- unique(tb$location)
      allLocations <- lapply(uniqueLocations, function(locality) {
        listOfChangesLocal <- lapply(listOfChanges, '[[', paste0("location", locality))
        increased <- lapply(listOfChangesLocal, '[[', "increased")
        decreased <- lapply(listOfChangesLocal, '[[', "decreased")
        noChange <- lapply(listOfChangesLocal, '[[', "noChange")
        
        increasedConsistent <- Reduce(intersect, increased)
        decreasedConsistent <- Reduce(intersect, decreased)
        noChangeConsistent <- Reduce(intersect, noChange)
        
        return(list(increased = increasedConsistent,
                    decreased = decreasedConsistent,
                    noChange = noChangeConsistent))
      })
  names(allLocations) <- paste0("location", uniqueLocations)
  return(allLocations)
}
  } else {
  increased <- lapply(listOfChanges, '[[', "increased")
  decreased <- lapply(listOfChanges, '[[', "decreased")
  noChange <- lapply(listOfChanges, '[[', "noChange")
  
  increasedConsistent <- Reduce(intersect, increased)
  decreasedConsistent <- Reduce(intersect, decreased)
  noChangeConsistent <- Reduce(intersect, noChange)
  
  return(list(increased = increasedConsistent, decreased = decreasedConsistent, noChange = noChangeConsistent))
  }
}

whichSpeciesChangeDT <- function(tb){
  inc <- tb[result == "increased", species]
  dec <- tb[result == "decreased", species]
  noChange <- tb[result == "no change", species]
  dt <- list(increased = inc, decreased = dec, noChange = noChange)
  return(dt)
}
