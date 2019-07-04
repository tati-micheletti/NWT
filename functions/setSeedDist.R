setSeedDist <- function(speciesTable, param, facMult, species){
  spTb <- lapply(X = species, FUN = function(sp){
    speciesTable[species == sp, (param) := eval(parse(text = param))*facMult]
  })
  return(spTb[[1]])
}
