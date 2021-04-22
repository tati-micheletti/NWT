mods <- rbindlist(lapply(names(sim$birdModels), FUN = function(sp){
  MOD <- sim$birdModels[[sp]]
  Abundance <- MOD$gbm.call$dataframe[["ABUND"]]
  counts <- sum(Abundance != 0)
  DT <- data.table(species = sp,
                   counts = counts)
  return(DT)
}))
