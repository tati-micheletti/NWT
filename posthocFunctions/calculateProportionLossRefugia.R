calculateProportionLossRefugia <- function(species, currentDistribution, 
                                           futureDistribution, RTM){
  message(paste0("calculating proportion change refugia for ", species)) 
  currentDistribution[is.na(RTM)] <- NA # Need to do that for both rasters!
  futureDistribution[is.na(RTM)] <- NA # Need to do that for both rasters!
  currentDistributionSum <- sum(currentDistribution[], na.rm = TRUE)
  futureDistributionSum <- sum(futureDistribution[], na.rm = TRUE)
  changeInTerritory <- (futureDistributionSum-currentDistributionSum)/currentDistributionSum
  return(changeInTerritory)
}
