hotspotIndexModel <- function(corrDT){
  
  # CLEAR UP DATA.TABLE FOR NA's
  corrDT <- na.omit(corrDT)
  # To identify the best spots: which points have both values at their most maximized points?
  corrDT$RSF <- scale(corrDT$RSF)
  corrDT$richness <- scale(corrDT$richness)
  corrDT[,hotspotsIndex := RSF*richness*(RSF/(RSF+richness))]
  return(corrDT[,c("pixelID", "year", "hotspotsIndex")])
}
