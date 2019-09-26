fitTheModel <- function(corrDT, byLocation = FALSE){
  if (byLocation){
    uniqueLocations <- unique(corrDT$location)[!is.na(unique(corrDT$location))]
    byLocationModelList <- lapply(uniqueLocations, function(locality){
      corrDTloc <- corrDT[location == locality,]
      mod <- coreRSFRichnessFitting(corrDT = corrDTloc)
      return(mod)
    })
  } else {
    return(coreRSFRichnessFitting(corrDT = corrDT))
  }
  names(byLocationModelList) <- paste0("location", uniqueLocations)
  return(byLocationModelList)
}

#   # FOR EACH PIXEL  ::: Abandoned. didn't work!
# slopeValues <- rbindlist(lapply(X = unique(corrDT[["pixelID"]]), FUN = function(pix){
#   sbset <- corrDT[pixelID == pix]
#   if (fastLM){
#     slpCoef <- RcppArmadillo::fastLmPure(X = cbind(1, sbset$richness), y = sbset$RSF) # Original formula was way slower: lm(x ~ times, data = dfX,  na.action = na.omit)
#     coef <- slpCoef$coefficients[2]
#     pVal <- 2*pt(abs(slpCoef$coefficients/slpCoef$stderr), slpCoef$df.residual, lower.tail=FALSE)[2]
#   } else {
#     browser()
#     
# # COULDN'T MAKE THE SUBSETED MODEL WORK... TRIED THE WHOLE DATA MODEL.
#     # STILL DIDN"T WORK. Need to figure out how to account
#     # for temporal autocorrelation in the RSF ~ richness model!
#     
#     
#     # need to rescale the variables?
#     sbset$RSF <- scale(sbset$RSF)
#     if (length(unique(sbset$richness)) != 1)
#       sbset$richness <- scale(sbset$richness)
#     # slpCoef <- lme(fixed = RSF ~ richness, data = sbset, random = pixelID)
#     # corrDT
#     newCorrDT <- corrDT
#     newCorrDT$RSF <- scale(newCorrDT$RSF)
#     newCorrDT$richness <- scale(newCorrDT$richness)
#         slpCoef <- lme(fixed = RSF ~ richness + pixelID, data = newCorrDT, random = ~ 1, correlation = corAR1())
#     # coef <- 
#     # pVal <- 
#   }
#   return(data.table(pixelID = pix, coef = coef, pVal = pVal))
# }))