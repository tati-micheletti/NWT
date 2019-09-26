coreRSFRichnessFitting <- function(corrDT){
  # CLEAR UP DATA.TABLE FOR NA's
  corrDT <- na.omit(corrDT)
  # 1. We run a model for each year.
  modelAllYears <- lapply(X = unique(corrDT[["year"]]), FUN = function(yr){
    sbset <- corrDT[year == yr]
    sbset$RSF <- scale(sbset$RSF)
    sbset$richness <- scale(sbset$richness)
    slpCoef <- lm(RSF ~ richness, data = sbset)
    return(slpCoef)
  })
  names(modelAllYears) <- unique(corrDT[["year"]])
  return(modelAllYears)
}