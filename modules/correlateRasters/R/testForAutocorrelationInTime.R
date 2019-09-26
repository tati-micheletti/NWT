testForAutocorrelationInTime <- function(corrDT, sampleSize){
  # Sample 10.000 points of RSF through time, and fit a lm(RSF ~ time). 
  # If TIME is significant, then RSF incorporates time and should be ok 
  # Clean data.table of NA's
  cleanDT <- na.omit(corrDT)
  # Subset 2000 pixels with data, get the whole TS for each of these pixels
  sbset <- cleanDT[, .SD[sample(.N, min(sampleSize, .N))], by = year]
  pix <- sample(x = unique(sbset$pixelID), size = sampleSize, replace = FALSE)
  sbset <- cleanDT[pixelID %in% pix]
  # Apply a lm for each subset
  seqYears <- length(unique(sbset$year))
  sbset[, TIME := 1:seqYears, by = pixelID]
  resultsLM <- sbset[, list(sig = summary(lm(formula = RSF ~ TIME, data = .SD))$coefficients["TIME", "Pr(>|t|)"]), by = pixelID]
  resultsLM[, sigSig := ifelse(sig < 0.05, 1, 0)]
  return(sum(resultsLM$sigSig)/NROW(resultsLM))
}