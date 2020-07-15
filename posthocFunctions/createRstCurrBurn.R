createRstCurrBurn <- function(burnDT, rstLCC){ # vegMap == LCC05
  rstCurrentBurn <- rstLCC #This preserves NAs
  names(rstCurrentBurn) <- NULL
  rstCurrentBurn[!is.na(rstCurrentBurn)] <- 0 #reset annual burn
  rstCurrentBurn[burnDT$pixels] <- 1 #update annual burn
  return(rstCurrentBurn)
}
