correlationModel <- function(corrDT){
  
  corrDT <- na.omit(corrDT)
  # JUST HAD AN IDEA YESTERDAY:
  # 1. We run a correlation analysis on all paired pixels RSF and richness for each year.
  # 2. We extract the R2 of these: and the regression coefficient (linear model?)
  # 3. This way we know the direction of the relationship, and the strength. We can compare them as time goes by
  # how it changes. 
  corrAllYears <- lapply(X = unique(corrDT[["year"]]), FUN = function(yr){
    sbset <- corrDT[year == yr]    
    corrCoef <- cor(sbset[,c("RSF", "richness")], use="complete.obs", method="spearman")
    return(corrCoef)
  })
  names(corrAllYears) <- unique(corrDT[["year"]])
  return(corrAllYears)
}