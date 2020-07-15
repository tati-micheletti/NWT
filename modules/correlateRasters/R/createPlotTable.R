createPlotTable <- function(fittedDT, byLocation = FALSE, locality = NULL){
  plotTable <- rbindlist(lapply(X = names(fittedDT), FUN = function(yr){
    sm <- summary(fittedDT[[yr]])
    dt <- data.table::data.table(year = usefulFuns::substrBoth(yr, 4, TRUE),
                                 richnessEstimate = sm$coefficients["richness", "Estimate"],
                                 pValue = sm$coefficients["richness", "Pr(>|t|)"])
    if (byLocation){
      dt <- cbind(dt, data.table(location = locality))
    }
    return(dt)
  }))
}