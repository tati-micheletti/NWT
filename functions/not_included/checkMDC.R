# Check weather (MDC)
checkMDC <- function(annualStacks, firePolys){
  # annualStacks is in objects
  # firePolys is in sim
  dt <- data.table( 
    year = names(annualStacks),
    meanAnnualMDC = unlist(lapply(annualStacks, function(x) median(asInteger(x$weather[]/10)*10L, na.rm = TRUE))),
    AnnualAreaBurned = unlist(lapply(firePolys, function(x) sum(asInteger(x$POLY_HA/10)*10L, na.rm = TRUE))))
  plot(dt[,-1], pch = "", main = "NWT fire size by MDC and fire year")
  text(dt[,-1], labels = gsub("^..", "", dt$year))
  lm1 <- lm(AnnualAreaBurned ~ meanAnnualMDC, data = dt)
  abline(lm1)
}
