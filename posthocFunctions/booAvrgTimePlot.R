booAvrgTimePlot <- function(dtCS, dtNoCS, upload, outputFolder){
  dt <- rbind(dtCS, dtNoCS)
  library("ggplot2")
  p <- ggplot(data = dt, aes(x = year, y = average, ymin = (average - IC), ymax = (average + IC), group = scenario)) +
    geom_line(aes(color = scenario)) +
    geom_ribbon(aes(fill = scenario), alpha = 0.5) +
    theme_bw()
  pngFig <- file.path(outputFolder, "averageCaribouSelection.png")
  png(pngFig, width = 700, height = 480)
  print(p)
  dev.off()
  if(upload)
    googledrive::drive_upload(pngFig,
                              path = googledrive::as_id("1lhhIr_865VZI05mhiQ91iN8MfDGn5PV7"))
  return(p)
}