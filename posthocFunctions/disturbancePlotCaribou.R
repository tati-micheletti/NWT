disturbancePlotCaribou <- function(folderData,
                                   typeSim){
  
  library("ggplot2")
  library("usefun")
  library("LandR")
  library("reproducible")
  library("SpaDES.tools")
  folder <- folderData #"04JUL19" #18JUN19_CS_SCFM" #"08JUN19" #"29JUN19" 12JUL19 --> NoCS  12JUL19 --> CS
  simul <- typeSim
  folderPath <- paste0("/mnt/data/Micheletti/NWT/outputs/", folder,"/")
  
  cohorDataList <- Cache(bringObjectTS, path = folderPath, rastersNamePattern = "cohortData")
  pixelGroupList <- Cache(bringObjectTS, path = folderPath, rastersNamePattern = "pixelGroupMap")
  
  # MAX AGE
  maxAge <- data.table::rbindlist(lapply(X = names(cohorDataList), function(index){
    cohort <- cohorDataList[[index]]
    pixelGroup <- pixelGroupList[[index]]
    a <- cohort[, list(maxAge = max(age, na.rm = TRUE)), by = "pixelGroup"]
    r <- rasterizeReduced(a, pixelGroup, "maxAge", "pixelGroup")
    return(list(meanAge = mean(r[], na.rm = TRUE),
                minAge = min(r[], na.rm = TRUE),
                maxAge = max(r[], na.rm = TRUE),
                medianAge = median(r[], na.rm = TRUE),
                years = as.numeric(usefun::substrBoth(strng = index, 
                                                      howManyCharacters = 4, 
                                                      fromEnd = TRUE))))
  }))
  agePlot <- ggplot2::ggplot(data = maxAge, aes(x = years)) +
    geom_ribbon(aes(ymin = 40, ymax = 60), alpha = 0.3, fill = "red") + # Old burn
    geom_ribbon(aes(ymin = 0, ymax = 40), alpha = 0.3, fill = "yellow") + # Recent burn
    geom_ribbon(aes(ymin = 60, ymax = maxAge), alpha = 0.3, fill = "green") + # No burn
    geom_line(aes(y = meanAge), size = 1.2) +
    geom_line(aes(y = medianAge), size = 1.2, linetype = "dashed") +
    ggtitle(paste0("Forest Age")) +
    theme(legend.position = "bottom")
return(agePlot)
}
