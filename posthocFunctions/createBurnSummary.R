createBurnSummary <- function(folderData, 
                              typeSim){
  
  library("data.table")
  library("usefulFuns")
  library("LandR")
  library("reproducible")
  library("raster")
  folder <- folderData #"04JUL19" #18JUN19_CS_SCFM" #"08JUN19" #"29JUN19" 12JUL19 --> NoCS  12JUL19 --> CS
  simul <- typeSim
  folderPath <- paste0("/mnt/data/Micheletti/NWT/outputs/", folder,"/")
  
  burnDT <- bringObjectTS(path = folderPath, rastersNamePattern = "burnDT")
  burnSummary <- rbindlist(lapply(X = names(burnDT), FUN = function(yr){
    burnDTYear <- burnDT[[yr]]
    tempDT <- burnDTYear[, .(.N), by = "initialPixels"]
    tempDT$year <- as.numeric(usefulFuns::substrBoth(string = yr, 
                                         howManyCharacters = 4, fromEnd = TRUE))
    fireRegimeRas <- readRDS(file = file.path(folderPath, grepMulti(x = list.files(folderPath), 
                                                                    patterns = "fireRegimeRas")))
    tempDT$areaBurned <- tempDT$N * prod(raster::res(fireRegimeRas))/10000 # Meters to ha
    tempDT$polyID <- fireRegimeRas[tempDT$initialPixels]
    setnames(tempDT, c("initialPixels"), c("igLoc"))
    return(tempDT)
  })
  )
  totAreaBurned <- burnSummary[, totAreaBurned := sum(areaBurned), by = "year"]
  numberFires <- burnSummary[, numberFires := length(N), by = "year"]
  library("ggplot2")
  p1 <- ggplot(data = totAreaBurned, aes(x = year, y = totAreaBurned)) +
    geom_bar(aes(y = totAreaBurned), fill = "red4", stat = "identity") +
    geom_line(aes(y = numberFires*10^5), col = "black", size = 2) +
    labs(x = "Year", y = paste0("Total Area Burned (red) \n Total Number of Fires * 10^5 (black)"), title = paste0("Total area burned")) +
    theme(legend.position = "none")
  print(p1)
  return(p1)
}