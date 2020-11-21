# CHECK ATA and CMI
library(raster)
library(reproducible)
library(ggplot2)
library(rasterVis)

outPath <- "~/projects/NWT/outputs/posthoc/climate"
rasterToMatch <- raster::raster(file.path(Paths$inputPath, "flammableRTM.tif"))

runName <- "NWT_BCR6"
source("1_generalSetup.R")
source("2_generatingInputs.R") # Get studyArea!

CMI <- raster::stack(file.path(Paths$modulePath[1], "gmcsDataPrep/data", "Canada3ArcMinute_CCSM4_85_CMI2011-2100.grd"))
# "climate moisture deficit at year X"
ATA <- raster::stack(file.path(Paths$modulePath[1], "gmcsDataPrep/data", "Can3ArcMinute_CCSM4_RCP85_ATA2011-2100.grd"))
# "annual temperature anomaly"

years <- 2011:2100
flsCMI <- file.path(outPath, paste0("CMI_NWT_", years, ".grd"))
if (!all(file.exists(flsCMI))){
  CMI_NWT <- stack(lapply(1:nlayers(CMI), function(index){
    message(paste0("Processing CMI for layer ", years[index], " (", index,"of 90): ", 
                   100*(round(index/90, 3)),"% done..."))
    proc <- postProcess(x = CMI[[index]], 
                        studyArea = studyArea,
                        rasterToMatch = rasterToMatch,
                        destinationPath = outPath,
                        filename2 = paste0("CMI_NWT_", years[index]),
                        userTags = c("CMIprocessing", paste0("layer", index)))
    return(proc)
  }))
} else {
  CMI_NWT <- stack(lapply(flsCMI, raster::raster))
}
names(CMI_NWT) <- paste0("CMI", years)

flsATA <- file.path(outPath, paste0("ATA_NWT_", years, ".grd"))
if (!all(file.exists(flsATA))){
  ATA_NWT <- stack(lapply(1:nlayers(ATA), function(index){
    message(paste0("Processing ATA for layer ", years[index], " (", index,"of 90): ", 
                   100*(round(index/90, 3)),"% done..."))
    proc <- postProcess(x = ATA[[index]], 
                        studyArea = studyArea,
                        rasterToMatch = rasterToMatch,
                        destinationPath = outPath,
                        filename2 = paste0("ATA_NWT_", years[index]),
                        userTags = c("ATAprocessing", paste0("layer", index)))
    return(proc)
  }))
} else {
  ATA_NWT <- stack(lapply(flsATA, raster::raster))
}
names(ATA_NWT) <- paste0("ATA", years)

pixelID <- data.table(pixelID = 1:ncell(flammableRTM),
                      val = getValues(flammableRTM))

DT_CMI <- data.table(getValues(CMI_NWT))
DT_CMI <- na.omit(cbind(pixelID, DT_CMI))
DT_CMI[, val := NULL]

DT_ATA <- data.table(getValues(ATA_NWT))
DT_ATA <- na.omit(cbind(pixelID, DT_ATA))
DT_ATA[, val := NULL]

# Calculate the trend in ATA and CMI
lmForPix <- function(x){
  DT <- data.table(YYYY = 1:length(x),
                   variable = x)
  model <- lm(variable ~ YYYY, data = DT)
  return(round(as.numeric(coef(model)[2]), 2))
}

library("tictoc")

tic("Total time elapsed lm:")
DT_CMI[, trendCMI := apply(.SD, 1, lmForPix), .SDcols = names(CMI_NWT)]
toc()

tic("Total time elapsed lm:")
DT_ATA[, trendATA := apply(.SD, 1, lmForPix), .SDcols = names(ATA_NWT)]
toc()
colsToKeep <- c("pixelID", "trendCMI")
DT_CMI_Reduced <- DT_CMI[, ..colsToKeep]
colsToKeep <- c("pixelID", "trendATA")
DT_ATA_Reduced <- DT_ATA[, ..colsToKeep]

pixelID[, val := NULL]

DT_CMI_Final <- merge(DT_CMI_Reduced, pixelID, all.y = TRUE)
DT_ATA_Final <- merge(DT_ATA_Reduced, pixelID, all.y = TRUE)

CMI_trend <- setValues(x = raster(CMI_NWT), values = DT_CMI_Final$trendCMI)
ATA_trend <- setValues(x = raster(ATA_NWT), values = DT_ATA_Final$trendATA)

# removeCorner
noCorner <- raster::raster("~/projects/NWT/inputs/NWT_BCR6/RTM_noCorner.tif")
ATA_trend[is.na(noCorner)] <- NA
CMI_trend[is.na(noCorner)] <- NA

ATAsub <- "Annual Temperature Anomaly"
CMIsub <- "Climate Moisture Index"
nColors <- 100

plts <- lapply(c("CMI", "ATA"), function(climateIndex){ 
  climateIndexPath <- file.path(outPath, paste0("trend_2011_2100_", climateIndex, ".png"))
  minVal <- raster::minValue(get(paste0(climateIndex, "_trend"))) 
  maxVal <- raster::maxValue(get(paste0(climateIndex, "_trend")))
  if (minVal < 0){
    totalRange <- maxVal-minVal
    i <-  2
    interv <- round(totalRange/nColors, i)
    while (interv == 0){
      interv <- round(totalRange/nColors, i)
      i <- i + 1
    }
    nColsNeg <- round(abs(minVal)/interv, 0)
    nColsPos <- round(abs(maxVal)/interv, 0)
    
    neg <- colorRampPalette(colors = c("darkred", "lightgoldenrod2"))(nColsNeg)
    pos <- colorRampPalette(colors = c("lightgoldenrod2", "darkblue"))(nColsPos)
    pal <- c(neg, pos)
  } else {
    pal <- viridis::viridis_pal(option = "D")(nColors)
    # pal <- pals::kovesi.rainbow(nColors)
  }

  # plot(NULL, xlim=c(0,length(pal)), ylim=c(0,1),
  #      xlab="", ylab="", xaxt="n", yaxt="n")
  # rect(0:(length(pal)-1), 0, 1:length(pal), 1, col=pal)

    png(filename = climateIndexPath,
        width = 21, height = 29,
        units = "cm", res = 300)
    print(levelplot(get(paste0(climateIndex, "_trend")),
                    sub = paste0("Slope of trend in ", 
                                 get(paste0(climateIndex, "sub")),
                                 " from 2011 to 2100 "),
                    margin = FALSE,
                    maxpixels = 6e6,
                    colorkey = list(
                      space = 'right',
                      axis.line = list(col = 'black'),
                      width = 0.75
                    ),
                    par.settings = list(
                      strip.border = list(col = 'transparent'),
                      strip.background = list(col = 'transparent'),
                      axis.line = list(col = 'transparent')),
                    scales = list(draw = FALSE),
                    col.regions = pal,
                    par.strip.text = list(cex = 0.8,
                                          lines = 1,
                                          col = "black")))
    dev.off()
})

# # TEST # ==> In 4.5 hours I will have the answer
# DT_T10 <- DT_CMI[1:10,]
# DT_T50 <- DT_CMI[1:50,]
# DT_T100 <- DT_CMI[1:100,]
# DT_T500 <- DT_CMI[1:500,]
# DT_T1000 <- DT_CMI[1:1000,]
# DT_T5000 <- DT_CMI[1:5000,]
# 
# TIMES <- microbenchmark::microbenchmark(
#   DT_T10[, trendCMI := apply(.SD, 1, lmForPix), .SDcols = names(CMI_NWT)],
#   DT_T50[, trendCMI := apply(.SD, 1, lmForPix), .SDcols = names(CMI_NWT)],
#   DT_T100[, trendCMI := apply(.SD, 1, lmForPix), .SDcols = names(CMI_NWT)],
#   DT_T500[, trendCMI := apply(.SD, 1, lmForPix), .SDcols = names(CMI_NWT)],
#   DT_T1000[, trendCMI := apply(.SD, 1, lmForPix), .SDcols = names(CMI_NWT)],
#   DT_T5000[, trendCMI := apply(.SD, 1, lmForPix), .SDcols = names(CMI_NWT)], times = 2)
# 
# TIMESdt <- summary(TIMES) 
# plotDT <- data.table(subsetSize = c(10, 50, 100, 500, 1000, 5000), Time = TIMESdt$mean)
# lmSubset <- lm(formula = Time ~ subsetSize, data = plotDT)
# toPredict <- data.table(subsetSize = NROW(DT_CMI))
# totalTime <- predict(object = lmSubset, newdata = toPredict)
# totalTimeInHours <- totalTime/3.6e+6
