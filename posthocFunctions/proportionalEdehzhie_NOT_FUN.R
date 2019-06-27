# 9. Edehzhie inside/out proportionally: average RSF (2011, 2100) + average spRichness (2011, 2100)

caribouRSF_2011 <- raster::raster("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/relativeSelectionTaigaPlains_Year2011.tif")
caribouRSF_2100 <- raster::raster("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouRSF/relativeSelectionTaigaPlains_Year2100.tif")
birdRichness_2011 <- raster::raster("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/comMetrics/richnessRaster_2011.tif")
birdRichness_2100 <- raster::raster("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/comMetrics/richnessRaster_2100.tif")

birdRichness_2011 <- postProcess(birdRichness_2011, rasterToMatch = caribouRSF_2011, 
                                 filename2 = NULL, destinationPath = tempdir())
birdRichness_2100 <- postProcess(birdRichness_2100, rasterToMatch = caribouRSF_2011, 
                                 filename2 = NULL, destinationPath = tempdir())

rasList <- list(caribouRSF_2011, caribouRSF_2011, birdRichness_2011, birdRichness_2100)
names(rasList) <- c("caribouRSF_2011", "caribouRSF_2011", "birdRichness_2011", "birdRichness_2100")
url.sA <- "https://drive.google.com/open?id=15n9BOtswKCJ81-us1u8Dbs0WT9f8bagq"
studyArea <- Cache(prepInputs,
                   url = url.sA,
                   destinationPath = tempdir(),
                   userTags = "studyArea", filename2 = NULL,
                   omitArgs = c("destinationPath"))
studyArea <- postProcess(studyArea, rasterToMatch = caribouRSF_2011,
                         filename2 = NULL, destinationPath = tempdir())
studyAreaSF <- sf::st_as_sf(studyArea)
studyAreaSF$ID <- as.double(1:length(studyAreaSF$Name)) # ?
sAras <- fasterize::fasterize(sf = studyAreaSF, raster = caribouRSF_2011, field = "ID")

sAras <- postProcess(sAras, rasterToMatch = caribouRSF_2011, maskWithRTM = TRUE,
                     filename2 = NULL, destinationPath = tempdir())
sAras[is.na(sAras)] <- 2
sAras <- postProcess(sAras, rasterToMatch = caribouRSF_2011, maskWithRTM = TRUE,
                     filename2 = NULL, destinationPath = tempdir())

dt <- data.table(pixelID = 1:ncell(caribouRSF_2011),
                 RSF2011 = raster::getValues(x = caribouRSF_2011),
                 RSF2100 = raster::getValues(x = caribouRSF_2100),
                 Richness2011 = raster::getValues(x = birdRichness_2011),
                 Richness2100 = raster::getValues(x = birdRichness_2100),
                 studyArea = raster::getValues(x = sAras))

plotDT <- data.table(Reduce(merge, lapply(c("mean", "min", "max", "sd"), function(fun){
  tb <- DT[, lapply(.SD, get(fun), na.rm=TRUE), by = studyArea]
  tb[["operation"]] <- paste0("F",fun)
  tb[, pixelID := NULL]
  library("reshape2")
  tb2 <- melt(data = tb, id = c("studyArea", "operation"))
  tb2$year <- substrBoth(x = as.character(tb2[,variable]), n = 4)
  tb2$index <- substrBoth(x = as.character(tb2[,variable]), n = 4, fromEnd = FALSE)
  tb2[, variable := NULL]
  tb2$studyArea[tb2$studyArea == 2] <- "Ouside Edehzhie"
  tb2$studyArea[tb2$studyArea == 1] <- "Edehzhie"
  tb3 <- dcast(tb2, formula = studyArea + year + index ~ operation)
  names(tb3)[names(tb3) == "index"] <- "ind" 
  return(tb3)
})
))

library("ggplot2")
lapply(unique(plotDT$ind), function(grph){
  dt <- plotDT[ind == grph,]
  barPlots <- ggplot(data = dt, aes(x = as.factor(year), y = Fmean)) +
    geom_bar(stat = "identity", aes(fill = studyArea), position = "dodge") +
    labs(x = "Year", y = "Average value") +
    geom_errorbar(aes(ymax = Fmean + Fsd,
                      ymin = Fmean - Fsd), width=.2,
                  position = position_dodge(.9)) +
    theme_bw()
  png(filename = file.path("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/", paste0("comparison", grph, ".png")), height = 500, width = 600)
  print(barPlots)
  dev.off()
})

