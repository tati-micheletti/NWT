caribouCall$predictedPresenceProbability$Year0

writeOGR(NWTbound, dsn = file.path(getwd(), "outputs/14MAY19"), layer = "NWT", driver = "ESRI Shapefile")
raster::writeRaster(x = caribouCall$predictedPresenceProbability$Year0$TaigaPlains$relativeSelection, filename = "caribouNWTnoCSYear0", format = "GTiff")
raster::writeRaster(x = caribouCall$predictedPresenceProbability$Year100$TaigaPlains$relativeSelection, filename = "caribouNWTnoCSYear100", format = "GTiff")

raster::writeRaster(x = fire0NWT, filename = "fireNWTYear0", format = "GTiff")
raster::writeRaster(x = fire100NWT, filename = "fireNWTYear100", format = "GTiff", overwrite = TRUE)

googledrive::drive_upload("fireNWTYear100.tif",
                          path = as_id("1irqKYE5QpuLBu3QpMFxiOFI6ufErWjBx"))

googledrive::drive_upload(file.path(getwd(), "outputs/birds_metrics_14MAY19.rds"),
                          path = as_id("1irqKYE5QpuLBu3QpMFxiOFI6ufErWjBx"))

googledrive::drive_upload(file.path(getwd(), "caribouNWTnoCSYear100.tif"),
                          path = as_id("1irqKYE5QpuLBu3QpMFxiOFI6ufErWjBx"))

canBound <- raster::getData('GADM', country='CAN', level=1)
NWTbound <- canBound[canBound$NAME_1 == "Northwest Territories",]
# NWTbound <- cropInputs(NWTbound, studyArea = studyArea)

googledrive::drive_upload(file.path("modules/birdsNWT/data/OSFLpredNWT_15MAY19.gif"),
                          path = googledrive::as_id("1irqKYE5QpuLBu3QpMFxiOFI6ufErWjBx"))

library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library(ggmap)

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

library("ggmap")
mapsKey <- "AIzaSyBEzfVauyBANtryiVes4ECcJywAd4_2_k0"
ggmap::register_google(key = mapsKey)

EdeURL <- "https://drive.google.com/open?id=15n9BOtswKCJ81-us1u8Dbs0WT9f8bagq"
studyAreaEde <- cloudCache(prepInputs,
                           url = EdeURL,
                           destinationPath = getPaths()$inputPath[[1]],
                           cloudFolderID = cloudFolderID,
                           omitArgs = c("destinationPath", "cloudFolderID"))

EdeReproj <- projectInputs(studyAreaEde, 
                           targetCRS = raster::crs(birdsNWT$birdPrediction$Year0$OSFL$predictedOSFLYear0))

sA <- cropInputs(birdsNWT$birdPrediction$Year0$OSFL$predictedOSFLYear0, studyArea = EdeReproj)

out <- birdsNWT$birdPrediction$Year0$OSFL$predictedOSFLYear0
out <- postProcess(x = out, studyArea = studyAreaEde)
ceiling_dec <- function(x, level = 1) round(x + 5*10^(-level-1), level)
mxVal <- ceiling_dec(max(raster::maxValue(out)), level = 2)
breaks <- quantile(x = out, probs = seq(from = 0, to = 1, by = 0.1), na.rm = TRUE) 
# %>%
# apply(MARGIN = 2, FUN = max)
cols <- RColorBrewer::brewer.pal(n = length(breaks), name = "Spectral")

# fixedStack <- lapply(X = 1:quickPlot::numLayers(out), FUN = function(ras){
  dt <- raster::as.data.frame(out,
                              xy = TRUE, na.rm = FALSE, 
                              long = FALSE)
  names(dt) <- c("x", "y", "value")
  dtable <- data.table::data.table(dt) 
  dtable[, group := cut(value, unique(breaks))]
  dt  <- as.data.frame(dtable)
  names(cols) <- unique(sort(dt$group))
  cols[is.na(cols)] <- "grey93"
  library(ggplot2)
  spPlot <- ggplot2::ggplot(data = dt, aes(x = x, y = y)) +
    geom_tile(aes(fill = group)) +
    scale_fill_manual(values = cols) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "grey93"),
          axis.title = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(title.hjust = 0.5, reverse = TRUE)) +
    ggtitle(label = paste0("Predicted ", "OSFL", 
                           " for year 2005"))
  pngPlotName <- file.path(getwd(), "outputs", paste0("Predicted", species[sp], "Year", 
                                                      strsplit(x = names(out[[ras]]),
                                                               split = "Year")[[1]][2],
                                                      "_", toupper(format(Sys.time(), "%d%b%y")),".png"))
 
  
  library(ggplot2)
  spPlot <- ggplot2::ggplot(data = dt, aes(x = x, y = y)) +
    geom_tile(aes(fill = group)) +
    ggmap(get_googlemap(center = c(lon = -121.771563, lat = 63.840037),
                        zoom = 2, scale = 2,
                        maptype ='terrain',
                        color = 'color'))+
    scale_fill_manual(
      values = cols) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "grey93"),
          axis.title = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(title.hjust = 0.5, reverse = TRUE)) +
    ggtitle(label = paste0("Predicted ", species[sp], 
                           " for year ", strsplit(x = names(out[[ras]]), 
                                                  split = "Year")[[1]][2]))
  
  
  orderedRasterList <- lapply(X = seq_len(length(birdsNWT_Ede$birdsList)-1), FUN = function(index){
    sbset <- unlist(lapply(birdsNWT_Ede$birdPrediction, `[[`, index), use.names = TRUE)
    return(sbset)
  })
  names(orderedRasterList) <- birdsNWT_Ede$birdsList[1:9]
  
  
  lapply(orderedRasterList$OSFL, function(each){
    googledrive::drive_upload(each@file@name,
                              path = as_id("1irqKYE5QpuLBu3QpMFxiOFI6ufErWjBx"))
  })
  lapply(orderedRasterList$RUBL, function(each){
    googledrive::drive_upload(each@file@name,
                              path = as_id("1irqKYE5QpuLBu3QpMFxiOFI6ufErWjBx"))
  })
  