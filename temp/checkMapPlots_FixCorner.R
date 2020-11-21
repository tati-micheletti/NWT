map <-  raster("/home/tmichele/projects/NWT/inputs/NWT_BCR6/CCSM4_85/CCSM4_RCP85_annual/CCSM4_RCP85_2011MSY/AHMYear2011.tif")
otherMap <- raster("/home/tmichele/projects/NWT/inputs/NWT_BCR6/CCSM4_85/CCSM4_RCP85_annual/CCSM4_RCP85_2011MSY/AHM.asc")
crs(otherMap) <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'

library(sp)
library(quickPlot)
library(raster)

Plot(map)
somePoints <- clickCoordinates(3) # pick 3 points with 3 mouse clicks
somePointsSP <- SpatialPoints(somePoints$coords, proj4string = crs(map))
vals1 <- raster::extract(map, somePointsSP)
somePointsSP_OM <- spTransform(somePointsSP, crs(otherMap))
vals2 <- raster::extract(otherMap, somePointsSP_OM)

# FIXING CORNER
rasterToMatch
noCorner <- rasterToMatch
noCorner[is.na(map)] <- NA
plot(noCorner)
test <- noCorner
test[is.na(noCorner) & rasterToMatch==1] <- 2
plot(test)
writeRaster(noCorner, "~/projects/NWT/inputs/NWT_BCR6/RTM_noCorner.tif", format = "GTiff")
