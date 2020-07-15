# Checking succession layers
library(raster)
library(colorspace)
successionLayers_year2011
y2011 <- successionLayers_year2011[[13:nlayers(successionLayers_year2011)]]
y2100 <- successionLayers_year2100[[13:nlayers(successionLayers_year2100)]]
propChange <- (y2100 - y2011)/y2011
names(propChange) <- names(y2011)
library(quickPlot)
nlayers(propChange)
png(file.path("/mnt/data/Micheletti/NWT/outputs/23OCT19/", "deltaClimateLayers1.png"))
plot(propChange[[1:9]], col = diverge_hcl(12, h = c(128, 330), c = 98, l = c(65, 90)))
dev.off()
clearPlot()
png(file.path("/mnt/data/Micheletti/NWT/outputs/23OCT19/", "deltaClimateLayers2.png"))
plot(propChange[[10:19]], col = diverge_hcl(12, h = c(128, 330), c = 98, l = c(65, 90)))
dev.off()
clearPlot()
png(file.path("/mnt/data/Micheletti/NWT/outputs/23OCT19/", "deltaClimateLayers3.png"))
plot(propChange[[19:24]], col = diverge_hcl(12, h = c(128, 330), c = 98, l = c(65, 90)))
dev.off()
fl <- usefulFuns::grepMulti(x = list.files("/mnt/data/Micheletti/NWT/outputs/23OCT19/", full.names = TRUE), patterns = c("deltaClimateLayers"))
lapply(fl, function(r){
  googledrive::drive_upload(r, googledrive::as_id("1jgGqwGcOpxSjUwxJw8_bee-iQtcHSRTG"))
})
