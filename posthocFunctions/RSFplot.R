RSFplot <- function(ras, upload, writeReclasRas, outputFolder, rasName){
  library("raster")
  library("ggplot2")
  
  vals <- raster::getValues(ras)
  getBin <- function(vals){
    (max(vals, na.rm = TRUE)-min(vals, na.rm = TRUE))/10
  }
  bin <- getBin(vals)
  mn <- min(vals, na.rm = TRUE)
  
  m <- matrix(c(mn-1, mn+bin, 1,
                mn+bin, mn+bin*2, 2,
                mn+bin*2, mn+bin*3, 3,
                mn+bin*3, mn+bin*4, 4,
                mn+bin*4, mn+bin*5, 5,
                mn+bin*5, mn+bin*6, 6,
                mn+bin*6, mn+bin*7, 7,
                mn+bin*7, mn+bin*8, 8,
                mn+bin*8, mn+bin*9, 9,
                mn+bin*9, mn+bin*10, 10), ncol = 3, byrow = TRUE)
  
  r <- reclassify(ras, m)
  
  greenRed<-colorRampPalette(c("darkgreen","yellow","red"))
  colsGR <- greenRed(10)
  
  library(rasterVis)
  
p <- gplot(r) +
     geom_raster(aes(fill = factor(value))) +
     scale_fill_manual(values = colsGR, aesthetics = "fill")
     coord_equal()
     
  print(p)
  if (writeReclasRas)
  writeRaster(r, filename = file.path(outputFolder, rasName), format = "GTiff")
  
  if(upload)
    googledrive::drive_upload(file.path(outputFolder, rasName),
                              path = googledrive::as_id("1lhhIr_865VZI05mhiQ91iN8MfDGn5PV7"))
       
  return(r)
}


  