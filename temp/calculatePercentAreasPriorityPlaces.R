folder <- "C:/Users/Tati/Google Drive/Postdoc PFC-UBC/NWT 2019/G&C_Contract/priorityPlacesPreliminaryResults"
library(raster)
library(reproducible)
# 1. Extract the percentage of 0 and 1 inside Edehzhie and Tuyeta
p_15_W_2011 <- raster(file.path(folder, "p_15_W_2011.tif"))
p_15_W_2100 <- raster(file.path(folder, "p_15_W_2100.tif"))
p_15_noW_2011 <- raster(file.path(folder, "p_15_noW_2011.tif"))
p_15_noW_2100 <- raster(file.path(folder, "p_15_noW_2100.tif"))
p_30_W_2011 <- raster(file.path(folder, "p_30_W_2011.tif"))
p_30_W_2100 <- raster(file.path(folder, "p_30_W_2100.tif"))
p_30_noW_2011 <- raster(file.path(folder, "p_30_noW_2011.tif"))
p_30_noW_2100 <- raster(file.path(folder, "p_30_noW_2100.tif"))
p_60_W_2011 <- raster(file.path(folder, "p_60_W_2011.tif"))
p_60_W_2100 <- raster(file.path(folder, "p_60_W_2100.tif"))
p_60_noW_2011 <- raster(file.path(folder, "p_60_noW_2011.tif"))
p_60_noW_2100 <- raster(file.path(folder, "p_60_noW_2100.tif"))

allObjs <- ls(pattern = "p_")

tuyeta <- shapefile("C:/Users/Tati/Google Drive/Postdoc PFC-UBC/NWT 2019/Data/Tuyeta_Bdy_SGB_Edited_23JUL2019.shp")
tuyeta <- postProcess(tuyeta, rasterToMatch = p_15_W_2011, filename2 = NULL)
edehzhie <- shapefile("C:/Users/Tati/Google Drive/Postdoc PFC-UBC/NWT 2019/Data/edehzhie_boundary.shp")
edehzhie <- postProcess(edehzhie, rasterToMatch = p_15_W_2011, filename2 = NULL)

perc <- rbindlist(lapply(allObjs, function(obj){
  ras <- get(obj)
  perc <- rbindlist(lapply(c("tuyeta", "edehzhie"), function(shp){
    shape <- get(shp)
    ext <- raster::extract(ras, shape)
    tb <- table(ext)
    perc <- round((tb[2]/length(ext[[1]]))*100, 2)
    weight <- ifelse(length(grepMulti(obj, patterns = "noW")) == 0, "weighted","noWeight")
    target <- usefun::substrBoth(usefun::substrBoth(strng = obj, 
                                                    howManyCharacters = 4, 
                                                    fromEnd = FALSE), 
                                 howManyCharacters = 2, fromEnd = TRUE)
    year <- usefun::substrBoth(strng = obj, 
                               howManyCharacters = 4, 
                               fromEnd = TRUE)
    dt <- data.table::data.table(Weight = weight, 
                                 Target = target, 
                                 Year = year, 
                                 Percentage = perc,
                                 area = shp)
    return(dt)
  }))
  return(perc)
}))
perc

write.csv(perc, file.path(folder, "percentagePriority.csv"))




perc2 <- rbindlist(lapply(allObjs, function(obj){
  ras <- get(obj)
  tb <- table(ras[])
  namesOnes <- setdiff(names(tb), "0")
  ones <- sum(tb[namesOnes])
  perc <- round(ones/ncell(ras)*100, 2)
  weight <- ifelse(length(grepMulti(obj, patterns = "noW")) == 0, "weighted","noWeight")
  target <- usefun::substrBoth(usefun::substrBoth(strng = obj, 
                                                    howManyCharacters = 4, 
                                                    fromEnd = FALSE), 
                                 howManyCharacters = 2, fromEnd = TRUE)
  year <- usefun::substrBoth(strng = obj, 
                               howManyCharacters = 4, 
                               fromEnd = TRUE)
  dt <- data.table::data.table(Weight = weight, 
                                 Target = target, 
                                 Year = year, 
                                 Percentage = perc)
    return(dt)
  }))
perc2
