makeCorrelationModel <- function(caribouRSFRas, birdDiversityRas, studyArea){

  library("reproducible")
  library("raster")
  library("data.table")
  library("sf")
  library("fasterize")
  
  names(birdDiversityRas) <- c("ShannonIndex", "SimpsonIndex", "RichnessIndex")
  caribouRSFRas <- postProcess(caribouRSFRas, rasterToMatch = birdDiversityRas[[1]], 
                               filename2 = NULL, destinationPath = tempdir())
if (!is(studyArea, "RasterLayer")) {
  studyArea <- postProcess(studyArea, rasterToMatch = caribouRSFRas,
                           filename2 = NULL, destinationPath = tempdir())
  
  studyAreaSF <- sf::st_as_sf(studyArea)
  studyAreaSF$ID <- as.double(1:length(studyAreaSF$NAME))

  sAras <- fasterize::fasterize(sf = studyAreaSF, raster = caribouRSFRas, field = "ID")
  
  sAras <- postProcess(sAras, rasterToMatch = caribouRSFRas, maskWithRTM = TRUE,
                           filename2 = NULL, destinationPath = tempdir())
}  else {
  sAras <- studyArea
}
  sAras <- postProcess(sAras, rasterToMatch = caribouRSFRas, maskWithRTM = TRUE,
                       filename2 = NULL, destinationPath = tempdir())
  
    dt <- data.table(pixelID = 1:ncell(caribouRSFRas),
                   RSF = raster::getValues(x = caribouRSFRas), 
                   richness = raster::getValues(x = birdDiversityRas[["RichnessIndex"]]),
                   studyArea = raster::getValues(x = sAras))
  
  dt$herd <- as.factor(dt$studyArea)
  dt$richnessIndex <- scale(dt$richness)
  browser()
  
  # CONVERT INTO A GLMER Herd should be a RE... No interaction either!
  mod <- lm(data = dt, formula = RSF ~ richnessIndex + herd + richnessIndex:herd)
  summary(mod)
  
  tbl <- data.table::data.table(herd = sort(unique(dt$herd)),
                                herdEstimate = c(0, mod$coefficients[3]), # [ FIX ] can't be hardcoded!! Redo it  
                                richnessIndex = mod$coefficients["richnessIndex"])
  tbl$slope <- tbl$herdEstimate + tbl$richnessIndex # DOUBLE CHECK THIS... SEEMS WEIRD...
  tbl[, c("herdEstimate", "richnessIndex") := NULL]
  dt <- merge(dt, tbl, all.x = TRUE)
  setkey(dt, pixelID)
  slopeRas <- raster::setValues(x = raster(caribouRSFRas), values = dt$slope)
  
  # + beta --> Umbrella species
  # - beta --> tradeoff

  return(slopeRas)
}