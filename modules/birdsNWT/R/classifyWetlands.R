classifyWetlands <- function(LCC = P(sim)$baseLayer,
                             wetLayerInput = sim$rasterDUCKS,
                             pathData = dataPath(sim),
                             studyArea = sim$studyArea){
  
  Require("LandR")
  Require("sf")
  Require("sp")
  Require("raster")

  # Load LCC layer
    rasLCC <- LandR::prepInputsLCC(year = LCC, destinationPath = pathData, 
                         studyArea = studyArea, filename2 = paste0("LCC", LCC), 
                         format = "GTiff")
    if (as.character(crs(rasLCC))!=as.character(crs(wetLayerInput))){
      rasLCC <- raster::projectRaster(from = rasLCC, crs = crs(wetLayerInput))      
    }

    # get xy of all pixels in DUCKS that are 1, 2 or 3+
    watIndex <- which(values(wetLayerInput)==1)
    wetIndex <- which(values(wetLayerInput)==2)
    upIndex <- which(values(wetLayerInput)>2)
    
    # extract the pixel numbers of these xy from LCC05.
    wetLocations <- xyFromCell(wetLayerInput, wetIndex)
    watLocations <- xyFromCell(wetLayerInput, watIndex)
    upLocations <- xyFromCell(wetLayerInput, upIndex)
    
    lcc05Wetlands <- as.data.table(raster::extract(rasLCC, wetLocations, cellnumbers = TRUE))
    lcc05Water <- as.data.table(raster::extract(rasLCC, watLocations, cellnumbers = TRUE))
    lcc05Uplands <- as.data.table(raster::extract(rasLCC, upLocations, cellnumbers = TRUE))
    
    # Calculate how many of times each pixel index exists
    countWet <- lcc05Wetlands[, .N, by = cells]
    countWat <- lcc05Water[, .N, by = cells]
    countUp <- lcc05Uplands[, .N, by = cells]

    # 50 or more pixels (50%) are 1, 2 or , that pixel index in LCC05 is actually a wetland
    lccWetIndex <- countWet[N > 49]
    lccWatIndex <- countWat[N > 49]
    lccUpIndex <- countUp[N > 49]
    wetVector <- lccWetIndex$cells 
    watVector <- lccWatIndex$cells 
    upVector <- lccUpIndex$cells 
    
    # Generate and return the mask layer
    lccWetLayer <- rasLCC
    lccWetLayer[!is.na(lccWetLayer)] <- -1
    lccWetLayer[watVector] <- 1
    lccWetLayer[wetVector] <- 2
    lccWetLayer[upVector] <- 3
    lccWetLayer[lccWetLayer == -1] <- NA

    storage.mode(lccWetLayer[]) <- "integer"

  return(lccWetLayer)
}