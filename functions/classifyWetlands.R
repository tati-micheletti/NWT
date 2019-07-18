#' classifyWetlands classifies wetlands (really!) using the wetlands layer set as input and a either LCC05 or LCC2010
#'
#' @param LCC numeric. 2005 (250m resolution) or 2010 (30m resolution) landcover  rasters.
#'
#' @param wetLayerInput Which wetland should be used as input (raster with projection). 
#'                      It was originally designed to work with the DUCKS Unlimited Waterland 
#'                      layer (30m) but can work with any waterlayers that have the following code: 
#'                      possibleLakes == 0
#'                      water bodies == 1
#'                      wetlands == 2
#'                      uplands > 2
#'                      
#'  @param pathData Where the layers are stored and/or should be saved to
#'  
#'  @param studyArea If the layer should be cropped and masked after classification. Optional.
#'  
#'  @param rasterToMatch raster to match the new layer after classification to. Optional.                 
#'                      
#'                
#' @return As with \code{\link[archivist]{cache}}, returns the value of the
#' function call or the cached version (i.e., the result from a previous call
#' to this same cached function with identical arguments).
#'
#' @author Tati Micheletti
#' @export
#' @importFrom LandR prepInputsLCC 
#' @importFrom raster raster projectRaster extract
#' @importFrom data.table data.table 
#' @importFrom reproducible prepInputs postProcess Require
#' @rdname classifyWetlands

classifyWetlands <- function(LCC,
                             wetLayerInput,
                             pathData,
                             studyArea,
                             rasterToMatch){
  Require("LandR")
  Require("raster")
  
  # Load LCC layer
  rasLCC <- LandR::prepInputsLCC(year = LCC, destinationPath = pathData, 
                                 studyArea = studyArea, filename2 = paste0("LCC", LCC), 
                                 format = "GTiff", overwrite = TRUE)
  if (as.character(crs(rasLCC))!=as.character(crs(wetLayerInput))){
    rasLCC <- raster::projectRaster(from = rasLCC, crs = crs(wetLayerInput))      
  }
  
  # get xy of all pixels in DUCKS that are 1, 2 or 3+
  possibleLakes <- which(values(wetLayerInput)==0) ###
  watIndex <- which(values(wetLayerInput)==1)
  wetIndex <- which(values(wetLayerInput)==2)
  upIndex <- which(values(wetLayerInput)>2)
  
  # extract the pixel numbers of these xy from LCC05.
  lakes <- xyFromCell(wetLayerInput, possibleLakes) ###
  wetLocations <- xyFromCell(wetLayerInput, wetIndex)
  watLocations <- xyFromCell(wetLayerInput, watIndex)
  upLocations <- xyFromCell(wetLayerInput, upIndex)
  
  lcc05Lakes <- as.data.table(raster::extract(rasLCC, lakes, cellnumbers = TRUE)) ###
  lcc05Wetlands <- as.data.table(raster::extract(rasLCC, wetLocations, cellnumbers = TRUE))
  lcc05Water <- as.data.table(raster::extract(rasLCC, watLocations, cellnumbers = TRUE))
  lcc05Uplands <- as.data.table(raster::extract(rasLCC, upLocations, cellnumbers = TRUE))
  
  # Calculate how many of times each pixel index exists
  countLake <- lcc05Lakes[, .N, by = cells] ###
  countWet <- lcc05Wetlands[, .N, by = cells]
  countWat <- lcc05Water[, .N, by = cells]
  countUp <- lcc05Uplands[, .N, by = cells]
  
  # 50 or more pixels (50%) are 1, 2 or , that pixel index in LCC05 is actually a wetland
  lccLakeIndex <- countLake[N > 49]
  lccWetIndex <- countWet[N > 49]
  lccWatIndex <- countWat[N > 49]
  lccUpIndex <- countUp[N > 49]
  
  lakeVector <- lccLakeIndex$cells 
  wetVector <- lccWetIndex$cells 
  watVector <- lccWatIndex$cells 
  upVector <- lccUpIndex$cells 
  
  # Generate and return the mask layer
  lccWetLayer <- rasLCC
  lccWetLayer[!is.na(lccWetLayer)] <- -1
  
  # Lakes and water
  lccWetLayer[lakeVector] <- 1
  lccWetLayer[watVector] <- 1

  # Mask it with RTM
  prepRTM <- reproducible::postProcess(rasterToMatch, rasterToMatch = lccWetLayer, filename2 = NULL)
  lccWetLayer <- reproducible::postProcess(lccWetLayer, rasterToMatch = prepRTM, maskWithRTM = TRUE, filename2 = NULL)
  lccWetLayer[lccWetLayer == 0] <- NA
  
  # Do uplands and wetlands
  lccWetLayer[wetVector] <- 2
  lccWetLayer[upVector] <- 3
  lccWetLayer[lccWetLayer == -1] <- NA
  
  storage.mode(lccWetLayer[]) <- "integer"
  
  return(lccWetLayer)
}