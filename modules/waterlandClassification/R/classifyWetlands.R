classifyWetlands <- function(LCC = P(sim)$baseLayer,
                             wetLayerInput = sim$rasterDUCKS,
                             pathData = dataPath(sim),
                             studyArea = sim$studyArea){
  
  Require("LandR")
  Require("sf")
  Require("sp")

  listLCC <- lapply(X = LCC, FUN = function(yearLCC){
  if (!any(yearLCC %in% c("LCC10", "LCC05"))) message(paste0("Currently, only LCC10 and LCC05 are available.\n",
                                                        "LCC05 will be returned unless LCC10 was specified."))
    year <- if (grepl(x = yearLCC, pattern = "10")) 2010 else 2005

    rasLCC <- LandR::prepInputsLCC(year = year, destinationPath = pathData, 
                         studyArea = studyArea, filename2 = yearLCC, format = "GTiff")
    if (as.character(crs(rasLCC))!=as.character(crs(wetLayerInput)))
      rasLCC <- raster::projectRaster(from = rasLCC, crs = crs(wetLayerInput))
      
    return(rasLCC)
  })
  names(listLCC) <- LCC
  
  # Generate random points and extract a table with the value of those points from LCC10, LCC05 and DUCKS
  notna <- which(!is.na(values(wetLayerInput)) & values(wetLayerInput)!=0)
  set.seed(1983)
  samp <- Cache(sample, x = notna, size = 10^6, replace = FALSE)
  wetLayerInputPoints <- wetLayerInput[samp]
  samplocs <- xyFromCell(wetLayerInput, samp)
  
  lcc05Points <- raster::extract(listLCC$LCC05, samplocs)
  lcc10Points <- raster::extract(listLCC$LCC10, samplocs)
  
  wetLCC <- as.data.table(cbind(samplocs, samp, wetLayerInputPoints, lcc05Points, lcc10Points))
  names(wetLCC) <- c('x', 'y', 'indexDUCKS', 'valueDUCKS', 'valueLCC05', 'valueLCC10')
  
  # To check where the points lay
  # samp_sf <- st_as_sf(as.data.frame(wetLCC), coords = c('x', 'y'), crs = as.character(crs(wetLayerInput)))
  # raster::plot(samp_sf, add = T, col = 'red')
  
  return(wetLCC)
}