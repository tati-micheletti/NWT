loadStaticLayers <- function(fileURL = extractURL("urlStaticLayers"),
                             pathData = dataPath(sim),
                             cloudFolderID = sim$cloudFolderID,
                             studyArea = sim$studyArea){
  require("raster")
  stkPre <- preProcess(url = fileURL, targetFile = "bcr6_2011rasters250.grd",
                    destinationPath = pathData)
  stk <- raster::stack(stkPre$targetFilePath)
  stkNames <- unlist(lapply(X = 1:length(stk@layers), FUN = function(layers){
    lay <- stk@layers[[layers]]@data@names
    return(lay)
  }))
  stk <- postProcess(stk, studyArea = studyArea)
  names(stk) <- stkNames
  fixedLayers <- stkNames[!grepl(pattern = "Species", x = stkNames)]
  staticLayers <- raster::subset(x = stk, subset = fixedLayers)
  return(staticLayers)
}