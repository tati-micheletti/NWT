loadStaticLayers <- function(folderUrl = extractURL("urlStaticLayers"),
                             pathData = dataPath(sim),
                             cloudFolderID = sim$cloudFolderID){

  browser()
  stk <- prepInputs(url = fileURL, targetFile = "bcr6_2011rasters250.grd",
                    destinationPath = pathData, archive = "bcr6_2011rasters250.zip",
                    cloudFolderID = cloudFolderID, fun = "raster::stack")
}