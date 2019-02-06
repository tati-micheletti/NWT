loadStaticLayers <- function(folderUrl = extractURL("urlStaticLayers"),
                             pathData = dataPath(sim),
                             cloudFolderID = sim$cloudFolderID){

  browser()
  stk <- cloudCache(prepInputs, url = folderUrl, destinationPath = pathData,
                    cloudFolderID = cloudFolderID)
}