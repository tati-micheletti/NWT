loadStaticLayers <- function(folderUrl = extractURL("urlStaticLayers"),
                             pathData = dataPath(sim),
                             cloudFolderID = sim$cloudFolderID){
  stk <- cloudCache(prepInputs, url = folderUrl, destinationPath = pathData)
  browser()
}