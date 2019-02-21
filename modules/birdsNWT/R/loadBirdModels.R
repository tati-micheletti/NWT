loadBirdModels <- function(birdsList = sim$birdsList,
                           folderUrl = extractURL("urlModels"),
                           cloudFolderID = sim$cloudFolderID,
                           pathData = dataPath(sim)){
  reproducible::Require("googledrive")
  filesToDownload <- cloudCache(googledrive::drive_ls, path = folderUrl, pattern = "brt1.R",
                                cloudFolderID = cloudFolderID, useCloud = TRUE,
                                omitArgs = c("destinationPath", "cloudFolderID"))
  modelsPath <- checkPath(file.path(pathData, "models"), create = TRUE)
  modelsForBirdList <- filesToDownload$name[grepl(pattern = paste(birdsList, collapse = "|"), x = filesToDownload$name)]
  downloadedModels <- lapply(X = modelsForBirdList, FUN = function(modelFile){
    if (!file.exists(file.path(modelsPath, modelFile))){
      cloudCache(googledrive::drive_download, file = paste0(folderUrl, modelFile),
                 path = file.path(modelsPath, modelFile), cloudFolderID = cloudFolderID,
                 omitArgs = c("destinationPath", "cloudFolderID"))
    }
    return(get(load(file.path(modelsPath, modelFile))))
  })
  names(downloadedModels) <- birdsList
  return(downloadedModels)
}
