retrieveRasters <- function(dataFolder,
                            years = c(2000, 2100), 
                            patternsToRetrieveRasters = NULL, 
                            patternsUsedForGrouping = NULL){
  
  if (is.null(patternsUsedForGrouping))
    patternsUsedForGrouping <- "group1"
  if (is.null(patternsToRetrieveRasters)){
    message("patternsToRetrieveRasters is NULL. All '.tif' files in each one of the the 
            dataFolders will be returned.")
    patternsToRetrieveRasters <- ".tif"
  }
  rastersOrganized <- lapply(X = names(dataFolder), function(eachFolder){
    allFiles <- grepMulti(x = list.files(path = dataFolder[[eachFolder]], full.names = TRUE), 
                          patterns = patternsToRetrieveRasters)
      groupFiles <- lapply(X = patternsUsedForGrouping, FUN = function(eachGroup){
        filesPath <- grepMulti(x = allFiles, patterns = c(eachGroup, paste(years, collapse = "|")))
        rastersTS <- raster::stack(lapply(X = years, FUN = function(eachTS){
          rasPath <- grepMulti(x = filesPath, patterns = eachTS)
          if (length(rasPath) == 0) 
            stop("At least one of the rasters doesn't seem to exist for the year sequence provided. Please check your data")
          ras <- raster::raster(rasPath)
          names(ras) <- paste(eachFolder, eachGroup, eachTS, sep = "_")
          return(ras)
        })
        )
        return(rastersTS)
      })
      names(groupFiles) <- patternsUsedForGrouping
      return(groupFiles)
  })
  names(rastersOrganized) <- names(dataFolder)
 return(rastersOrganized)
}