pathsSetup <- function(whichComputer){

  # Set a storage project folder
  workDirectory <- getwd()
  message("Your current temporary directory is ", tempdir())
  
  # Set up paths
  paths <- list(
    inputPath = file.path(workDirectory, "inputs"),
    outputPath = file.path(workDirectory, "outputs"),
    modulePath = file.path(workDirectory, "modules")
    )
    
  if (whichComputer == "388"){
    cachePath388 <- reproducible::checkPath(file.path("/mnt/storage", "NWT"), create = TRUE)
    paths$cachePath <- file.path(cachePath388, "cache")
  } else {
    paths$cachePath <- file.path(workDirectory, "cache")
  }

  message(paste0("Paths are set:"))
  message(paste(names(paths), "=", paths, collapse = "\n"))
  return(paths)
}