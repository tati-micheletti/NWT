pathsSetup <- function(whichComputer, isTest){

  # Set a storage project folder
  workDirectory <- getwd()
  message("Your current temporary directory is ", tempdir())
  
  if (isTest){
    outs <- checkPath(file.path(workDirectory, "outputs", paste0(toupper(format(Sys.time(), "%d%b%y")), "_TEST")), create = TRUE)    
  } else {
    outs <- checkPath(file.path(workDirectory, "outputs", toupper(format(Sys.time(), "%d%b%y"))), create = TRUE)
  }

  # Set up paths
  paths <- list(
    inputPath = file.path(workDirectory, "inputs"),
    outputPath = outs,
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