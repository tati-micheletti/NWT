pathsSetup <- function(whichComputer, setTmpFolder){

  # Set a storage project folder
  workDirectory <- getwd()
  
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
    paths <- list(
      inputPath = file.path(workDirectory, "cache")
      )
  }

  # Make a temporary folder for downloading files
  tempFolder <- reproducible::checkPath(file.path(dirname(paths$cachePath), "tmp"), create = TRUE)
  
  # Set a temporary folder
  if (Sys.info()['sysname'] == "Windows"){
    write(paste0("TMPDIR = '", tempFolder, "'"), file = file.path(Sys.getenv('R_USER'), '.Renviron'))
  } else {
    tryCatch(library(unixtools),
             error = function(e) install.packages("unixtools", repos = 'http://www.rforge.net/'))
    unixtools::set.tempdir(tempFolder)
  }
  
  return(paths)
}