setTempFolder <- function(paths, setTmpFolder, usr){
  # Set a storage project folder
  require("reproducible")
  workDirectory <- getwd()
  message("Your current temporary directory is ", tempdir())
  
  # Make a temporary folder for downloading files
  answer <- "NO"
  if (setTmpFolder == TRUE){
    if (usr %in% c("tmichele", "Tati")) {
      answer <- "YES"
    } else {
      answer <- readline(paste0("[ ATTENTION: ] \nYou are changing you temporary folder to \n",
                                file.path(paths$cachePath, "tmp"), "\nAre you sure you ",
                                "want to continue? [YES/NO/CANCEL]"))
    }
    if (answer == "YES") {
      unlink(file.path(paths$cachePath, "tmp"), recursive = TRUE, force = TRUE)
      tempFolder <- asPath(reproducible::checkPath(file.path(paths$cachePath, "tmp"), create = TRUE))
      
      # Set a temporary folder
      if (Sys.info()['sysname'] == "Windows"){
        write(paste0("TMPDIR = '", tempFolder, "'"), file = file.path(Sys.getenv('R_USER'), '.Renviron'))
      } else {
        tryCatch(library(unixtools),
                 error = function(e) install.packages("unixtools", repos = 'http://www.rforge.net/'))
        unixtools::set.tempdir(tempFolder)
      }
    } else {
      message("Temporary folder setting was CANCELED by the user")
    }
  } 
  if (setTmpFolder == TRUE & answer == "YES")
  message(paste0("You changed your temporary folder location,",
                    "\n If you are using Windows, please restart",
                 " your R session so the changes can take effect."))
}