bringObjectTS <- function(path,
                          rastersNamePattern){
  message(crayon::green(paste0("Looking for files in ", path, "\nUsing the following pattern(s): ", 
                               paste(rastersNamePattern, sep = "\n"))))
  filesToLoad  <- grepMulti(x = list.files(path = path, full.names = TRUE), pattern = rastersNamePattern)
  if (length(filesToLoad) == 0)
    stop("No files in the folder that have this pattern. Did you pass the correct folder and/or patterns?")
  message(crayon::green("Loading the following file(s):"))
  message(crayon::magenta(paste0(" "), paste0(filesToLoad, sep = "\n")))
  allRas <- lapply(1:length(filesToLoad), function(index){
    if (grepl(x = filesToLoad[[index]], pattern = ".tif")) # temporary workaround. Should behave like preProcess's fun
    {
      eachRas <- raster::raster(filesToLoad[[index]])
    } else {
      if (grepl(x = filesToLoad[[index]], pattern = ".rds")) # temporary workaround. Should behave like preProcess's fun
      {
        eachRas <- readRDS(filesToLoad[[index]])
      } else {
        stop("The function can ony deal with .tif or .rds objects dor now")
      }
    }
    return(eachRas)
  })
  return(allRas)
}