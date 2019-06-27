makeTimeSeriesPlots <- function(path, 
                                rastersNamePattern,
                                scenario){
  message(crayon::green(paste0("Looking for rasters in ", path, "\nUsing the following pattern(s): ", 
                               paste(rastersNamePattern, sep = "\n"))))
  filesToLoad  <- grepMulti(x = list.files(path = path, full.names = TRUE), pattern = rastersNamePattern)
  message(crayon::green("Loading the following file(s):"))
  message(crayon::magenta(paste0(" "), paste0(filesToLoad, sep = "\n")))
  env <- environment()
  allRas <- lapply(1:length(filesToLoad), function(index){
    eachRas <- readRDS(filesToLoad[[index]])
    library("quickPlot")
    nm <- reproducible::basename2(x = filesToLoad[[index]])
    Name <- strsplit(x = nm, split = "_")[[1]][1]
    Year <- tools::file_path_sans_ext(strsplit(x = nm, split = "_")[[1]][2])
    Year <- strsplit(x = Year, split = "year")[[1]][2]
    names(eachRas) <- paste0(Name, Year)
    tbl <- table(eachRas[])
    burnedPix <- tbl[[2]]
    assign(x = paste0("burnedPix", Year), value = burnedPix, envir = env)
    burnedArea <- burnedPix/sum(tbl)
    assign(x = paste0("burnedArea", Year), value = burnedArea, envir = env)
    return(eachRas)
  })
  ras <- raster::stack(unlist(allRas))
  raster::plot(ras, col = c("grey70","red"))
  
  burnedPix <- grepMulti(x = ls(), patterns = "burnedPix")
  burnedPix <- data.table::rbindlist(lapply(X = burnedPix, FUN = function(y){
    pixVal <- get(y)
    dt <- data.table::data.table(burned = pixVal, 
                                 year = as.numeric(strsplit(x = y, split = "burnedPix")[[1]][2]),
                                 scenario = scenario)
    return(dt)
  }))

  burnedArea <- grepMulti(x = ls(), patterns = "burnedArea")
  burnedArea <- data.table::rbindlist(lapply(X = burnedArea, FUN = function(y){
    AreaVal <- get(y)
    dt <- data.table::data.table(burned = AreaVal, 
                                 year = as.numeric(strsplit(x = y, split = "burnedArea")[[1]][2]),
                                 scenario = scenario)
    return(dt)
  }))
  
  
  return(list(ras = ras, burnedPix = burnedPix, burnedArea = burnedArea))
}