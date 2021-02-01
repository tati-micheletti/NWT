checkRasterStackIsInMemory <- function(rasStack){
  allInMemory <- unlist(lapply(1:raster::nlayers(rasStack), 
                               function(ras){
                                 r <- rasStack[[ras]]
                                 inMem <- r@data@inmemory
                                 return(inMem)
                               }))
  return(allInMemory)
}