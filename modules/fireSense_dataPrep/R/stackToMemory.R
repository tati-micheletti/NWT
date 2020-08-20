# pull to memory
stackToMemory <- function (x, ...){
  r <- raster::stack(x, ...)
  r <- raster::setValues(r, raster::getValues(r))
  return(r)
}

