# pull to memory
stackToMemory <- function (x, ...){
  r <- stack(x, ...)
  r <- setValues(r, getValues(r))
  return(r)
}

