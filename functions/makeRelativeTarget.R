makeRelativeTarget <- function(targets){
  return((floor((targets/sum(targets)) * 1000) / 1000))
         # round(targets/sum(targets), 3))
}
