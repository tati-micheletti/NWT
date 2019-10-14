memoryThisPid <- function(){
  thisPid <- Sys.getpid()
  ps <- Sys.which("ps")
  if (nzchar(ps)) {
    aa <- try(system(paste("ps -eo comm,rss,pid --sort -rss | grep", thisPid), intern = TRUE), silent = TRUE)
    aa2 <- strsplit(aa, split = " +")[[1]]
    aa3 <- as.numeric(aa2[2]) * 1024
    class(aa3) <- "object_size"
  }
  return(aa3)
}

future::plan("multicore")
ongoingAvailableMemory <- function(pathToSave = getwd()){
  numTimes = 1
  while(numTimes < 100) {
    Sys.sleep(0.2)
    a <- memoryThisPid()
    cat(a, "\n", file = file.path(pathToSave, "memoryAvailable.txt"), append = TRUE)
    numTimes <- numTimes + 1
  }}