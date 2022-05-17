binRSFtoDeMars2019 <- function(ras){
  message("Loading binning table...")
  binningTable <- Cache(prepInputs, 
                        targetFile = "AllYear_noMac_SelectionRatios_20210120.csv",
                        url = "https://drive.google.com/file/d/1KXNlCN9iBLcPBcEge469fU9Kvws2trAc",
                        destinationPath = Paths[["inputPath"]], 
                        fun = "data.table::fread",
                        userTags = c("object:binningTable"))
  reclassMatrix <- matrix(cbind(binningTable[["Min.Value"]],
                                binningTable[["Max.Value"]],
                                binningTable[["RSF.Bin"]]), 
                          ncol = 3)
  # Make sure that the max value is infinite, so it accommodates any bigger value
  # than before
  reclassMatrix[nrow(reclassMatrix), 2] <- Inf
  rasBinned <- raster::reclassify(x = ras, 
                                  rcl = reclassMatrix)
  return(rasBinned)
}