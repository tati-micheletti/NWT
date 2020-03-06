getFirePoints_NFDB <- function(url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip", 
                               studyArea = NULL, rasterToMatch = NULL,
                               redownloadIn = 1, # Time in YEARS that we tolerate the data to be "old" i.e. 0.5 would mean "redownload data older than 6 months"
                               NFDB_pointPath # Can't be NULL. Needs to be an existing location for the fire points
                               ){
  
  check <- Checksums(NFDB_pointPath, checksumFile = file.path(NFDB_pointPath, "CHECKSUMS.txt"), write = TRUE)
  whRowIsShp <- grep("NFDB_point.*shp$", check$expectedFile)
  whIsOK <- which(check$result[whRowIsShp] == "OK")
  needNewDownload <- TRUE
  if (any(whIsOK)) {
    dateOfFile <- gsub("NFDB_point_|\\.shp", "", check[whRowIsShp[whIsOK], "expectedFile"])
    if ((as.Date(dateOfFile, format = "%Y%m%d") + dyear(redownloadIn)) > Sys.Date()) {
      # can change dyear(...) to whatever... e.g., dyear(0.5) would be 6 months
      needNewDownload <- FALSE
    }
  }
  if (needNewDownload) {
    print("downloading NFDB")# put prepInputs here
    firePoints <- Cache(prepInputs, url = url,
                            studyArea = studyArea, fun = "shapefile",
                            destination = NFDB_pointPath, useCache = "overwrite",
                            useSAcrs = TRUE, omitArgs = c("NFDB_pointPath", "overwrite"))
  } else {
    NFDBs <- grep(list.files(NFDB_pointPath), pattern = "^NFDB", value = TRUE)
    shps <- grep(list.files(NFDB_pointPath), pattern = ".shp$", value = TRUE)
    aFile <- NFDBs[NFDBs %in% shps][1] #in case there are multiple files
    firePoints <- Cache(shapefile, file.path(NFDB_pointPath, aFile))
    firePoints <- Cache(postProcess, x = firePoints,
                            studyArea = studyArea, filename2 = NULL,
                            rasterToMatch = rasterToMatch,
                            userTags = c("cacheTags", "NFDB"))
  }
  return(firePoints)
}