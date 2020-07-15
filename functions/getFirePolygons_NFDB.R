getFirePolygons_NFDB <- function(studyArea = NULL, rasterToMatch = NULL,
                                 redownloadIn = 2, # Time in YEARS that we tolerate the data to be "old" i.e. 0.5 would mean "redownload data older than 6 months"
                                 NFDB_polyPath, # Can't be NULL. Needs to be an existing location for the fire points
                                 redoChecksums = FALSE){
  
  if (redoChecksums){
    check <- Checksums(NFDB_polyPath, checksumFile = file.path(NFDB_polyPath, "CHECKSUMS.txt"), write = TRUE) 
  } else {
    check <- Checksums(NFDB_polyPath, checksumFile = file.path(NFDB_polyPath, "CHECKSUMS.txt"), write = FALSE)
  }
    whRowIsShp <- grep("NFDB_poly.*shp$", check$expectedFile)
    whIsOK <- which(check$result[whRowIsShp] == "OK")
    needNewDownload <- TRUE
    if (any(whIsOK)) {
      filesToCheck <- tools::file_path_sans_ext(unlist(lapply(check[whRowIsShp[whIsOK], "expectedFile"], as.character)))
      dateOfFile <- substr(x = filesToCheck, start = nchar(filesToCheck) - 8 + 
                             1, nchar(filesToCheck))
      if (any((as.Date(dateOfFile, format = "%Y%m%d") + dyear(redownloadIn)) > Sys.Date())) {
        # can change dyear(...) to whatever... e.g., dyear(0.5) would be 6 months
        needNewDownload <- FALSE
      }
    }
    if (needNewDownload) {
      print("downloading NFDB")# put prepInputs here
      firePolys <- Cache(prepInputs, url = url,
                          studyArea = studyArea, fun = "shapefile",
                          destination = NFDB_polyPath, useCache = "overwrite",
                          useSAcrs = TRUE, omitArgs = c("NFDB_polyPath", "overwrite"))
    } else {
      NFDBs <- grep(list.files(NFDB_polyPath), pattern = "^NFDB", value = TRUE)
      shps <- grep(list.files(NFDB_polyPath), pattern = ".shp$", value = TRUE)
      aFile <- NFDBs[NFDBs %in% shps][1] #in case there are multiple files
      firePolys <- Cache(shapefile, file.path(NFDB_polyPath, aFile))
      firePolys <- Cache(postProcess, x = firePolys,
                          studyArea = studyArea, filename2 = NULL,
                          rasterToMatch = rasterToMatch,
                          userTags = c("cacheTags", "NFDB"))
    }
    return(firePolys)
  }
