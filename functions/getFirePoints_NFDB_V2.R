#' Get Fire SpatialPoints from Canadian Fire Database
#'
#' @param url Passed to \code{prepInputs}
#' @param studyArea Passed to \code{prepInputs}
#' @param rasterToMatch Passed to \code{prepInputs}
#' @param redownloadIn Numeric Time in YEARS that we tolerate the data to be "old" i.e.
#'   0.5 would mean "redownload data older than 6 months"
#' @param years Numeric vector of consecutive years to fetch.
#' @param fireSizeColName Character describing the name of the column containing fire size information.
#' @param NFDB_pointPath Passed to \code{destinationPath} in \code{prepInputs}
#'
#' @return A \code{SpatialPointsDataFrame}.
#'
#' @export
#' @importFrom LandR asInteger
#' @importFrom raster crs crs<- res
#' @importFrom reproducible Cache Checksums prepInputs
#' @importFrom SpaDES.core dyear
#' @importFrom tools file_path_sans_ext
getFirePoints_NFDB_V2 <- function(url = NULL,
                               studyArea = NULL, 
                               rasterToMatch = NULL,
                               redownloadIn = 1,
                               years = 1991:2017,
                               fireSizeColName = "SIZE_HA",
                               NFDB_pointPath, # Can't be NULL. Needs to be an existing location for the fire points
                               ...) {
  if (is.null(url))
    url <- "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point.zip"
  
  check <- Checksums(NFDB_pointPath, checksumFile = file.path(NFDB_pointPath, "CHECKSUMS.txt"),
                     write = TRUE)
  whRowIsShp <- grep("NFDB_point.*shp$", check$expectedFile)
  whIsOK <- which(check$result[whRowIsShp] == "OK")
  needNewDownload <- TRUE
  if (any(whIsOK)) {
    filesToCheck <- tools::file_path_sans_ext(unlist(lapply(check[whRowIsShp[whIsOK],
                                                                  "expectedFile"], as.character)))
    dateOfFile <- substr(x = filesToCheck,
                         start = nchar(filesToCheck) - 8 + 1, nchar(filesToCheck))
    if ((as.Date(dateOfFile, format = "%Y%m%d") + dyear(redownloadIn)) > Sys.Date()) {
      # can change dyear(...) to whatever... e.g., dyear(0.5) would be 6 months
      needNewDownload <- FALSE
    }
  }
  if (needNewDownload) {
    print("downloading NFDB...")# put prepInputs here
    firePoints <- Cache(prepInputs, url = url,
                        fun = "shapefile",
                        destinationPath = NFDB_pointPath,
                        useSAcrs = TRUE, 
                        omitArgs = c("NFDB_pointPath", 
                                     "overwrite"))
    # Fix for messed up bbox
    message("Correcting original data problem...")
    DT <- as.data.frame(firePoints@data)
    coordinates(DT) <- cbind(firePoints$LONGITUDE, firePoints$LATITUDE)
    correctCRS <- "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"
    crs(DT) <- correctCRS
    firePointsReady <- projectInputs(DT,
                                     destinationPath = NFDB_pointPath,
                                     filename2 = "NFDBpointsProjected",
                                     targetCRS = crs(rasterToMatch))
    firePoints <- crop(firePointsReady, studyArea)
    message("Fire points corrected")
    raster::plot(firePoints, col = "red"); raster::plot(studyArea, add = TRUE)
  } else {
    print("NFDB present. Loading...")# put prepInputs here
    NFDBs <- grep(list.files(NFDB_pointPath), pattern = "^NFDB", value = TRUE)
    shps <- grep(list.files(NFDB_pointPath), pattern = ".shp$", value = TRUE)
    aFile <- NFDBs[NFDBs %in% shps][1] #in case there are multiple files
    firePoints <- raster::shapefile(file.path(NFDB_pointPath, aFile))
    # Fix for messed up bbox
    message(crayon::yellow("Correcting original data problem..."))
    DT <- as.data.frame(firePoints@data)
    coordinates(DT) <- cbind(firePoints$LONGITUDE, firePoints$LATITUDE)
    correctCRS <- "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"
    crs(DT) <- correctCRS
    firePointsReady <- projectInputs(DT,
                                     destinationPath = NFDB_pointPath,
                                     filename2 = "NFDBpointsProjected",
                                     targetCRS = crs(rasterToMatch))
    firePoints <- crop(firePointsReady, studyArea)
    message(crayon::green("Fire points corrected"))
    raster::plot(firePoints, col = "red"); raster::plot(studyArea, add = TRUE)
  }
  firePoints <- firePoints[firePoints$YEAR <= max(years) &
                             firePoints$YEAR >= min(years),]
  firePoints$fireSize <- asInteger(firePoints[[fireSizeColName]] / prod(res(rasterToMatch)) * 1e4)
  return(firePoints)
}
