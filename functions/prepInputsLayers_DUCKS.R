#' Prepare DUCKS layer
#' @description This function is intendend to prepare the DUCKS Unlimited Hybrid Wetland 
#'              v. 2.1 layer to be used for different purposes. The output is a RasterLayer 
#'              cropped and reprojected to the \code{studyArea}, as well as resampled to the 
#'              \code{rasterToMatch} if any of these are provided.
#'              
#' @param destinationPath Path where to save the downloaded file.
#' @param url The url from where the layer should be downloaded from. if \code{NULL}, the default is \code{}
#' @param studyArea Study area for which the layer should be cropped to
#' @param rasterToMatch If output rasters should
#'
#' @return RasterLayer
#'
#' @export
#' @importFrom reproducible asPath Cache prepInputs
#' @rdname prepInputsLayers_DUCKS
prepInputsLayers_DUCKS <- function(destinationPath,
                                   url = NULL, archive = NULL,
                                   targetFile = NULL,
                                   studyArea = NULL, rasterToMatch = NULL) {
  if (is.null(url))
    url <- "https://drive.google.com/open?id=1wNpBdLICWDJ-DGwDboPb9wVwRwtGm1go"
  if (is.null(targetFile))
    targetFile <- "HWL_BCR6.tif"
  if (is.null(archive))
    archive <- "HWL_BCR6.zip"
  
  message("  Loading DUCKS Unlimited Hybrid Wetland v. 2.1 layers...")
  DUCKSlayer <- Cache(prepInputs,
                      targetFile = targetFile,
                      archive = archive,
                      url = url,
                      alsoExtract = "similar",
                      destinationPath = destinationPath,
                      fun = "raster::raster",
                      studyArea = studyArea,
                      rasterToMatch = rasterToMatch,
                      datatype = "INT1U",
                      overwrite = TRUE,
                      userTags =  c("DUCKs", "Hybrid", "Wetland"))
  
  return(DUCKSlayer)
}