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
#' @rdname prepSpeciesLayers
prepSpeciesLayers_DUCKS <- function(destinationPath,
                                     url = NULL,
                                     studyArea, rasterToMatch) {
  if (is.null(url))
    url <- ""

  message("  Loading DUCKS Unlimited Hybrid Wetland v. 2.1 layers...")
  DUCKSlayer <- Cache(prepInputs,
                     targetFile = ,
                     archive = ,
                     url = url,
                     alsoExtract = NULL,
                     destinationPath = destinationPath,
                     fun = "rgdal::readOGR",
                     studyArea = studyArea,
                     rasterToMatch = rasterToMatch,
                     datatype = "INT1U",
                     overwrite = TRUE,
                     userTags =  c("DUCKs", "Hybrid", "Wetland"))
  
  return(DUCKSlayer)
}