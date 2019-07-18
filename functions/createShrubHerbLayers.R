#' createShrubHerbLayers creates the necessary shrub or herb layers for predictive caribou RSF module. 
#' It is a function designed to work inside a SpaDES module.
#'
#' @param landCoverECCC RasterLayer LCC2005 reclassified to ECCC land cover.
#' @param reclassLCC05 List with reclassification for LCC05 values
#'                     (i.e. LCC05 classes that should be classified as shrub or herbs)
#' @param layerName character. Name of the shrub layer in the model.
#' @param includeCrops Logical. Should this layer include crops?
#' 
#' @return RasterLayer of herbs or shrubs
#'
#' @author Tati Micheletti
#' @export
#' @importFrom raster raster setValues
#' @importFrom data.table data.table setkey
#' @importFrom reproducible prepInputs postProcess Require
#' 
#' @rdname createShrubHerbLayers

createShrubHerbLayers <- function(landCoverECCC, reclassLCC05, layerName, includeCrops = FALSE){
  className <- unique(reclassLCC05$ECCC_Description[grepl(pattern = layerName, x = reclassLCC05$ECCC_Description)])
  if (!isTRUE(includeCrops))
    className <- className[!grepl(x = className, pattern = "cropland")]
  shrubHerbLayer <- raster(landCoverECCC)
  valsshrubHerbs <- raster::getValues(landCoverECCC)
  valshrubHerbFromTable <- unique(reclassLCC05[ECCC_Description == className, classesECCC])
  valsshrubHerbs[(valsshrubHerbs != valshrubHerbFromTable) & !is.na(valsshrubHerbs)] <- 0
  valsshrubHerbs[valsshrubHerbs == valshrubHerbFromTable] <- 1
  shrubHerbLayer <- raster::setValues(x = shrubHerbLayer, values = valsshrubHerbs)
  names(shrubHerbLayer) <- layerName
  return(shrubHerbLayer)
}