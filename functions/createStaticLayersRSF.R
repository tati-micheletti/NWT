#' createDynamicLayerRSF creates the necessary layers for predictive caribou RSF module. 
#' It is a function designed to work inside a SpaDES module.
#'
#' @param elevation RasterLayer of elevation
#' @param vrug RasterLayer of ruggeness           
#' @param LCC RasterLayer of landcover classes 2005      
#' @param reclassLCC05 List with reclassification for LCC05 values
#'                     (i.e. LCC05 classes that should be classified as shrub or herbs)
#' @param RTM RasterLayer template for these layers to match.
#' @param shrubName character. Name of the shrub layer in the model.
#' @param herbName character. Name of the herb layer in the model.
#' @param vrugName character. Name of the vrug layer in the model.
#' @param elevationName character. Name of the elevation layer in the model.
#' @param dynamicLayers RasterStack to be stacked with the staticLayers 
#'                      created in the current function to be passed for the model prediction.
#' 
#' @return RasterStack of layers
#'
#' @author Tati Micheletti
#' @export
#' @importFrom raster raster projectRaster extract dropLayer stack nlayers extent reclassify
#' @importFrom SpaDES.tools rasterizeReduced
#' @importFrom data.table data.table setkey
#' @importFrom reproducible prepInputs postProcess Require
#' @include createShrubHerbLayers
#' @include nameAndBringOn
#' 
#' @rdname createDynamicLayersRSF

createStaticLayersRSF <- function(elevation,
                                  vrug,
                                  LCC,
                                  shrubName,
                                  herbName,
                                  elevationName,
                                  vrugName,
                                  reclassLCC05,
                                  dynamicLayers,
                                  RTM){
  
  message("Creating static layers. Cached objects are fine here...")
  elevation <- Cache(nameAndBringOn, ras = elevation, name = elevationName, RTM = RTM)
  vrug <- Cache(nameAndBringOn, ras = vrug, name = vrugName, RTM = RTM)
  
  # 1. Extract shrub and herb from LCC05: which classes are these? Don't forget naming
  landCoverECCC <- raster::reclassify(x =  LCC, rcl = matrix(data = c(reclassLCC05[["classesLCC05"]], 
                                                                      reclassLCC05[["classesECCC"]]), 
                                                             ncol = 2, byrow = FALSE))
  Herbs <- Cache(createShrubHerbLayers, reclassLCC05 = reclassLCC05, landCoverECCC = landCoverECCC, layerName = herbName)
  Shrubs <- Cache(createShrubHerbLayers, reclassLCC05 = reclassLCC05, landCoverECCC = landCoverECCC, layerName = shrubName)
  Dec <- dynamicLayers$Deciduous
  
  # Need to override the deciduous from LandR with LCC05
  Dec[Herbs[] == 1] <- 0
  Dec[Shrubs[] == 1] <- 0
  
  # Make sure all rasters are in the same extent
  tryCatch(expr = {
    staticStack <- raster::stack(elevation, vrug, Shrubs, Herbs, Dec)
    return(staticStack)
    
  }, error = function(e){
    message("One or more layers have a different extent and/or crs. Trying to fix with postProcess...")
    exts <- c(raster::extent(elevation), 
              raster::extent(vrug), 
              raster::extent(Shrubs), 
              raster::extent(Herbs))
    tbl <- outer(exts, exts, Vectorize(all.equal))
    whichNot <- unlist(lapply(X = seq_len(length(exts)), function(res){
      r <- if (isTRUE(tbl[, 1][[res]])) NULL else names(exts)[res]
      return(r)
    }))
    
    message(paste0("The following layers don't match the base Deciduous (biomassMap) and will be fixed: ", crayon::magenta(whichNot)))
    fixedLayers <- raster::stack(lapply(X = whichNot, FUN = function(badLay){
      fxL <- reproducible::postProcess(x = get(badLay), rasterToMatch = biomassMap, useCache = FALSE,
                                       destinationPath = tempdir(), filename2 = NULL)
      return(fxL)
    }
    ))
    fineStacks <- setdiff(c(elevationName, vrugName, shrubName, herbName), whichNot)
    fineStacks <- raster::stack(lapply(X = fineStacks, FUN = function(r){
      ras <- get(r)
      return(ras)
    }))
    
    staticStack <- raster::stack(fineStacks, fixedLayers)
    return(staticStack)
  }
  )
}