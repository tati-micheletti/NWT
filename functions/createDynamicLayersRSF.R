#' createDynamicLayerRSF creates the necessary layers for predictive caribou RSF module. 
#' It is a function designed to work inside a SpaDES module.
#'
#' @param ageMap RasterLayer. Map with forest age.
#' @param biomassMap RasterLayer. Map with forest biomass.
#' @param biomassMapName character. Name of the forest biomass layer.
#' @param roadDensity Anthropogenic disturbance (raster) layer. Currently, road density layer used for both RSF and demographic models.
#' @param roadDensityName character. Name of the road Density layer in the model.
#' @param waterRaster Raster layer indicating water bodies.
#' @param oldBurnName character. Name of the old burn layer in the model.
#' @param newBurnName character. Name of the old burn layer in the model.
#' @param waterRasterName character. Name of the water layer in the model.
#' @param oldBurnTime numeric. Definition of the initial interval considered to be old burn. 
#'                    The end of this time is 20 years later (i.e. 40-60 years).
#' @param RTM RasterLayer template for these layers to match.
#' 
#' @return RasterStack of layers
#'
#' @author Tati Micheletti
#' @export
#' @importFrom raster raster projectRaster extract dropLayer stack nlayers extent
#' @importFrom SpaDES.tools rasterizeReduced
#' @importFrom data.table data.table setkey
#' @importFrom reproducible prepInputs postProcess Require
#' @include burnFromAge
#' @include nameAndBringOn
#' 
#' @rdname createDynamicLayersRSF

createDynamicLayersRSF <- function(ageMap,
                                   biomassMap,
                                   biomassMapName,
                                   oldBurnTime,
                                   oldBurnName,
                                   newBurnName,
                                   roadDensity,
                                   roadDensityName,
                                   waterRaster,
                                   waterRasterName,
                                   RTM){

  biomassMap <- nameAndBringOn(ras = biomassMap, name = biomassMapName, RTM = RTM)
  roadDensity <- nameAndBringOn(ras = roadDensity, name = roadDensityName, RTM = RTM)
  waterRaster <- nameAndBringOn(ras = waterRaster, name = waterRasterName, RTM = RTM)

  burnStk <- raster::stack(burnFromAge(ageMap = ageMap, oldBurnTime = oldBurnTime, 
                                       newBurnName = newBurnName, oldBurnName = oldBurnName))

  burnStk <- raster::stack(lapply(X = seq_len(raster::nlayers(burnStk)), FUN = function(nLay){
    ras <- nameAndBringOn(ras = burnStk[[nLay]], name = names(burnStk)[nLay], RTM = RTM)
    return(ras)
  }))

   # 2. Make sure all rasters are in the same extent
  tryCatch(expr = {
    
    dynamicStack <- raster::stack(burnStk, biomassMap, roadDensity, waterRaster)
    return(dynamicStack)
    
  }, error = function(e){
    message("One or more layers have a different extent and/or crs. Trying to fix with postProcess...")

    exts <- c(raster::extent(biomassMap), 
                   raster::extent(burnStk), 
                   raster::extent(roadDensity), 
                   raster::extent(waterRaster))
    names(exts) <- c("biomassMap", "burnStk", "roadDensity", "waterRaster")
    
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
    fineStacks <- setdiff(c("burnStk", "biomassMap", "roadDensity", "waterRaster"), whichNot)
    fineStacks <- raster::stack(lapply(X = fineStacks, FUN = function(r){
      ras <- get(r)
      return(ras)
    }))
    
    dynamicStack <- raster::stack(fineStacks, fixedLayers)
    return(dynamicStack)
    }
  )
}
