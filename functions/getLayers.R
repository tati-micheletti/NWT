#' getLayers gets the necessary layers for predictive modules such as caribouPopGrowth and caribouRSF. 
#' It is a function designed to work inside a SpaDES module.
#'
#' @param currentTime numeric. Current time being used (i.e. \code{time(sim)}).
#' @param cohortData data.table. Output from LandR_Biomass module.
#' @param pixelGroupMap raster to identify the cohortData.
#' @param startTime numeric. startTime of the simulation. Needed to verify and potentially adjust relative simulation times.
#' @param endTime numeric. endTime of the simulation. Needed to verify and potentially adjust relative simulation times.
#' @param recoveryTime numeric. Recovery time in years that the forest needs to support Caribou. Default = 40.
#' @param listSACaribou list of shapefiles with polygons for which we want to calculate lambda for the caribou demographic model.
#' @param anthropogenicLayer Anthropogenic disturbance (raster) layer. Currently, road density layer used for both RSF and demographic models.
#' @param waterRaster Raster layer indicating water bodies.
#' @param isRSF logical. Identify if it should get the layers for the RSF or demographic model.
#' @param decidousSp binary raster layer indicating if the dominant biomass in a pixel belongs to a deciduous species.
#' @param oldBurnTime numeric. Definition of the initial interval considered to be old burn. The end of this time is 20 years later (i.e. 40-60 years).
#' @param elevation RasterLayer of elevation
#' @param vrug RasterLayer of ruggeness           
#' @param rstLCC RasterLayer of landcover classes 2005      
#' @param reclassLCC05 List with reclassification for rstLCC values (i.e. rstLCC classes that should be classified as shrub or herbs)
#' @param rasterToMatch RasterLayer template for these layers to match.
#' 
#' @return 
#'
#' @author Tati Micheletti
#' @export
#' @importFrom LandR prepInputsLCC 
#' @importFrom raster raster projectRaster extract dropLayer stack
#' @importFrom SpaDES.tools rasterizeReduced
#' @importFrom data.table data.table setkey
#' @importFrom reproducible prepInputs postProcess Require
#' @include createDynamicLayersRSF
#' @include createStaticLayersRSF
#' 
#' @rdname getLayers

getLayers <- function(currentTime,
                           cohortData, # Has age info per pixel group
                           pixelGroupMap, #Map of pixel groups
                           startTime,
                           endTime,
                           recoveryTime = 40,
                           listSACaribou,
                           anthropogenicLayer,
                           waterRaster,
                           isRSF = FALSE,
                           decidousSp = NULL,
                      oldBurnTime = NULL,
                      elevation = NULL,
                      vrug = NULL,
                      rstLCC = NULL,
                      reclassLCC05 = NULL,
                      rasterToMatch = NULL){
  
  if (is.null(pixelGroupMap)){
    message(crayon::red(paste0("pixelGroupMap is NULL for year ", currentTime, ". Returning NA")))
    return(NA)
  }
  # In a posterior version, will need to make this flexible for the model covariates
  reproducible::Require("raster")
  reproducible::Require("magrittr")
  originalTime <- currentTime
  if (startTime > 1){
    relEndTime <- endTime - startTime
    currentTime <- originalTime - startTime
  }
  
  threadsDT <- getDTthreads()
  setDTthreads(1)
  on.exit({setDTthreads(threadsDT)}, add = TRUE)
  
  ageMap <- raster(pixelGroupMap)
  valsAge <- data.table(pixelID = 1:ncell(ageMap), pixelGroup = getValues(x = pixelGroupMap))
  newAgeVals <- valsAge[cohortData[, list(age = max(age)), by = "pixelGroup"], on = "pixelGroup"]
  ageMap[newAgeVals$pixelID] <- newAgeVals$age
  names(ageMap) <- "ageMap"
  
  if (!isRSF){
    listDistForEachShpForEachPoly <- lapply(X = names(listSACaribou), FUN = function(caribouShapefile){
      message("Calculating disturbance for ", caribouShapefile)
      listPolyDist <- extractDisturbanceFast(ageMap = ageMap,
                            caribouShapefile = listSACaribou[[caribouShapefile]],
                            recoveryTime = recoveryTime,
                            anthropogenicLayer = anthropogenicLayer,
                            waterRaster = waterRaster,
                            rasterToMatch = rasterToMatch)
    })
    names(listDistForEachShpForEachPoly) <- names(listSACaribou)
    return(listDistForEachShpForEachPoly)
  } else {
    # Determine which pixels are deciduous
    setkey(cohortData, B)
    cohortData[, domSp := speciesCode[.N], by = "pixelGroup"]
    cohortData[, deciduous := ifelse(domSp %in% decidousSp, 1, 0)] 
    
    # Create the deciduous map
    cohortDataRed <- cohortData[, c("pixelGroup", "deciduous"), with = FALSE]    
    setkey(cohortDataRed, pixelGroup)
    cohortDataRed <- unique(cohortDataRed,  by = "pixelGroup")
    biomassMap <- SpaDES.tools::rasterizeReduced(reduced = cohortDataRed, fullRaster = pixelGroupMap,
                                                 newRasterCols = "deciduous", mapcode = "pixelGroup")
    
    # ageMap = old and new burns
    # anthropogenicLayer = roadDensity
    # waterLayer = waterRaster
    # Deciduous = biomassMap
    
    dynamicLayers <- createDynamicLayersRSF(ageMap = ageMap,
                                            biomassMap = biomassMap,
                                            biomassMapName = "Deciduous",
                                            oldBurnTime = oldBurnTime,
                                            oldBurnName = "OldBurn",
                                            newBurnName = "RecentBurn",
                                            roadDensity = anthropogenicLayer,
                                            roadDensityName = "RoadDensity",
                                            waterRaster = waterRaster,
                                            waterRasterName = "Water",
                                            RTM = rasterToMatch)
    
    staticLayers <- createStaticLayersRSF(elevation = elevation,
                                          vrug = vrug,
                                          rstLCC = rstLCC,
                                          shrubName = "Shrub",
                                          herbName = "Herb",
                                          elevationName = "Elevation",
                                          vrugName = "Vrug",
                                          reclassLCC05 = reclassLCC05,
                                          dynamicLayers = dynamicLayers,
                                          RTM = rasterToMatch)
    
    # We need to override the LandR_Biomass pixels with deciduous trees that were originally classified as 
    # "herbaceous" by ECCC 
    # We also need to mask the decidous to ONLY FOREST PIXELS!! 
    # [UPDATE on 7th June] I will try not to mask it. 
    # LandR should be providing way better estimates of biomass for non-forest pixels. Therefore next line is commented out
    # staticLayers[["Deciduous"]] <- postProcess(x = staticLayers[["Deciduous"]], rasterToMatch = forestOnly, maskWithRTM = TRUE,
    #                                        destinationPath = tempdir(), useCache = FALSE, filename2 = NULL)
    
    # This forestOnly layer excludes water too. However, for the RSF models we need to put these back, 
    # so add back the pixels as 0 from staticLayer[["Water"]] == 1
    staticLayers[["Deciduous"]][dynamicLayers[["Water"]] == 1] <- 0
    
    dynamicLayers[["Deciduous"]] <- staticLayers[["Deciduous"]]
    staticLayers <- raster::dropLayer(staticLayers, i = which(names(staticLayers)=="Deciduous"))
    
    # Stack both dynamic and static layers for prediction
    covStack <- raster::stack(dynamicLayers, staticLayers)
    covStack <- list(covStack) # List of the year
    name <- paste0("Year", originalTime)
    names(covStack) <- name
  }
  return(covStack)
}
