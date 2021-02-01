# make a function to create a raster with the distances
# Calculate the distance for:
# 1. Major Roads
# 2. Polygonal disturbance
# 3. Settlement
# 4. Need still to calculate the linear feature density for the anthropogenic disturbance layer 
# I will still put together (that puts together both within NT1 data and outside of NT1/within 
# BCR6_NWT) --> This layer needs already to be created anyway. I need for distance to polygonal 
# disturbance

majorRoadsURL <- Cache(prepInputs, targetFile = "NRN_NT_10_0_ROADSEG.shp",
                       archive = "NRN_RRN_NT_10_0_SHAPE.zip",
                       alsoExtract = "similar",
                       url = "https://geo.statcan.gc.ca/nrn_rrn/nt/NRN_RRN_NT_10_0_SHAPE.zip",
                       # studyArea = studyArea, # Doesn't work currently in prepInputs
                       destinationPath = Paths$inputPath,
                       filename2 = "majorRoads_BCR6NWT",
                       rasterToMatch = rasterToMatch,
                       userTags = c(stepCacheTag,
                                    "outFun:Cache", "step:prepMajorRoads"))

majorRoadsURLcropped <- Cache(raster::crop, majorRoadsURL, studyArea,
                              userTags = c(stepCacheTag,
                                           "outFun:Cache", "step:prepMajorRoadsCrop"))

makeDistanceRaster <- function(ras, shp, overwrite = FALSE){
  if (as.character(crs(ras)) != as.character(crs(shp)))
    stop("CRS of ras and shp need to match")
  rasName <- file.path(Paths$inputPath, "majorRoadsDistanceRaster")
  if (any(!file.exists(rasName), overwrite)){
    require(raster)
    require(tictoc)
    pRas <- as(ras, "SpatialPoints")
    message("Creating distance raster...")
    tic("Time Elapsed:")
    # d <- rgeos::gDistance(spgeom1 = pRas, spgeom2 = shp, byid = TRUE) # Doesn't work. Crashes.
    # browser() # Alternative to gDistance
    # raster::distance(x = pRas, y = shp, 
    #                  filename = rasName, 
    #                  format = "GTiff", 
    #                  overwrite = TRUE)
    toc()
  }
  return(raster(rasName))
}

roadDistance <- makeDistanceRaster(ras = rasterToMatch, 
                      shp = majorRoadsURLcropped)#, 
                      # userTags = "majorRoadsDistanceRaster")
writeRaster(roadDistance, file.path(Paths$inputPath, "majorRoadsDistanceRaster"), format = "GTiff")

polygonalDisturbancesNT1 <- ""
polygonalDisturbancesNT1out <- "http://www.ec.gc.ca/data_donnees/STB-DGST/003/Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip" # EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS
# EC_borealdisturbance_linear_2008_2010_FINAL_ALBERS # Linear --> to make the linear feature density
