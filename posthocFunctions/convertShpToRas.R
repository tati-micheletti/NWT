convertShpToRas <- function(genericShp, 
                            rasterToMatch, 
                            destinationPath,
                            background = NA,
                            rasStk = FALSE,
                            shapefileName = "genericShp"){
  # If the polygons intercept and rasList = FALSE,
  # the function will return one rasterLayer and the
  # polygon with higher ID will be on top. 
  # If rasStk == TRUE, a stack of the rasters will
  # be returned, where each polygon is a layer 
  message(crayon::blue("Fasterizing shapefile..."))
  genericShp <- Cache(reproducible::postProcess, x = genericShp,
                            rasterToMatch = rasterToMatch,
                            destinationPath = destinationPath,
                            filename2 = "genericShp",
                            userTags = c("objectName:genericShp",
                                         "outFun:Cache"))
  genericShpSF <- sf::st_as_sf(genericShp)
  nm <- if (!is.null(genericShp$NAME)){
    "NAME"
  } else {
    if (!is.null(genericShp$Name)) {
      "Name"
    } else {
      if (!is.null(genericShp$HERD)){
        "HERD"
      } else {
        NULL
      }
    }
  }
  if (is.null(nm))
    stop(paste0("The shapefile ", shapefileName, " does not have a field named ",
                "'NAME' or 'Name'. Please add that to it and run the ",
                "function again"))
  
  genericShpSF$ID <- as.numeric(seq(1:length(genericShpSF[[nm]])))
  genericShpRas <- fasterize::fasterize(sf = genericShpSF,
                                              raster = rasterToMatch,
                                              field = "ID")
  
  # Remove the ID's that are not in the rasterized version of the sA (because they are too small)
  availableInRas <- na.omit(unique(genericShpRas[]))
  polsToRemove <- setdiff(genericShpSF[["ID"]], availableInRas)
  genericShpSF <- genericShpSF[!genericShpSF$ID %in% polsToRemove,]
  if (rasStk){
    genericRAS <- raster::stack(lapply(genericShpSF[["ID"]], function(polyID){
      # Subset the shp by the polyID
      genericShpSFSub <- genericShpSF[genericShpSF$ID == polyID,]
      ras <- fasterize::fasterize(sf = genericShpSFSub, 
                                  raster = rasterToMatch, 
                                  field = "ID",
                                  background = background)
      return(ras)
    }))
    names(genericRAS) <- genericShpSF[[nm]]
  } else {
    genericRAS <- stack(fasterize::fasterize(sf = genericShpSF, 
                                       raster = rasterToMatch, 
                                       field = "ID", 
                                       background = background))
    names(genericRAS) <- shapefileName
  }
  return(genericRAS)
}
