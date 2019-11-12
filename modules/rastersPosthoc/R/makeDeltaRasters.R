makeDeltaRasters <- function(listOfRasters, 
                             relativeDelta = TRUE, 
                             years = c(2000, 2100), 
                             outputFolder, lightLoad = TRUE,
                             overwrite = FALSE,
                             upload = FALSE,
                             folderID = NULL){
  rastersOrganized <- future_lapply(X = names(listOfRasters), function(eachSimulation){
    groupFiles <- lapply(X = names(listOfRasters[[eachSimulation]]), FUN = function(eachGroup){
      currentGroupsRas <- listOfRasters[[eachSimulation]][[eachGroup]]
      firstRas <- currentGroupsRas[[1]]
      lastRas <- currentGroupsRas[[nlayers(currentGroupsRas)]]
      if (isTRUE(relativeDelta)){
        denom <- firstRas 
      } else {
        denom <- 1
      }
      rasName <- file.path(outputFolder, 
                           paste0(paste(eachSimulation, eachGroup, sep = "_"), "delta.tif"))
      if (all(file.exists(rasName), !isTRUE(overwrite))){
        message(crayon::green(paste0("Delta maps exist for ", eachGroup, " for ", eachSimulation, ". Returning")))   
      } else {
        message(crayon::yellow(paste0("Calculating delta maps for ", eachGroup, " for ", eachSimulation)))
        deltaRas <- (lastRas - firstRas)/denom
        writeRaster(x = deltaRas, filename = rasName, overwrite = TRUE, format = "GTiff")
        rm(deltaRas)
        gc()
      }
      if(upload){
        if (is.null(folderID)) stop("Please provide folderID when upload == TRUE")
        # Need to find which name of the folderID is contained in the `eachSimulation` as simulations can 
        # have bird model version, which is not identical to the folderID 
        # ie. eachSimulation = "LandR.CS_SCFM_V6" while names(folderID)[2] = "LandR.CS_SCFM"
        simulFolder <- names(folderID)[which(!is.na(pmatch(names(folderID), eachSimulation)))]
        googledrive::drive_upload(rasName, path = googledrive::as_id(folderID[[simulFolder]]))
      }
      if (lightLoad){
        return(rasName)
      } else{
        return(raster::raster(rasName))
      }
      })
    names(groupFiles) <- names(listOfRasters[[eachSimulation]])
    return(groupFiles)
    })

    names(rastersOrganized) <- names(listOfRasters)
    return(rastersOrganized)
}