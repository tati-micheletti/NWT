getOverlappedCoreAreas <- function(DT, RTM){
  # Folder refugia: overlap between current and future core area (i.e., 30-90 yr time lag 
  # depending on the time period)
  allFiles <- data.table(drive_ls(as_id("1xAHntTNbPg4lNSWpuPP8xbkRFKkOaRwM")))
  allSpecies <- lapply(DT[["species"]], function(sp){
    message(crayon::yellow(paste0("Processing raster for ", sp, 
                                  " (", which(DT$species == sp), 
                                  " of ", NROW(DT),")")))
    scen <- if (DT[species == sp, "whichScenario"] == "30yrlag") "ref2020combo" else
      if (DT[species == sp, "whichScenario"] == "60yrlag") "ref2050combo" else 
        if (DT[species == sp, "whichScenario"] == "90yrlag") "ref2080combo" else NA
    if (is.na(scen)){
      message(crayon::red(paste0("Refugia core for ", sp, " for scenario ", 
                                 scen, " not found.")))
      return(NA)
    }
  filesSpecies <- paste0(sp, "_", scen, ".asc")
    fileID <- allFiles[name == filesSpecies, id]
      spFileName <- file.path(Paths$outputPath, paste0(sp, "_refugia", 
                                                       ".tif"))
      if (!file.exists(spFileName)){
        ascFile <- prepInputs(url = paste0("https://drive.google.com/file/d/", 
                                           fileID,
                                           "/view?usp=sharing"), 
                              destinationPath = Paths$outputPath, 
                              fun = "raster::raster")
        crs(ascFile) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
        postASC <- postProcess(x = ascFile, 
                               studyArea = studyArea,
                               rasterToMatch = flammableRTM,
                               filename2 = spFileName, 
                               format = "GTiff"
        )
      }
      return(spFileName)
  })
  DT[, refugiaCore := unlist(allSpecies)]
  print("Rasters ready! :) ")
}
