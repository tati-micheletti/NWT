getCoreAreas <- function(DT, RTM){
  allFolders <- data.table(drive_ls(as_id("1PipfXMz0GAB7nwOq_CRIn_sF259MzUvw"))) 
  interest <- unlist(unique(DT[["whichScenario"]]))
  interested <- allFolders[name %in% interest, ]
  # Google folders
  # No time lag: "nolag" folder (e.g. SPEC_core2080combo.tif)
  # 30-yr lag: 30yrlag folder
  # 60-yr lag: 60yrlag folder
  # 90-yr lag: refugia folder (i.e., strict refugia)
  allFiles <- rbindlist(lapply(interest, function(i){
    dr <- interested[name == i, id]
    allFiles <- data.table(drive_ls(path = as_id(dr)))
    return(allFiles)
  }))
    DT[, URL := lapply(1:NROW(DT), function(rowIndex){
      if (whichScenario[[rowIndex]] == "nolag"){
        fileName <- paste0(DT$species[rowIndex], "_", "core2080combo.asc")
      } else 
        if (whichScenario[[rowIndex]] == "refugia") {
          fileName <- paste0(DT$species[rowIndex], "_", "ref2080combo.asc")
        } else {
          fileName <- paste0(DT$species[rowIndex], "_", "core2080combo_", whichScenario[[rowIndex]], ".asc")
        }
      return(as.character(allFiles[name == fileName, id]))
    })]
    DT[, URL := as.character(URL)]
    allSpecies <- lapply(DT[["species"]], function(sp){
      message(crayon::yellow(paste0("Processing raster for ", sp, 
                                    " (", which(DT$species == sp), 
                                    " of ", NROW(DT),")")))
      if (DT[species == sp, URL] != "character(0)"){
        spFileName <- file.path(Paths$outputPath, paste0(sp, "_", DT[species == sp, whichScenario], 
                                           ".tif"))
        if (!file.exists(spFileName)){
          ascFile <- prepInputs(url = paste0("https://drive.google.com/file/d/", 
                                             DT[species == sp, "URL"],
                                             "/view?usp=sharing"), 
                                destinationPath = Paths$outputPath, 
                                fun = "raster::raster")
          crs(ascFile) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
          postASC <- postProcess(x = ascFile, 
                                 studyArea = studyArea,
                                 rasterToMatch = RTM,
                                 filename2 = spFileName, 
                                 format = "GTiff"
          )
        } 
        return(spFileName)
      } else {
        message(crayon::red(paste0("Future core for ", sp, " for scenario ", 
                                   DT[species == sp, whichScenario], " not found.")))
        return(NA)
        }
    })
    DT[, futureCore := unlist(allSpecies)]
    # NOW I NEED TO GET THE CORE AREA
    coreFiles <- data.table(drive_ls(as_id("1P4QhZFLUAxGLy-kBDBncbGEwog3Gh8-p")))
    allSpeciesCore <- lapply(DT[["species"]], function(sp){
      message(crayon::yellow(paste0("Processing current core raster for ", sp, 
                                    " (", which(DT$species == sp), 
                                    " of ", NROW(DT),")")))
      fileName <- paste0(sp, "_", "corecurrcombo.asc")
      URL <- as.character(coreFiles[name == fileName, id])
      spFileName <- file.path(Paths$outputPath, paste0(sp, "_core.tif"))
        if (!file.exists(spFileName)){
          ascFile <- prepInputs(url = paste0("https://drive.google.com/file/d/", 
                                             URL,
                                             "/view?usp=sharing"), 
                                destinationPath = Paths$outputPath, 
                                fun = "raster::raster")
          crs(ascFile) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
          # core habitat for a given species, time period
          # and model was defined as the grid cells where the model predicted
          # density exceeded the mean baseline (1961–1990) -
          # predicted density for that species within the model-building
          # area
          # ==> These rasters are ALREADY overlapped. They are already the final deal, so I should 
          # postProcess
          postASC <- postProcess(x = ascFile, 
                                 studyArea = studyArea,
                                 rasterToMatch = RTM,
                                 filename2 = spFileName, 
                                 format = "GTiff"
          )
        }
        return(spFileName)
    })
    DT[, currentCore := unlist(allSpeciesCore)]
    print("Rasters ready! :) ")
}
