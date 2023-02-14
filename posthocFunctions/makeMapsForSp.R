makeMapsForSp <- function(Species, birdSolPath, booSolPath, 
                          Year, Scenario, outPath, overwriteMap = FALSE,
                          overwriteFig = FALSE, uploadTo = NULL,
                          loadPathOnly = TRUE, forceUpload = TRUE){
  Require("RColorBrewer")
  Require("raster")
  Require("lattice")
  Require("rasterVis")
  Require("googledrive")
  
  scen <- as.numeric(switch(as.character(Scenario),
                            "0.05" = "1",
                            "0.15" = "2",
                            "0.25" = "3",
                            "0.35" = "4",
                            "0.45" = "5",
                            "0.55" = "6",
                            "0.65" = "7",
                            "0.75" = "8",
                            "0.85" = "9",
                            "0.95" = "10"))
  
  fileNameMap <- file.path(outPath, paste0(Species, "_", 
                                           strsplit(as.character(Scenario), 
                                                    split = ".", fixed = TRUE)[[1]][2], 
                                           "_", Year, ".tif"))
  message(paste0("Searching for ", fileNameMap))
  booMapsPath <- file.path(outPath, paste0("caribouSolutionMaps_", Year, "_", scen,".tif"))
  birdMapsPath <- file.path(outPath, paste0(Species, "SolutionMaps_", Year, "_", scen, ".tif"))

  if (any(!file.exists(fileNameMap),
          overwriteMap,
          !file.exists(booMapsPath),
          !file.exists(birdMapsPath))){
  message(paste0(Species, " final map (or needed files) not found. Creating."))  
  ClimateScenarios <- c("CCSM4", "CanESM2", "INM-CM4")
  Runs <- paste0("run", 1:5)
  CS_R <- data.table(expand.grid(ClimateScenarios, Runs))
  CS_R[, CsR := paste0(Var1, "_", Var2)]
  # How to use all climate projections and runs
  # Color intensity: how many times the pixel has been selected by one, or both (value/2 to match the not selected)
  # Color: purple = birds, green = both; caribou = "blue"

  if (!file.exists(booMapsPath)){
    message("Caribou solutions raster for all replicates and scenarios not found. Creating.")
    booSolutions <- raster::stack(unlist(lapply(unique(CS_R[, CsR]), function(cs){
      booSolution <-  list.files(path = booSolPath, 
                                 full.names = TRUE, 
                                 recursive = TRUE,
                                 pattern = paste0("Solution_caribou_", cs,
                                                  "_", Year, ".tif"))
      booRas <- raster::stack(booSolution)
      booRas <- booRas[[scen]]
      return(booRas)
    })))
    boos <- calc(booSolutions, fun = sum,
                 filename = booMapsPath, overwrite = TRUE)
  } 
  if (!file.exists(birdMapsPath)){
    message(paste0(Species, " solutions raster for all replicates and scenarios not found. Creating."))

    birdSolutions <- raster::stack(unlist(lapply(unique(CS_R[, CsR]), function(cs){
      birdSolution <- list.files(path = birdSolPath, 
                                 pattern = paste0("Solution_", Species,"_", cs, 
                                                  "_", Year,".tif"), 
                                 full.names = TRUE, recursive = TRUE)
      birdRas <- raster::stack(birdSolution)
      birdRas <- birdRas[[scen]]
      return(birdRas)
    })))
    birds <- calc(birdSolutions, fun = sum, 
                  filename = birdMapsPath, overwrite = TRUE)
  } 
  
  booMaps <- raster::raster(booMapsPath)
  birdMaps <- raster::raster(birdMapsPath)
  finalMapDT <- na.omit(data.table(pixelID = 1:ncell(booMaps), 
                           boo = getValues(booMaps),
                           bird = getValues(birdMaps)))
  
  finalMapDT[bird != 0 & boo != 0, comb := round((boo+bird)/2, 0)]
  finalMapDT[is.na(comb), comb := 0]
  
  finalMapDT[, bird := bird+100]
  finalMapDT[bird == 100, bird := 0]
  finalMapDT[, comb := comb+1000]
  finalMapDT[comb == 1000, comb := 0]
  finalMapDT[, final := bird+boo] # Get individually boo and bird (the ones where both are selected will be overwritten)
  finalMapDT[comb > 0, final := comb] # Replace the ones where both boo and bird are selected
  finalMapDT[, c("boo", "bird", "comb") := NULL]
  finalMap <- merge(data.table(pixelID = 1:ncell(booMaps)),
                    finalMapDT, by = "pixelID", all.x = TRUE)
  finalMap <- setValues(raster(booMaps), values = finalMap[["final"]])

  # Convert from numeric to factor
  r <- ratify(finalMap)
  rat <- levels(r)[[1]]
  booVals <- na.omit(sort(unique(finalMap[finalMap < 100])))
  birdVals <- na.omit(sort(unique(finalMap[finalMap > 100 & finalMap < 200])))
  combVals <- na.omit(sort(unique(finalMap[finalMap > 200])))
  fst <- ifelse(min(rat) == 0, "Not Selected", NA)
  booV <- if (!is.null(booVals)) paste0("caribou", round((100*booVals[booVals != 0])/15, 0), "%") else NA
  birV <- if (!is.null(birdVals)) paste0("landbird ", round((100*(birdVals-100))/15, 0), "%") else NA
  comV <- if (!is.null(combVals)) paste0("overlap ", round(rescale(x = combVals[combVals > 200], to = c(0, 100), from = c(1003, 1015)), 0), "%") else NA
  rat$selection <- na.omit(c(fst,booV,birV,comV))
  levels(r) <- rat
  
  # Cleanup the attributes table
  A <- data.table(r@data@attributes[[1]])
  r@data@attributes[[1]] <- A[selection != "",]
  
  # Save the map
  message(paste0("Writing raster for: ", Species, " year ", Year, " scenario ", Scenario))
  writeRaster(x = r, filename = fileNameMap, format = "GTiff", overwrite = TRUE)
  }
  
  figName <- paste0(Species, "_", 
                    strsplit(as.character(Scenario), 
                             split = ".", fixed = TRUE)[[1]][2], 
                    "_", Year, ".png")
  fullNameFig <- file.path(outPath, figName)
  if (any(!file.exists(fullNameFig),
          overwriteFig)){
    print(paste0("Making MAP for: ", Species, " year ", Year, " scenario ", Scenario))
  # Load the map when exists and not override
    booMaps <- raster(booMapsPath)
    birdMaps <- raster(birdMapsPath)
    finalMap <- raster(fileNameMap)
    
    booVals <- na.omit(sort(unique(finalMap[finalMap > 0 & finalMap < 100])))
    birdVals <- na.omit(sort(unique(finalMap[finalMap > 100 & finalMap < 200])))
    combVals <- na.omit(sort(unique(finalMap[finalMap > 200])))
      
    # Add the colortable! (And fixing from the tifs)
    birdPal <- if (length(birdVals) > 0) colorRampPalette(brewer.pal(9, name = "Purples"))(length(birdVals)) else NA
    booPal <- if (length(booVals) > 0) colorRampPalette(brewer.pal(9, name = "Greens"))(length(booVals)) else NA
    combPal <- if (length(combVals) > 0) colorRampPalette(brewer.pal(9, name = "Reds"))(length(combVals)) else NA
    zeroVal <- if (0 %in% sort(unique(finalMap[]))) "lightgrey" else NA
    allPals <- na.omit(c(zeroVal, booPal, birdPal, combPal))
    
    perc <- round(100*(1:15/15), 0)
    category <- c("Not Selected", 
                  paste0("caribou ", perc, "%"),
                  paste0("landbirds ", perc, "%"),
                  paste0("overlap ", perc, "%"))
    fixedTable <- data.table(ID = c(0:15, 101:115, 1001:1015),
                             category = category)

    A <- data.table(finalMap@data@attributes[[1]])
    AT <- A[category != "", "ID"]
    AT2 <- merge(AT, fixedTable, by = "ID")
    if (length(allPals) != NROW(AT)) stop(paste0("Number of rows in attribute table (n = ", NROW(AT), 
                                                 ") does not match number of values from raster (n = ", 
                                                 length(allPals), ") for ", Species, " year ", Year, 
                                                 " scenario ", Scenario))
    finalMap@data@attributes[[1]] <- AT2
        png(filename = fullNameFig,
        width = 21, height = 29,
        units = "cm", res = 300)
    print(levelplot(finalMap,
                    sub = paste0("Area selected by ", Species, 
                                 " and caribou for ", Scenario*100, 
                                 "% of total area chosen for ", Year),
                    margin = FALSE,
                    maxpixels = 8e6,
                    at = AT$ID,
                    colorkey = list(
                      space = 'right',
                      axis.line = list(col = 'black'),
                      width = 0.75
                    ),
                    par.settings = list(
                      strip.border = list(col = 'transparent'),
                      strip.background = list(col = 'transparent'),
                      axis.line = list(col = 'transparent')),
                    scales = list(draw = FALSE),
                    col.regions = allPals,
                    par.strip.text = list(cex = 0.8,
                                          lines = 1,
                                          col = "black")
                    ))
    dev.off()
  }

if (all(!is.null(uploadTo),
        forceUpload)){
  allFls <- drive_ls(as_id(uploadTo))
  fileThere <- any(grepl(pattern = figName, x = allFls$name))
  if (any(!fileThere, forceUpload))
    lapply(c(fullNameFig, fileNameMap), drive_upload, as_id(uploadTo))
}
  if (loadPathOnly) return(fileNameMap) else return(raster(fileNameMap))
}
