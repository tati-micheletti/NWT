# Shortcut to maps

yearsWanted <- c(2031, 2051, 2071)
whichComparison <- c("synergy30", "synergy70")

scenPerYear <- qs::qread(file.path(getwd(), 
                                   "outputs/landscapeRuns/LandR.CS_fS/hotspots/meanSolutionsAllYearsI_XIV.qs"))

Fig1_maps <- lapply(yearsWanted, function(Y){
  allComps <- lapply(whichComparison, function(whichComp){
    
comp <- switch(whichComp,
               "synergy70" = c("VI", "V"),
               "synergy30" = c("VII", "VIII"))

firstRas <- scenPerYear[[paste0("Year",Y)]][[paste0(comp[1],"_meanSolutions_Year",Y)]]
secondRas <- scenPerYear[[paste0("Year",Y)]][[paste0(comp[2],"_meanSolutions_Year",Y)]]
# Remove water
firstRas[waterRaster[] == 1] <- NA
secondRas[waterRaster[] == 1] <- NA

# 2. Get only areas that are selected more than 80% of the time
firstRas[firstRas[] >= 0.8] <- 1
firstRas[firstRas[] < 0.8] <- 0
secondRas[secondRas[] >= 0.8] <- 1
secondRas[secondRas[] < 0.8] <- 0

secondRas[secondRas[] == 1] <- 2
finalRas <- firstRas + secondRas
finalRas <- ratify(finalRas)

rat <- levels(finalRas)[[1]]
rat$species <- c("NotSelected", "landbirds", "caribou", "both")
levels(finalRas) <- rat
ColorsTB <- data.table(levs = c("NotSelected", "landbirds", "caribou", "both"),
                       Colors = c("grey85", "darkmagenta", "forestgreen", "blue4"))
ColorsTB <- ColorsTB[match(rat[["species"]], levs),]
message("Making Figure 1 map for year ", Y, " scenarios ", 
        paste(comp, collapse = " vs "))

fileNamePNG <- file.path(Paths$outputPath, "figuresForSam",
                         paste0(paste(comp, collapse = "_"),"_priorityAreas_Year", 
                                Y, ".tif"))

writeRaster(finalRas, filename = fileNamePNG, format = "GTiff")

# The "pretty" maps were made in ArcMap

return(plotPath = fileNamePNG)
})
  names(allComps) <- whichComparison
  return(allComps)
})
names(Fig1_maps) <- paste0("Year", yearsWanted)
Fig1_maps <- unlist(Fig1_maps)

Fig2_maps <- c(file.path(Paths$outputPath, "umbrellaLandbirdsPA.tif"),
               file.path(Paths$outputPath, "umbrellaCaribouPA.tif"),
               file.path(Paths$outputPath, "optimizedPA.tif"))

allMaps <- c(Fig1_maps, Fig2_maps)

folderID <- "1jUWhjkcLxGg3WKUB6l2xseSzzOLtjRZz"

Require("googledrive")

lapply(allMaps, drive_upload, path = as_id(folderID))
